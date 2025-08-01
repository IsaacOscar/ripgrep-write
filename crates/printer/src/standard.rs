use std::{
    cell::{Cell, RefCell},
    cmp,
    io::{self, Write},
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};

use {
    bstr::ByteSlice,
    grep_matcher::{Match, Matcher},
    grep_searcher::{
        LineStep, Searcher, Sink, SinkContext, SinkContextKind, SinkFinish,
        SinkMatch,
    },
    termcolor::{ColorSpec, NoColor, WriteColor},
};

use crate::{
    color::ColorSpecs,
    hyperlink::{self, HyperlinkConfig},
    multi_writer::MultiWriter,
    stats::Stats,
    util::{
        find_iter_at_in_context, trim_ascii_prefix, trim_line_terminator,
        DecimalFormatter, PrinterPath, Replacer, Sunk,
    },
};

/// The configuration for the standard printer.
///
/// This is manipulated by the StandardBuilder and then referenced by the
/// actual implementation. Once a printer is build, the configuration is frozen
/// and cannot changed.
#[derive(Debug, Clone)]
struct Config {
    colors: ColorSpecs,
    hyperlink: HyperlinkConfig,
    stats: bool,
    heading: bool,
    path: bool,
    only_matching: bool,
    per_match: bool,
    per_match_one_line: bool,
    replacement: Arc<Option<Vec<u8>>>,
    max_columns: Option<u64>,
    max_columns_preview: bool,
    max_matches: Option<u64>,
    column: bool,
    byte_offset: bool,
    trim_ascii: bool,
    separator_search: Arc<Option<Vec<u8>>>,
    separator_context: Arc<Option<Vec<u8>>>,
    separator_field_match: Arc<Vec<u8>>,
    separator_field_context: Arc<Vec<u8>>,
    separator_path: Option<u8>,
    path_terminator: Option<u8>,
    ensure_eol: bool,
    output_directory: Option<PathBuf>,
    buffer_files: bool,
}

impl<'a> Default for Config {
    fn default() -> Config {
        Config {
            colors: ColorSpecs::default(),
            hyperlink: HyperlinkConfig::default(),
            stats: false,
            heading: false,
            path: true,
            only_matching: false,
            per_match: false,
            per_match_one_line: false,
            replacement: Arc::new(None),
            max_columns: None,
            max_columns_preview: false,
            max_matches: None,
            column: false,
            byte_offset: false,
            trim_ascii: false,
            separator_search: Arc::new(None),
            separator_context: Arc::new(Some(b"--".to_vec())),
            separator_field_match: Arc::new(b":".to_vec()),
            separator_field_context: Arc::new(b"-".to_vec()),
            separator_path: None,
            path_terminator: None,
            ensure_eol: true,
            output_directory: None,
            buffer_files: true,
        }
    }
}

/// A builder for the "standard" grep-like printer.
///
/// The builder permits configuring how the printer behaves. Configurable
/// behavior includes, but is not limited to, limiting the number of matches,
/// tweaking separators, executing pattern replacements, recording statistics
/// and setting colors.
///
/// Some configuration options, such as the display of line numbers or
/// contextual lines, are drawn directly from the
/// `grep_searcher::Searcher`'s configuration.
///
/// Once a `Standard` printer is built, its configuration cannot be changed.
#[derive(Clone, Debug)]
pub struct StandardBuilder {
    config: Config,
}

impl StandardBuilder {
    /// Return a new builder for configuring the standard printer.
    pub fn new() -> StandardBuilder {
        StandardBuilder { config: Config::default() }
    }

    /// Build a printer using any implementation of `termcolor::WriteColor`.
    ///
    /// The implementation of `WriteColor` used here controls whether colors
    /// are used or not when colors have been configured using the
    /// `color_specs` method.
    ///
    /// For maximum portability, callers should generally use either
    /// `termcolor::StandardStream` or `termcolor::BufferedStandardStream`
    /// where appropriate, which will automatically enable colors on Windows
    /// when possible.
    ///
    /// However, callers may also provide an arbitrary writer using the
    /// `termcolor::Ansi` or `termcolor::NoColor` wrappers, which always enable
    /// colors via ANSI escapes or always disable colors, respectively.
    ///
    /// As a convenience, callers may use `build_no_color` to automatically
    /// select the `termcolor::NoColor` wrapper to avoid needing to import
    /// from `termcolor` explicitly.
    pub fn build<W: WriteColor>(&self, wtr: W) -> Standard<W> {
        Standard {
            config: self.config.clone(),
            wtr: RefCell::new(MultiWriter::new_with_file_output(
                wtr,
                self.config.output_directory.clone(), // It's easier to clone than deal with lifetimes
                self.config.buffer_files,
            )),
            matches: vec![],
        }
    }

    /// Build a printer from any implementation of `io::Write` and never emit
    /// any colors, regardless of the user color specification settings.
    ///
    /// This is a convenience routine for
    /// `StandardBuilder::build(termcolor::NoColor::new(wtr))`.
    pub fn build_no_color<W: io::Write>(
        &self,
        wtr: W,
    ) -> Standard<NoColor<W>> {
        self.build(NoColor::new(wtr))
    }

    /// Set the user color specifications to use for coloring in this printer.
    ///
    /// A [`UserColorSpec`](crate::UserColorSpec) can be constructed from
    /// a string in accordance with the color specification format. See
    /// the `UserColorSpec` type documentation for more details on the
    /// format. A [`ColorSpecs`] can then be generated from zero or more
    /// `UserColorSpec`s.
    ///
    /// Regardless of the color specifications provided here, whether color
    /// is actually used or not is determined by the implementation of
    /// `WriteColor` provided to `build`. For example, if `termcolor::NoColor`
    /// is provided to `build`, then no color will ever be printed regardless
    /// of the color specifications provided here.
    ///
    /// This completely overrides any previous color specifications. This does
    /// not add to any previously provided color specifications on this
    /// builder.
    pub fn color_specs(&mut self, specs: ColorSpecs) -> &mut StandardBuilder {
        self.config.colors = specs;
        self
    }

    /// Set the configuration to use for hyperlinks output by this printer.
    ///
    /// Regardless of the hyperlink format provided here, whether hyperlinks
    /// are actually used or not is determined by the implementation of
    /// `WriteColor` provided to `build`. For example, if `termcolor::NoColor`
    /// is provided to `build`, then no hyperlinks will ever be printed
    /// regardless of the format provided here.
    ///
    /// This completely overrides any previous hyperlink format.
    ///
    /// The default configuration results in not emitting any hyperlinks.
    pub fn hyperlink(
        &mut self,
        config: HyperlinkConfig,
    ) -> &mut StandardBuilder {
        self.config.hyperlink = config;
        self
    }

    /// Enable the gathering of various aggregate statistics.
    ///
    /// When this is enabled (it's disabled by default), statistics will be
    /// gathered for all uses of `Standard` printer returned by `build`,
    /// including but not limited to, the total number of matches, the total
    /// number of bytes searched and the total number of bytes printed.
    ///
    /// Aggregate statistics can be accessed via the sink's
    /// [`StandardSink::stats`] method.
    ///
    /// When this is enabled, this printer may need to do extra work in order
    /// to compute certain statistics, which could cause the search to take
    /// longer.
    ///
    /// For a complete description of available statistics, see [`Stats`].
    pub fn stats(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.stats = yes;
        self
    }

    /// Enable the use of "headings" in the printer.
    ///
    /// When this is enabled, and if a file path has been given to the printer,
    /// then the file path will be printed once on its own line before showing
    /// any matches. If the heading is not the first thing emitted by the
    /// printer, then a line terminator is printed before the heading.
    ///
    /// By default, this option is disabled. When disabled, the printer will
    /// not show any heading and will instead print the file path (if one is
    /// given) on the same line as each matching (or context) line.
    pub fn heading(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.heading = yes;
        self
    }

    /// When enabled, if a path was given to the printer, then it is shown in
    /// the output (either as a heading or as a prefix to each matching line).
    /// When disabled, then no paths are ever included in the output even when
    /// a path is provided to the printer.
    ///
    /// This is enabled by default.
    pub fn path(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.path = yes;
        self
    }

    /// Only print the specific matches instead of the entire line containing
    /// each match. Each match is printed on its own line. When multi line
    /// search is enabled, then matches spanning multiple lines are printed
    /// such that only the matching portions of each line are shown.
    pub fn only_matching(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.only_matching = yes;
        self
    }

    /// Print at least one line for every match.
    ///
    /// This is similar to the `only_matching` option, except the entire line
    /// is printed for each match. This is typically useful in conjunction with
    /// the `column` option, which will show the starting column number for
    /// every match on every line.
    ///
    /// When multi-line mode is enabled, each match is printed, including every
    /// line in the match. As with single line matches, if a line contains
    /// multiple matches (even if only partially), then that line is printed
    /// once for each match it participates in, assuming it's the first line in
    /// that match. In multi-line mode, column numbers only indicate the start
    /// of a match. Subsequent lines in a multi-line match always have a column
    /// number of `1`.
    ///
    /// When a match contains multiple lines, enabling `per_match_one_line`
    /// will cause only the first line each in match to be printed.
    pub fn per_match(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.per_match = yes;
        self
    }

    /// Print at most one line per match when `per_match` is enabled.
    ///
    /// By default, every line in each match found is printed when `per_match`
    /// is enabled. However, this is sometimes undesirable, e.g., when you
    /// only ever want one line per match.
    ///
    /// This is only applicable when multi-line matching is enabled, since
    /// otherwise, matches are guaranteed to span one line.
    ///
    /// This is disabled by default.
    pub fn per_match_one_line(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.per_match_one_line = yes;
        self
    }

    /// Set the bytes that will be used to replace each occurrence of a match
    /// found.
    ///
    /// The replacement bytes given may include references to capturing groups,
    /// which may either be in index form (e.g., `$2`) or can reference named
    /// capturing groups if present in the original pattern (e.g., `$foo`).
    ///
    /// For documentation on the full format, please see the `Capture` trait's
    /// `interpolate` method in the
    /// [grep-printer](https://docs.rs/grep-printer) crate.
    pub fn replacement(
        &mut self,
        replacement: Option<Vec<u8>>,
    ) -> &mut StandardBuilder {
        self.config.replacement = Arc::new(replacement);
        self
    }

    /// Set the maximum number of columns allowed for each line printed. A
    /// single column is heuristically defined as a single byte.
    ///
    /// If a line is found which exceeds this maximum, then it is replaced
    /// with a message indicating that the line has been omitted.
    ///
    /// The default is to not specify a limit, in which each matching or
    /// contextual line is printed regardless of how long it is.
    pub fn max_columns(&mut self, limit: Option<u64>) -> &mut StandardBuilder {
        self.config.max_columns = limit;
        self
    }

    /// When enabled, if a line is found to be over the configured maximum
    /// column limit (measured in terms of bytes), then a preview of the long
    /// line will be printed instead.
    ///
    /// The preview will correspond to the first `N` *grapheme clusters* of
    /// the line, where `N` is the limit configured by `max_columns`.
    ///
    /// If no limit is set, then enabling this has no effect.
    ///
    /// This is disabled by default.
    pub fn max_columns_preview(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.max_columns_preview = yes;
        self
    }

    /// Set the maximum amount of matching lines that are printed.
    ///
    /// If multi line search is enabled and a match spans multiple lines, then
    /// that match is counted exactly once for the purposes of enforcing this
    /// limit, regardless of how many lines it spans.
    pub fn max_matches(&mut self, limit: Option<u64>) -> &mut StandardBuilder {
        self.config.max_matches = limit;
        self
    }

    /// Print the column number of the first match in a line.
    ///
    /// This option is convenient for use with `per_match` which will print a
    /// line for every match along with the starting offset for that match.
    ///
    /// Column numbers are computed in terms of bytes from the start of the
    /// line being printed.
    ///
    /// This is disabled by default.
    pub fn column(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.column = yes;
        self
    }

    /// Print the absolute byte offset of the beginning of each line printed.
    ///
    /// The absolute byte offset starts from the beginning of each search and
    /// is zero based.
    ///
    /// If the `only_matching` option is set, then this will print the absolute
    /// byte offset of the beginning of each match.
    pub fn byte_offset(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.byte_offset = yes;
        self
    }

    /// When enabled, all lines will have prefix ASCII whitespace trimmed
    /// before being written.
    ///
    /// This is disabled by default.
    pub fn trim_ascii(&mut self, yes: bool) -> &mut StandardBuilder {
        self.config.trim_ascii = yes;
        self
    }

    /// Set the separator used between sets of search results.
    ///
    /// When this is set, then it will be printed on its own line immediately
    /// before the results for a single search if and only if a previous search
    /// had already printed results. In effect, this permits showing a divider
    /// between sets of search results that does not appear at the beginning
    /// or end of all search results.
    ///
    /// To reproduce the classic grep format, this is typically set to `--`
    /// (the same as the context separator) if and only if contextual lines
    /// have been requested, but disabled otherwise.
    ///
    /// By default, this is disabled.
    pub fn separator_search(
        &mut self,
        sep: Option<Vec<u8>>,
    ) -> &mut StandardBuilder {
        self.config.separator_search = Arc::new(sep);
        self
    }

    /// Set the separator used between discontiguous runs of search context,
    /// but only when the searcher is configured to report contextual lines.
    ///
    /// The separator is always printed on its own line, even if it's empty.
    ///
    /// If no separator is set, then nothing is printed when a context break
    /// occurs.
    ///
    /// By default, this is set to `--`.
    pub fn separator_context(
        &mut self,
        sep: Option<Vec<u8>>,
    ) -> &mut StandardBuilder {
        self.config.separator_context = Arc::new(sep);
        self
    }

    /// Set the separator used between fields emitted for matching lines.
    ///
    /// For example, when the searcher has line numbers enabled, this printer
    /// will print the line number before each matching line. The bytes given
    /// here will be written after the line number but before the matching
    /// line.
    ///
    /// By default, this is set to `:`.
    pub fn separator_field_match(
        &mut self,
        sep: Vec<u8>,
    ) -> &mut StandardBuilder {
        self.config.separator_field_match = Arc::new(sep);
        self
    }

    /// Set the separator used between fields emitted for context lines.
    ///
    /// For example, when the searcher has line numbers enabled, this printer
    /// will print the line number before each context line. The bytes given
    /// here will be written after the line number but before the context
    /// line.
    ///
    /// By default, this is set to `-`.
    pub fn separator_field_context(
        &mut self,
        sep: Vec<u8>,
    ) -> &mut StandardBuilder {
        self.config.separator_field_context = Arc::new(sep);
        self
    }

    /// Set the path separator used when printing file paths.
    ///
    /// When a printer is configured with a file path, and when a match is
    /// found, that file path will be printed (either as a heading or as a
    /// prefix to each matching or contextual line, depending on other
    /// configuration settings). Typically, printing is done by emitting the
    /// file path as is. However, this setting provides the ability to use a
    /// different path separator from what the current environment has
    /// configured.
    ///
    /// A typical use for this option is to permit cygwin users on Windows to
    /// set the path separator to `/` instead of using the system default of
    /// `\`.
    pub fn separator_path(&mut self, sep: Option<u8>) -> &mut StandardBuilder {
        self.config.separator_path = sep;
        self
    }

    /// Set the path terminator used.
    ///
    /// The path terminator is a byte that is printed after every file path
    /// emitted by this printer.
    ///
    /// If no path terminator is set (the default), then paths are terminated
    /// by either new lines (for when `heading` is enabled) or the match or
    /// context field separators (e.g., `:` or `-`).
    pub fn path_terminator(
        &mut self,
        terminator: Option<u8>,
    ) -> &mut StandardBuilder {
        self.config.path_terminator = terminator;
        self
    }

    /// Whether to ensure that a newline is always printed, even if the
    /// printed match/context doesn't end with one. Defaults to true.
    pub fn ensure_eol(&mut self, ensure: bool) -> &mut StandardBuilder {
        self.config.ensure_eol = ensure;
        self
    }

    /// Which directory to store any per-file output to (if any)
    /// And whether to use a buffer when writing files to it
    pub fn output_directory(
        &mut self,
        directory: Option<&Path>,
        buffer: bool,
    ) -> &mut StandardBuilder {
        // Trying to store the &Path inside self.config makes life too hard
        // due to all the necessary lifetime paramaters, so I just make a copy
        self.config.output_directory = directory.map(|p| p.to_path_buf());
        self.config.buffer_files = buffer;
        self
    }
}

/// The standard printer, which implements grep-like formatting, including
/// color support.
///
/// A default printer can be created with either of the `Standard::new` or
/// `Standard::new_no_color` constructors. However, there are a considerable
/// number of options that configure this printer's output. Those options can
/// be configured using [`StandardBuilder`].
///
/// This type is generic over `W`, which represents any implementation
/// of the `termcolor::WriteColor` trait. If colors are not desired,
/// then the `new_no_color` constructor can be used, or, alternatively,
/// the `termcolor::NoColor` adapter can be used to wrap any `io::Write`
/// implementation without enabling any colors.
#[derive(Clone, Debug)]
pub struct Standard<W: WriteColor> {
    config: Config,
    wtr: RefCell<MultiWriter<W>>,
    matches: Vec<Match>,
}

impl<W: WriteColor> Standard<W> {
    /// Return a standard printer with a default configuration that writes
    /// matches to the given writer.
    ///
    /// The writer should be an implementation of `termcolor::WriteColor`
    /// and not just a bare implementation of `io::Write`. To use a normal
    /// `io::Write` implementation (simultaneously sacrificing colors), use
    /// the `new_no_color` constructor.
    pub fn new(wtr: W) -> Standard<W> {
        StandardBuilder::new().build(wtr)
    }
}

impl<W: io::Write> Standard<NoColor<W>> {
    /// Return a standard printer with a default configuration that writes
    /// matches to the given writer.
    ///
    /// The writer can be any implementation of `io::Write`. With this
    /// constructor, the printer will never emit colors.
    pub fn new_no_color(wtr: W) -> Standard<NoColor<W>> {
        StandardBuilder::new().build_no_color(wtr)
    }
}

impl<W: WriteColor> Standard<W> {
    /// Return an implementation of `Sink` for the standard printer.
    ///
    /// This does not associate the printer with a file path, which means this
    /// implementation will never print a file path along with the matches.
    pub fn sink<'s, M: Matcher>(
        &'s mut self,
        matcher: M,
    ) -> StandardSink<'static, 's, M, W> {
        let interpolator =
            hyperlink::Interpolator::new(&self.config.hyperlink);
        let stats = if self.config.stats { Some(Stats::new()) } else { None };
        let needs_match_granularity = self.needs_match_granularity();
        StandardSink {
            matcher,
            standard: self,
            replacer: Replacer::new(),
            interpolator,
            path: None,
            start_time: Instant::now(),
            match_count: 0,
            after_context_remaining: 0,
            binary_byte_offset: None,
            stats,
            stats_match_count: 0,
            stats_matched_line_count: 0,
            needs_match_granularity,
        }
    }

    /// Return an implementation of `Sink` associated with a file path.
    ///
    /// When the printer is associated with a path, then it may, depending on
    /// its configuration, print the path along with the matches found.
    pub fn sink_with_path<'p, 's, M, P>(
        &'s mut self,
        matcher: M,
        path: &'p P,
    ) -> StandardSink<'p, 's, M, W>
    where
        M: Matcher,
        P: ?Sized + AsRef<Path>,
    {
        let interpolator =
            hyperlink::Interpolator::new(&self.config.hyperlink);
        let stats = if self.config.stats { Some(Stats::new()) } else { None };
        let ppath = PrinterPath::new(path.as_ref())
            .with_separator(self.config.separator_path);
        let needs_match_granularity = self.needs_match_granularity();
        StandardSink {
            matcher,
            standard: self,
            replacer: Replacer::new(),
            interpolator,
            path: Some(ppath),
            start_time: Instant::now(),
            match_count: 0,
            after_context_remaining: 0,
            binary_byte_offset: None,
            stats,
            stats_match_count: 0,
            stats_matched_line_count: 0,
            needs_match_granularity,
        }
    }

    /// Returns true if and only if the configuration of the printer requires
    /// us to find each individual match in the lines reported by the searcher.
    ///
    /// We care about this distinction because finding each individual match
    /// costs more, so we only do it when we need to.
    fn needs_match_granularity(&self) -> bool {
        let supports_color = self.wtr.borrow().supports_color();
        let match_colored = !self.config.colors.matched().is_none();

        // Coloring requires identifying each individual match.
        (supports_color && match_colored)
        // The column feature requires finding the position of the first match.
        || self.config.column
        // Requires finding each match for performing replacement.
        || self.config.replacement.is_some()
        // Emitting a line for each match requires finding each match.
        || self.config.per_match
        // Emitting only the match requires finding each match.
        || self.config.only_matching
        // Computing certain statistics requires finding each match.
        || self.config.stats
    }
}

impl<W: WriteColor> Standard<W> {
    /// Returns true if and only if this printer has written at least one byte
    /// to the underlying writer during any of the previous searches,
    /// or to an internal buffer in the current search.
    pub fn has_written(&self) -> bool {
        self.wtr.borrow().total_count() > 0
    }

    /// Return a mutable reference to the underlying writer.
    pub fn get_mut(&mut self) -> &mut W {
        self.wtr.get_mut().get_mut()
    }

    /// Consume this printer and return back ownership of the underlying
    /// writer.
    pub fn into_inner(self) -> W {
        self.wtr.into_inner().into_inner()
    }
}

/// An implementation of `Sink` associated with a matcher and an optional file
/// path for the standard printer.
///
/// A `Sink` can be created via the [`Standard::sink`] or
/// [`Standard::sink_with_path`] methods, depending on whether you want to
/// include a file path in the printer's output.
///
/// Building a `StandardSink` is cheap, and callers should create a new one
/// for each thing that is searched. After a search has completed, callers may
/// query this sink for information such as whether a match occurred or whether
/// binary data was found (and if so, the offset at which it occurred).
///
/// This type is generic over a few type parameters:
///
/// * `'p` refers to the lifetime of the file path, if one is provided. When
/// no file path is given, then this is `'static`.
/// * `'s` refers to the lifetime of the [`Standard`] printer that this type
/// borrows.
/// * `M` refers to the type of matcher used by
/// `grep_searcher::Searcher` that is reporting results to this sink.
/// * `W` refers to the underlying writer that this printer is writing its
/// output to.
#[derive(Debug)]
pub struct StandardSink<'p, 's, M: Matcher, W: WriteColor> {
    matcher: M,
    standard: &'s mut Standard<W>,
    replacer: Replacer<M>,
    interpolator: hyperlink::Interpolator,
    path: Option<PrinterPath<'p>>,
    start_time: Instant,
    match_count: u64,
    after_context_remaining: u64,
    binary_byte_offset: Option<u64>,
    stats: Option<Stats>,
    stats_match_count: u64,
    stats_matched_line_count: u64,
    needs_match_granularity: bool,
}

impl<'p, 's, M: Matcher, W: WriteColor> StandardSink<'p, 's, M, W> {
    /// Returns true if and only if this printer received a match in the
    /// previous search.
    ///
    /// This is unaffected by the result of searches before the previous
    /// search on this sink.
    pub fn has_match(&self) -> bool {
        self.match_count > 0
    }

    /// Return the total number of matches reported to this sink.
    ///
    /// This corresponds to the number of times `Sink::matched` is called
    /// on the previous search.
    ///
    /// This is unaffected by the result of searches before the previous
    /// search on this sink.
    pub fn match_count(&self) -> u64 {
        self.match_count
    }

    /// If binary data was found in the previous search, this returns the
    /// offset at which the binary data was first detected.
    ///
    /// The offset returned is an absolute offset relative to the entire
    /// set of bytes searched.
    ///
    /// This is unaffected by the result of searches before the previous
    /// search. e.g., If the search prior to the previous search found binary
    /// data but the previous search found no binary data, then this will
    /// return `None`.
    pub fn binary_byte_offset(&self) -> Option<u64> {
        self.binary_byte_offset
    }

    /// Return a reference to the stats produced by the printer for all
    /// searches executed on this sink.
    ///
    /// This only returns stats if they were requested via the
    /// [`StandardBuilder`] configuration.
    pub fn stats(&self) -> Option<&Stats> {
        self.stats.as_ref()
    }

    /// Execute the matcher over the given bytes and record the match
    /// locations if the current configuration demands match granularity.
    fn record_matches(
        &mut self,
        searcher: &Searcher,
        bytes: &[u8],
        range: std::ops::Range<usize>,
    ) -> io::Result<()> {
        self.standard.matches.clear();
        if !self.needs_match_granularity {
            return Ok(());
        }
        // If printing requires knowing the location of each individual match,
        // then compute and stored those right now for use later. While this
        // adds an extra copy for storing the matches, we do amortize the
        // allocation for it and this greatly simplifies the printing logic to
        // the extent that it's easy to ensure that we never do more than
        // one search to find the matches (well, for replacements, we do one
        // additional search to perform the actual replacement).
        let matches = &mut self.standard.matches;
        find_iter_at_in_context(
            searcher,
            &self.matcher,
            bytes,
            range.clone(),
            |m| {
                let (s, e) = (m.start() - range.start, m.end() - range.start);
                matches.push(Match::new(s, e));
                true
            },
        )?;
        // Don't report empty matches appearing at the end of the bytes.
        if !matches.is_empty()
            && matches.last().unwrap().is_empty()
            && matches.last().unwrap().start() >= range.end
        {
            matches.pop().unwrap();
        }
        Ok(())
    }

    /// If the configuration specifies a replacement, then this executes the
    /// replacement, lazily allocating memory if necessary.
    ///
    /// To access the result of a replacement, use `replacer.replacement()`.
    fn replace(
        &mut self,
        searcher: &Searcher,
        bytes: &[u8],
        range: std::ops::Range<usize>,
    ) -> io::Result<()> {
        self.replacer.clear();
        if self.standard.config.replacement.is_some() {
            let replacement = (*self.standard.config.replacement)
                .as_ref()
                .map(|r| &*r)
                .unwrap();
            self.replacer.replace_all(
                searcher,
                &self.matcher,
                bytes,
                range,
                replacement,
            )?;
        }
        Ok(())
    }

    /// Returns true if this printer should quit.
    ///
    /// This must return false if we still need to check if the file is binary or not.
    fn should_quit(&self, searcher: &Searcher) -> bool {
        !searcher.binary_detection().is_strict() && self.should_stop_printing()
    }

    /// Returns true if this printer should stop printing.
    ///
    /// This implements the logic for handling printing after seeing a certain
    /// amount of matches. In most cases, the logic is simple, but we must
    /// permit all "after" contextual lines to print after reaching the limit.
    fn should_stop_printing(&self) -> bool {
        let limit = match self.standard.config.max_matches {
            None => return false,
            Some(limit) => limit,
        };
        if self.match_count < limit {
            return false;
        }
        self.after_context_remaining == 0
    }

    /// Returns whether the current match count exceeds the configured limit.
    /// If there is no limit, then this always returns false.
    fn match_more_than_limit(&self) -> bool {
        let limit = match self.standard.config.max_matches {
            None => return false,
            Some(limit) => limit,
        };
        self.match_count > limit
    }
}

impl<'p, 's, M: Matcher, W: WriteColor> Sink for StandardSink<'p, 's, M, W> {
    type Error = io::Error;

    fn matched(
        &mut self,
        searcher: &Searcher,
        mat: &SinkMatch<'_>,
    ) -> Result<bool, io::Error> {
        if self.should_stop_printing() {
            return Ok(true);
        }
        self.match_count += 1;
        // When we've exceeded our match count, then the remaining context
        // lines should not be reset, but instead, decremented. This avoids a
        // bug where we display more matches than a configured limit. The main
        // idea here is that 'matched' might be called again while printing
        // an after-context line. In that case, we should treat this as a
        // contextual line rather than a matching line for the purposes of
        // termination.
        if self.match_more_than_limit() {
            self.after_context_remaining =
                self.after_context_remaining.saturating_sub(1);
        } else {
            self.after_context_remaining = searcher.after_context() as u64;
        }

        self.record_matches(
            searcher,
            mat.buffer(),
            mat.bytes_range_in_buffer(),
        )?;
        self.replace(searcher, mat.buffer(), mat.bytes_range_in_buffer())?;

        // For stats
        self.stats_match_count += self.standard.matches.len() as u64;
        self.stats_matched_line_count += mat.lines().count() as u64;

        if searcher.binary_detection().convert_byte().is_some() {
            if self.binary_byte_offset.is_some() {
                return Ok(false);
            }
        }

        StandardImpl::from_match(searcher, self, mat).sink()?;
        Ok(!self.should_quit(searcher))
    }

    fn context(
        &mut self,
        searcher: &Searcher,
        ctx: &SinkContext<'_>,
    ) -> Result<bool, io::Error> {
        if self.should_stop_printing() {
            return Ok(true);
        }
        self.standard.matches.clear();
        self.replacer.clear();

        if ctx.kind() == &SinkContextKind::After {
            self.after_context_remaining =
                self.after_context_remaining.saturating_sub(1);
        }
        if searcher.invert_match() {
            self.record_matches(searcher, ctx.bytes(), 0..ctx.bytes().len())?;
            self.replace(searcher, ctx.bytes(), 0..ctx.bytes().len())?;
        }
        if searcher.binary_detection().convert_byte().is_some() {
            if self.binary_byte_offset.is_some() {
                return Ok(false);
            }
        }

        StandardImpl::from_context(searcher, self, ctx).sink()?;
        Ok(!self.should_quit(searcher))
    }

    fn context_break(
        &mut self,
        searcher: &Searcher,
    ) -> Result<bool, io::Error> {
        StandardImpl::new(searcher, self).write_context_separator()?;
        Ok(true)
    }

    fn binary_data(
        &mut self,
        searcher: &Searcher,
        binary_byte_offset: u64,
    ) -> Result<bool, io::Error> {
        if searcher.binary_detection().quit_byte().is_some() {
            if let Some(ref path) = self.path {
                log::debug!(
                    "ignoring {path}: found binary data at \
                     offset {binary_byte_offset}",
                    path = path.as_path().display(),
                );
            }
        }
        if searcher.binary_detection().is_strict() {
            self.standard.wtr.borrow_mut().cancel();
            // Forget if we made any matches
            self.match_count = 0;
            self.stats_match_count = 0;
            self.stats_matched_line_count = 0;
        }
        self.binary_byte_offset = Some(binary_byte_offset);
        Ok(true)
    }

    fn begin(&mut self, searcher: &Searcher) -> Result<bool, io::Error> {
        self.standard.wtr.borrow_mut().begin(
            searcher.binary_detection().is_strict(),
            self.path.as_ref().map(|p| p.as_path()),
        )?;
        self.start_time = Instant::now();
        self.match_count = 0;
        self.after_context_remaining = 0;
        self.binary_byte_offset = None;
        self.stats_match_count = 0;
        self.stats_matched_line_count = 0;
        if self.standard.config.max_matches == Some(0) {
            return Ok(false);
        }
        Ok(true)
    }

    fn finish(
        &mut self,
        searcher: &Searcher,
        finish: &SinkFinish,
    ) -> Result<(), io::Error> {
        if let Some(offset) = self.binary_byte_offset {
            StandardImpl::new(searcher, self).write_binary_message(offset)?;
        }
        self.standard.wtr.borrow_mut().finish()?;
        if let Some(stats) = self.stats.as_mut() {
            stats.add_matches(self.stats_match_count);
            stats.add_matched_lines(self.stats_matched_line_count);

            stats.add_elapsed(self.start_time.elapsed());
            stats.add_searches(1);
            if self.match_count > 0 {
                stats.add_searches_with_match(1);
            }
            stats.add_bytes_searched(finish.byte_count());
            stats.add_bytes_printed(self.standard.wtr.borrow().count());
        }
        Ok(())
    }
}

/// The actual implementation of the standard printer. This couples together
/// the searcher, the sink implementation and information about the match.
///
/// A StandardImpl is initialized every time a match or a contextual line is
/// reported.
#[derive(Debug)]
struct StandardImpl<'a, M: Matcher, W: WriteColor> {
    searcher: &'a Searcher,
    sink: &'a StandardSink<'a, 'a, M, W>,
    sunk: Sunk<'a>,
    /// Set to true if and only if we are writing a match with color.
    in_color_match: Cell<bool>,
}

impl<'a, M: Matcher, W: WriteColor> StandardImpl<'a, M, W> {
    /// Bundle self with a searcher and return the core implementation of Sink.
    fn new(
        searcher: &'a Searcher,
        sink: &'a StandardSink<'_, '_, M, W>,
    ) -> StandardImpl<'a, M, W> {
        StandardImpl {
            searcher,
            sink,
            sunk: Sunk::empty(),
            in_color_match: Cell::new(false),
        }
    }

    /// Bundle self with a searcher and return the core implementation of Sink
    /// for use with handling matching lines.
    fn from_match(
        searcher: &'a Searcher,
        sink: &'a StandardSink<'_, '_, M, W>,
        mat: &'a SinkMatch<'a>,
    ) -> StandardImpl<'a, M, W> {
        let sunk = Sunk::from_sink_match(
            mat,
            &sink.standard.matches,
            sink.replacer.replacement(),
        );
        StandardImpl { sunk, ..StandardImpl::new(searcher, sink) }
    }

    /// Bundle self with a searcher and return the core implementation of Sink
    /// for use with handling contextual lines.
    fn from_context(
        searcher: &'a Searcher,
        sink: &'a StandardSink<'_, '_, M, W>,
        ctx: &'a SinkContext<'a>,
    ) -> StandardImpl<'a, M, W> {
        let sunk = Sunk::from_sink_context(
            ctx,
            &sink.standard.matches,
            sink.replacer.replacement(),
        );
        StandardImpl { sunk, ..StandardImpl::new(searcher, sink) }
    }

    fn sink(&self) -> io::Result<()> {
        self.write_search_prelude()?;
        if self.sunk.matches().is_empty() {
            if self.multi_line() && !self.is_context() {
                self.sink_fast_multi_line()
            } else {
                self.sink_fast()
            }
        } else {
            if self.multi_line() && !self.is_context() {
                self.sink_slow_multi_line()
            } else {
                self.sink_slow()
            }
        }
    }

    /// Print matches (limited to one line) quickly by avoiding the detection
    /// of each individual match in the lines reported in the given
    /// `SinkMatch`.
    ///
    /// This should only be used when the configuration does not demand match
    /// granularity and the searcher is not in multi line mode.
    fn sink_fast(&self) -> io::Result<()> {
        debug_assert!(self.sunk.matches().is_empty());
        debug_assert!(!self.multi_line() || self.is_context());

        self.write_prelude(
            self.sunk.absolute_byte_offset(),
            self.sunk.line_number(),
            None,
        )?;
        self.write_line(self.sunk.bytes())
    }

    /// Print matches (possibly spanning more than one line) quickly by
    /// avoiding the detection of each individual match in the lines reported
    /// in the given `SinkMatch`.
    ///
    /// This should only be used when the configuration does not demand match
    /// granularity. This may be used when the searcher is in multi line mode.
    fn sink_fast_multi_line(&self) -> io::Result<()> {
        debug_assert!(self.sunk.matches().is_empty());
        // This isn't actually a required invariant for using this method,
        // but if we wind up here and multi line mode is disabled, then we
        // should still treat it as a bug since we should be using matched_fast
        // instead.
        debug_assert!(self.multi_line());

        let line_term = self.searcher.line_terminator().as_byte();
        let mut absolute_byte_offset = self.sunk.absolute_byte_offset();
        for (i, line) in self.sunk.lines(line_term).enumerate() {
            self.write_prelude(
                absolute_byte_offset,
                self.sunk.line_number().map(|n| n + i as u64),
                None,
            )?;
            absolute_byte_offset += line.len() as u64;

            self.write_line(line)?;
        }
        Ok(())
    }

    /// Print a matching line where the configuration of the printer requires
    /// finding each individual match (e.g., for coloring).
    fn sink_slow(&self) -> io::Result<()> {
        debug_assert!(!self.sunk.matches().is_empty());
        debug_assert!(!self.multi_line() || self.is_context());

        if self.config().only_matching {
            for &m in self.sunk.matches() {
                self.write_prelude(
                    self.sunk.absolute_byte_offset() + m.start() as u64,
                    self.sunk.line_number(),
                    Some(m.start() as u64 + 1),
                )?;

                let buf = &self.sunk.bytes()[m];
                self.write_colored_line(&[Match::new(0, buf.len())], buf)?;
            }
        } else if self.config().per_match {
            for &m in self.sunk.matches() {
                self.write_prelude(
                    self.sunk.absolute_byte_offset() + m.start() as u64,
                    self.sunk.line_number(),
                    Some(m.start() as u64 + 1),
                )?;
                self.write_colored_line(&[m], self.sunk.bytes())?;
            }
        } else {
            self.write_prelude(
                self.sunk.absolute_byte_offset(),
                self.sunk.line_number(),
                Some(self.sunk.matches()[0].start() as u64 + 1),
            )?;
            self.write_colored_line(self.sunk.matches(), self.sunk.bytes())?;
        }
        Ok(())
    }

    fn sink_slow_multi_line(&self) -> io::Result<()> {
        debug_assert!(!self.sunk.matches().is_empty());
        debug_assert!(self.multi_line());

        if self.config().only_matching {
            return self.sink_slow_multi_line_only_matching();
        } else if self.config().per_match {
            return self.sink_slow_multi_per_match();
        }

        let line_term = self.searcher.line_terminator().as_byte();
        let bytes = self.sunk.bytes();
        let matches = self.sunk.matches();
        let mut midx = 0;
        let mut count = 0;
        let mut stepper = LineStep::new(line_term, 0, bytes.len());
        while let Some((start, end)) = stepper.next(bytes) {
            let mut line = Match::new(start, end);
            self.write_prelude(
                self.sunk.absolute_byte_offset() + line.start() as u64,
                self.sunk.line_number().map(|n| n + count),
                Some(matches[0].start() as u64 + 1),
            )?;
            count += 1;
            self.trim_ascii_prefix(bytes, &mut line);
            if self.exceeds_max_columns(&bytes[line]) {
                self.write_exceeded_line(bytes, line, matches, &mut midx)?;
            } else {
                self.write_colored_matches(bytes, line, matches, &mut midx)?;
                if self.config().ensure_eol {
                    self.write_line_term()?;
                }
            }
        }
        Ok(())
    }

    fn sink_slow_multi_line_only_matching(&self) -> io::Result<()> {
        let line_term = self.searcher.line_terminator().as_byte();
        let spec = self.config().colors.matched();
        let bytes = self.sunk.bytes();
        let matches = self.sunk.matches();
        let mut midx = 0;
        let mut count = 0;
        let mut stepper = LineStep::new(line_term, 0, bytes.len());
        while let Some((start, end)) = stepper.next(bytes) {
            let mut line = Match::new(start, end);
            self.trim_line_terminator(bytes, &mut line);
            self.trim_ascii_prefix(bytes, &mut line);
            while !line.is_empty() {
                if matches[midx].end() <= line.start() {
                    if midx + 1 < matches.len() {
                        midx += 1;
                        continue;
                    } else {
                        break;
                    }
                }
                let m = matches[midx];

                if line.start() < m.start() {
                    let upto = cmp::min(line.end(), m.start());
                    line = line.with_start(upto);
                } else {
                    let upto = cmp::min(line.end(), m.end());
                    self.write_prelude(
                        self.sunk.absolute_byte_offset() + m.start() as u64,
                        self.sunk.line_number().map(|n| n + count),
                        Some(m.start() as u64 + 1),
                    )?;

                    let this_line = line.with_end(upto);
                    line = line.with_start(upto);
                    if self.exceeds_max_columns(&bytes[this_line]) {
                        self.write_exceeded_line(
                            bytes, this_line, matches, &mut midx,
                        )?;
                    } else {
                        self.write_spec(spec, &bytes[this_line])?;
                        self.write_line_term()?;
                    }
                }
            }
            count += 1;
        }
        Ok(())
    }

    fn sink_slow_multi_per_match(&self) -> io::Result<()> {
        let line_term = self.searcher.line_terminator().as_byte();
        let spec = self.config().colors.matched();
        let bytes = self.sunk.bytes();
        for &m in self.sunk.matches() {
            let mut count = 0;
            let mut stepper = LineStep::new(line_term, 0, bytes.len());
            while let Some((start, end)) = stepper.next(bytes) {
                let mut line = Match::new(start, end);
                if line.start() >= m.end() {
                    break;
                } else if line.end() <= m.start() {
                    count += 1;
                    continue;
                }
                self.write_prelude(
                    self.sunk.absolute_byte_offset() + line.start() as u64,
                    self.sunk.line_number().map(|n| n + count),
                    Some(m.start().saturating_sub(line.start()) as u64 + 1),
                )?;
                count += 1;
                self.trim_line_terminator(bytes, &mut line);
                self.trim_ascii_prefix(bytes, &mut line);
                if self.exceeds_max_columns(&bytes[line]) {
                    self.write_exceeded_line(bytes, line, &[m], &mut 0)?;
                    continue;
                }

                while !line.is_empty() {
                    if m.end() <= line.start() {
                        self.write(&bytes[line])?;
                        line = line.with_start(line.end());
                    } else if line.start() < m.start() {
                        let upto = cmp::min(line.end(), m.start());
                        self.write(&bytes[line.with_end(upto)])?;
                        line = line.with_start(upto);
                    } else {
                        let upto = cmp::min(line.end(), m.end());
                        self.write_spec(spec, &bytes[line.with_end(upto)])?;
                        line = line.with_start(upto);
                    }
                }
                self.write_line_term()?;
                // It turns out that vimgrep really only wants one line per
                // match, even when a match spans multiple lines. So when
                // that option is enabled, we just quit after printing the
                // first line.
                //
                // See: https://github.com/BurntSushi/ripgrep/issues/1866
                if self.config().per_match_one_line {
                    break;
                }
            }
        }
        Ok(())
    }

    /// Write the beginning part of a matching line. This (may) include things
    /// like the file path, line number among others, depending on the
    /// configuration and the parameters given.
    #[inline(always)]
    fn write_prelude(
        &self,
        absolute_byte_offset: u64,
        line_number: Option<u64>,
        column: Option<u64>,
    ) -> io::Result<()> {
        let mut prelude = PreludeWriter::new(self);
        prelude.start(line_number, column)?;
        prelude.write_path()?;
        prelude.write_line_number(line_number)?;
        prelude.write_column_number(column)?;
        prelude.write_byte_offset(absolute_byte_offset)?;
        prelude.end()
    }

    #[inline(always)]
    fn write_line(&self, line: &[u8]) -> io::Result<()> {
        let line = if !self.config().trim_ascii {
            line
        } else {
            let lineterm = self.searcher.line_terminator();
            let full_range = Match::new(0, line.len());
            let range = trim_ascii_prefix(lineterm, line, full_range);
            &line[range]
        };
        if self.exceeds_max_columns(line) {
            let range = Match::new(0, line.len());
            self.write_exceeded_line(
                line,
                range,
                self.sunk.matches(),
                &mut 0,
            )?;
        } else {
            // self.write_trim(line)?;
            self.write(line)?;
            if self.config().ensure_eol && !self.has_line_terminator(line) {
                self.write_line_term()?;
            }
        }
        Ok(())
    }

    fn write_colored_line(
        &self,
        matches: &[Match],
        bytes: &[u8],
    ) -> io::Result<()> {
        // If we know we aren't going to emit color, then we can go faster.
        let spec = self.config().colors.matched();
        if !self.wtr().borrow().supports_color() || spec.is_none() {
            return self.write_line(bytes);
        }

        let mut line = Match::new(0, bytes.len());
        self.trim_ascii_prefix(bytes, &mut line);
        if self.exceeds_max_columns(bytes) {
            self.write_exceeded_line(bytes, line, matches, &mut 0)
        } else {
            self.write_colored_matches(bytes, line, matches, &mut 0)?;
            if self.config().ensure_eol {
                self.write_line_term()?;
            }
            Ok(())
        }
    }

    /// Write the `line` portion of `bytes`, with appropriate coloring for
    /// each `match`, starting at `match_index`.
    ///
    /// This accounts for trimming any whitespace prefix and will *never* print
    /// a line terminator. If a match exceeds the range specified by `line`,
    /// then only the part of the match within `line` (if any) is printed.
    fn write_colored_matches(
        &self,
        bytes: &[u8],
        mut line: Match,
        matches: &[Match],
        match_index: &mut usize,
    ) -> io::Result<()> {
        if self.config().ensure_eol {
            // If we are in ensure_eol mode, then self.write_line_terminator will be called after this
            // so we ensure no extra line terminator is added here
            self.trim_line_terminator(bytes, &mut line);
        }

        if matches.is_empty() {
            self.write(&bytes[line])?;
            return Ok(());
        }
        while !line.is_empty() {
            if matches[*match_index].end() <= line.start() {
                if *match_index + 1 < matches.len() {
                    *match_index += 1;
                    continue;
                } else {
                    self.end_color_match()?;
                    self.write(&bytes[line])?;
                    break;
                }
            }

            let m = matches[*match_index];
            if line.start() < m.start() {
                let upto = cmp::min(line.end(), m.start());
                self.end_color_match()?;
                self.write(&bytes[line.with_end(upto)])?;
                line = line.with_start(upto);
            } else {
                let upto = cmp::min(line.end(), m.end());
                self.start_color_match()?;
                self.write(&bytes[line.with_end(upto)])?;
                line = line.with_start(upto);
            }
        }
        self.end_color_match()?;
        Ok(())
    }

    fn write_exceeded_line(
        &self,
        bytes: &[u8],
        mut line: Match,
        matches: &[Match],
        match_index: &mut usize,
    ) -> io::Result<()> {
        if self.config().max_columns_preview {
            let original = line;
            let end = bytes[line]
                .grapheme_indices()
                .map(|(_, end, _)| end)
                .take(self.config().max_columns.unwrap_or(0) as usize)
                .last()
                .unwrap_or(0)
                + line.start();
            line = line.with_end(end);
            self.write_colored_matches(bytes, line, matches, match_index)?;
            if matches.is_empty() {
                self.write(b" [... omitted end of long line]")?;
            } else {
                let remaining = matches
                    .iter()
                    .filter(|m| {
                        m.start() >= line.end() && m.start() < original.end()
                    })
                    .count();
                let tense = if remaining == 1 { "match" } else { "matches" };
                write!(
                    self.wtr().borrow_mut(),
                    " [... {} more {}]",
                    remaining,
                    tense,
                )?;
            }
            self.write_line_term()?;
            return Ok(());
        }
        if self.sunk.original_matches().is_empty() {
            if self.is_context() {
                self.write(b"[Omitted long context line]")?;
            } else {
                self.write(b"[Omitted long matching line]")?;
            }
        } else {
            if self.config().only_matching {
                if self.is_context() {
                    self.write(b"[Omitted long context line]")?;
                } else {
                    self.write(b"[Omitted long matching line]")?;
                }
            } else {
                write!(
                    self.wtr().borrow_mut(),
                    "[Omitted long line with {} matches]",
                    self.sunk.original_matches().len(),
                )?;
            }
        }
        self.write_line_term()?;
        Ok(())
    }

    /// If this printer has a file path associated with it, then this will
    /// write that path to the underlying writer followed by a line terminator.
    /// (If a path terminator is set, then that is used instead of the line
    /// terminator.)
    fn write_path_line(&self) -> io::Result<()> {
        if let Some(path) = self.path() {
            self.write_path_hyperlink(path)?;
            if let Some(term) = self.config().path_terminator {
                self.write(&[term])?;
            } else {
                self.write_line_term()?;
            }
        }
        Ok(())
    }

    fn write_search_prelude(&self) -> io::Result<()> {
        let this_search_written = self.wtr().borrow().count() > 0;
        if this_search_written {
            return Ok(());
        }
        if let Some(ref sep) = *self.config().separator_search {
            let ever_written = self.wtr().borrow().total_count() > 0;
            if ever_written {
                self.write(sep)?;
                self.write_line_term()?;
            }
        }
        if self.config().heading {
            self.write_path_line()?;
        }
        Ok(())
    }

    fn write_binary_message(&self, offset: u64) -> io::Result<()> {
        if self.sink.match_count == 0 {
            return Ok(());
        }

        let bin = self.searcher.binary_detection();
        if let Some(byte) = bin.quit_byte() {
            if let Some(path) = self.path() {
                self.write_path_hyperlink(path)?;
                self.write(b": ")?;
            }
            let remainder = format!(
                "WARNING: stopped searching binary file after match \
                 (found {:?} byte around offset {})\n",
                [byte].as_bstr(),
                offset,
            );
            self.write(remainder.as_bytes())?;
        } else if let Some(byte) = bin.convert_byte() {
            if let Some(path) = self.path() {
                self.write_path_hyperlink(path)?;
                self.write(b": ")?;
            }
            let remainder = format!(
                "binary file matches (found {:?} byte around offset {})\n",
                [byte].as_bstr(),
                offset,
            );
            self.write(remainder.as_bytes())?;
        }
        Ok(())
    }

    fn write_context_separator(&self) -> io::Result<()> {
        if let Some(ref sep) = *self.config().separator_context {
            self.write(sep)?;
            self.write_line_term()?;
        }
        Ok(())
    }

    fn write_line_term(&self) -> io::Result<()> {
        self.write(self.searcher.line_terminator().as_bytes())
    }

    fn write_spec(&self, spec: &ColorSpec, buf: &[u8]) -> io::Result<()> {
        let mut wtr = self.wtr().borrow_mut();
        wtr.set_color(spec)?;
        wtr.write_all(buf)?;
        wtr.reset()?;
        Ok(())
    }

    fn write_path(&self, path: &PrinterPath) -> io::Result<()> {
        let mut wtr = self.wtr().borrow_mut();
        wtr.set_color(self.config().colors.path())?;
        wtr.write_all(path.as_bytes())?;
        wtr.reset()
    }

    fn write_path_hyperlink(&self, path: &PrinterPath) -> io::Result<()> {
        let status = self.start_hyperlink(path, None, None)?;
        self.write_path(path)?;
        self.end_hyperlink(status)
    }

    fn start_hyperlink(
        &self,
        path: &PrinterPath,
        line_number: Option<u64>,
        column: Option<u64>,
    ) -> io::Result<hyperlink::InterpolatorStatus> {
        let Some(hyperpath) = path.as_hyperlink() else {
            return Ok(hyperlink::InterpolatorStatus::inactive());
        };
        let values =
            hyperlink::Values::new(hyperpath).line(line_number).column(column);
        self.sink.interpolator.begin(&values, &mut *self.wtr().borrow_mut())
    }

    fn end_hyperlink(
        &self,
        status: hyperlink::InterpolatorStatus,
    ) -> io::Result<()> {
        self.sink.interpolator.finish(status, &mut *self.wtr().borrow_mut())
    }

    fn start_color_match(&self) -> io::Result<()> {
        if self.in_color_match.get() {
            return Ok(());
        }
        self.wtr().borrow_mut().set_color(self.config().colors.matched())?;
        self.in_color_match.set(true);
        Ok(())
    }

    fn end_color_match(&self) -> io::Result<()> {
        if !self.in_color_match.get() {
            return Ok(());
        }
        self.wtr().borrow_mut().reset()?;
        self.in_color_match.set(false);
        Ok(())
    }

    fn write(&self, buf: &[u8]) -> io::Result<()> {
        self.wtr().borrow_mut().write_all(buf)
    }

    fn trim_line_terminator(&self, buf: &[u8], line: &mut Match) {
        trim_line_terminator(&self.searcher, buf, line);
    }

    fn has_line_terminator(&self, buf: &[u8]) -> bool {
        self.searcher.line_terminator().is_suffix(buf)
    }

    fn is_context(&self) -> bool {
        self.sunk.context_kind().is_some()
    }

    /// Return the underlying configuration for this printer.
    fn config(&self) -> &'a Config {
        &self.sink.standard.config
    }

    /// Return the underlying writer that we are printing to.
    fn wtr(&self) -> &'a RefCell<MultiWriter<W>> {
        &self.sink.standard.wtr
    }

    /// Return the path associated with this printer, if one exists and we need to print it.
    fn path(&self) -> Option<&'a PrinterPath<'a>> {
        if self.config().path {
            self.sink.path.as_ref()
        } else {
            None
        }
    }

    /// Return the appropriate field separator based on whether we are emitting
    /// matching or contextual lines.
    fn separator_field(&self) -> &[u8] {
        if self.is_context() {
            &self.config().separator_field_context
        } else {
            &self.config().separator_field_match
        }
    }

    /// Returns true if and only if the given line exceeds the maximum number
    /// of columns set. If no maximum is set, then this always returns false.
    fn exceeds_max_columns(&self, line: &[u8]) -> bool {
        self.config().max_columns.map_or(false, |m| line.len() as u64 > m)
    }

    /// Returns true if and only if the searcher may report matches over
    /// multiple lines.
    ///
    /// Note that this doesn't just return whether the searcher is in multi
    /// line mode, but also checks if the matter can match over multiple lines.
    /// If it can't, then we don't need multi line handling, even if the
    /// searcher has multi line mode enabled.
    fn multi_line(&self) -> bool {
        self.searcher.multi_line_with_matcher(&self.sink.matcher)
    }

    /// Trim prefix ASCII spaces from the given slice and return the
    /// corresponding range.
    ///
    /// This stops trimming a prefix as soon as it sees non-whitespace or a
    /// line terminator.
    fn trim_ascii_prefix(&self, slice: &[u8], range: &mut Match) {
        if !self.config().trim_ascii {
            return;
        }
        let lineterm = self.searcher.line_terminator();
        *range = trim_ascii_prefix(lineterm, slice, *range)
    }
}

/// A writer for the prelude (the beginning part of a matching line).
///
/// This encapsulates the state needed to print the prelude.
struct PreludeWriter<'a, M: Matcher, W: WriteColor> {
    std: &'a StandardImpl<'a, M, W>,
    next_separator: PreludeSeparator,
    field_separator: &'a [u8],
    interp_status: hyperlink::InterpolatorStatus,
}

/// A type of separator used in the prelude
enum PreludeSeparator {
    /// No separator.
    None,
    /// The field separator, either for a matching or contextual line.
    FieldSeparator,
    /// The path terminator.
    PathTerminator,
}

impl<'a, M: Matcher, W: WriteColor> PreludeWriter<'a, M, W> {
    /// Creates a new prelude printer.
    #[inline(always)]
    fn new(std: &'a StandardImpl<'a, M, W>) -> PreludeWriter<'a, M, W> {
        PreludeWriter {
            std,
            next_separator: PreludeSeparator::None,
            field_separator: std.separator_field(),
            interp_status: hyperlink::InterpolatorStatus::inactive(),
        }
    }

    /// Starts the prelude with a hyperlink when applicable.
    ///
    /// If a heading was written, and the hyperlink format is invariant on
    /// the line number, then this doesn't hyperlink each line prelude, as it
    /// wouldn't point to the line anyway. The hyperlink on the heading should
    /// be sufficient and less confusing.
    #[inline(always)]
    fn start(
        &mut self,
        line_number: Option<u64>,
        column: Option<u64>,
    ) -> io::Result<()> {
        let Some(path) = self.std.path() else { return Ok(()) };
        if self.config().hyperlink.format().is_line_dependent()
            || !self.config().heading
        {
            self.interp_status =
                self.std.start_hyperlink(path, line_number, column)?;
        }
        Ok(())
    }

    /// Ends the prelude and writes the remaining output.
    #[inline(always)]
    fn end(&mut self) -> io::Result<()> {
        self.std.end_hyperlink(std::mem::replace(
            &mut self.interp_status,
            hyperlink::InterpolatorStatus::inactive(),
        ))?;
        self.write_separator()
    }

    /// If this printer has a file path associated with it, then this will
    /// write that path to the underlying writer followed by the given field
    /// separator. (If a path terminator is set, then that is used instead of
    /// the field separator.)
    #[inline(always)]
    fn write_path(&mut self) -> io::Result<()> {
        // The prelude doesn't handle headings, only what comes before a match
        // on the same line. So if we are emitting paths in headings, we should
        // not do it here on each line.
        if self.config().heading {
            return Ok(());
        }
        let Some(path) = self.std.path() else { return Ok(()) };
        self.write_separator()?;
        self.std.write_path(path)?;

        self.next_separator = if self.config().path_terminator.is_some() {
            PreludeSeparator::PathTerminator
        } else {
            PreludeSeparator::FieldSeparator
        };
        Ok(())
    }

    /// Writes the line number field if present.
    #[inline(always)]
    fn write_line_number(&mut self, line: Option<u64>) -> io::Result<()> {
        let Some(line_number) = line else { return Ok(()) };
        self.write_separator()?;
        let n = DecimalFormatter::new(line_number);
        self.std.write_spec(self.config().colors.line(), n.as_bytes())?;
        self.next_separator = PreludeSeparator::FieldSeparator;
        Ok(())
    }

    /// Writes the column number field if present and configured to do so.
    #[inline(always)]
    fn write_column_number(&mut self, column: Option<u64>) -> io::Result<()> {
        if !self.config().column {
            return Ok(());
        }
        let Some(column_number) = column else { return Ok(()) };
        self.write_separator()?;
        let n = DecimalFormatter::new(column_number);
        self.std.write_spec(self.config().colors.column(), n.as_bytes())?;
        self.next_separator = PreludeSeparator::FieldSeparator;
        Ok(())
    }

    /// Writes the byte offset field if configured to do so.
    #[inline(always)]
    fn write_byte_offset(&mut self, offset: u64) -> io::Result<()> {
        if !self.config().byte_offset {
            return Ok(());
        }
        self.write_separator()?;
        let n = DecimalFormatter::new(offset);
        self.std.write_spec(self.config().colors.column(), n.as_bytes())?;
        self.next_separator = PreludeSeparator::FieldSeparator;
        Ok(())
    }

    /// Writes the separator defined by the preceding field.
    ///
    /// This is called before writing the contents of a field, and at
    /// the end of the prelude.
    #[inline(always)]
    fn write_separator(&mut self) -> io::Result<()> {
        match self.next_separator {
            PreludeSeparator::None => {}
            PreludeSeparator::FieldSeparator => {
                self.std.write(self.field_separator)?;
            }
            PreludeSeparator::PathTerminator => {
                if let Some(term) = self.config().path_terminator {
                    self.std.write(&[term])?;
                }
            }
        }
        self.next_separator = PreludeSeparator::None;
        Ok(())
    }

    #[inline(always)]
    fn config(&self) -> &Config {
        self.std.config()
    }
}

#[cfg(test)]
mod tests {
    use grep_matcher::LineTerminator;
    use grep_regex::{RegexMatcher, RegexMatcherBuilder};
    use grep_searcher::SearcherBuilder;
    use termcolor::{Ansi, NoColor};

    use super::{ColorSpecs, Standard, StandardBuilder};

    const SHERLOCK: &'static str = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
be, to a very large extent, the result of luck. Sherlock Holmes
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.\
";

    #[allow(dead_code)]
    const SHERLOCK_CRLF: &'static str = "\
For the Doctor Watsons of this world, as opposed to the Sherlock\r
Holmeses, success in the province of detective work must always\r
be, to a very large extent, the result of luck. Sherlock Holmes\r
can extract a clew from a wisp of straw or a flake of cigar ash;\r
but Doctor Watson has to have it taken out for him and dusted,\r
and exhibited clearly, with a label attached.\
";

    fn printer_contents(printer: &mut Standard<NoColor<Vec<u8>>>) -> String {
        String::from_utf8(printer.get_mut().get_ref().to_owned()).unwrap()
    }

    fn printer_contents_ansi(printer: &mut Standard<Ansi<Vec<u8>>>) -> String {
        String::from_utf8(printer.get_mut().get_ref().to_owned()).unwrap()
    }

    #[test]
    fn reports_match() {
        let matcher = RegexMatcher::new("Sherlock").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        let mut sink = printer.sink(&matcher);
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
            .unwrap();
        assert!(sink.has_match());

        let matcher = RegexMatcher::new("zzzzz").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        let mut sink = printer.sink(&matcher);
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
            .unwrap();
        assert!(!sink.has_match());
    }

    #[test]
    fn reports_binary() {
        use grep_searcher::BinaryDetection;

        let matcher = RegexMatcher::new("Sherlock").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        let mut sink = printer.sink(&matcher);
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
            .unwrap();
        assert!(sink.binary_byte_offset().is_none());

        let matcher = RegexMatcher::new(".+").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        let mut sink = printer.sink(&matcher);
        SearcherBuilder::new()
            .line_number(false)
            .binary_detection(BinaryDetection::quit(b'\x00'))
            .build()
            .search_reader(&matcher, &b"abc\x00"[..], &mut sink)
            .unwrap();
        assert_eq!(sink.binary_byte_offset(), Some(3));
    }

    #[test]
    fn reports_stats() {
        use std::time::Duration;

        let matcher = RegexMatcher::new("Sherlock|opposed").unwrap();
        let mut printer =
            StandardBuilder::new().stats(true).build(NoColor::new(vec![]));
        let stats = {
            let mut sink = printer.sink(&matcher);
            SearcherBuilder::new()
                .line_number(false)
                .build()
                .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
                .unwrap();
            sink.stats().unwrap().clone()
        };
        let buf = printer_contents(&mut printer);

        assert!(stats.elapsed() > Duration::default());
        assert_eq!(stats.searches(), 1);
        assert_eq!(stats.searches_with_match(), 1);
        assert_eq!(stats.bytes_searched(), SHERLOCK.len() as u64);
        assert_eq!(stats.bytes_printed(), buf.len() as u64);
        assert_eq!(stats.matched_lines(), 2);
        assert_eq!(stats.matches(), 3);
    }

    #[test]
    fn reports_stats_multiple() {
        use std::time::Duration;

        let matcher = RegexMatcher::new("Sherlock|opposed").unwrap();
        let mut printer =
            StandardBuilder::new().stats(true).build(NoColor::new(vec![]));
        let stats = {
            let mut sink = printer.sink(&matcher);
            SearcherBuilder::new()
                .line_number(false)
                .build()
                .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
                .unwrap();
            SearcherBuilder::new()
                .line_number(false)
                .build()
                .search_reader(&matcher, &b"zzzzzzzzzz"[..], &mut sink)
                .unwrap();
            SearcherBuilder::new()
                .line_number(false)
                .build()
                .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
                .unwrap();
            sink.stats().unwrap().clone()
        };
        let buf = printer_contents(&mut printer);

        assert!(stats.elapsed() > Duration::default());
        assert_eq!(stats.searches(), 3);
        assert_eq!(stats.searches_with_match(), 2);
        assert_eq!(stats.bytes_searched(), 10 + 2 * SHERLOCK.len() as u64);
        assert_eq!(stats.bytes_printed(), buf.len() as u64);
        assert_eq!(stats.matched_lines(), 4);
        assert_eq!(stats.matches(), 6);
    }

    #[test]
    fn context_break() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .separator_context(Some(b"--abc--".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
--abc--
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn context_break_multiple_no_heading() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .separator_search(Some(b"--xyz--".to_vec()))
            .separator_context(Some(b"--abc--".to_vec()))
            .build(NoColor::new(vec![]));

        SearcherBuilder::new()
            .line_number(false)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();
        SearcherBuilder::new()
            .line_number(false)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
--abc--
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.
--xyz--
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
--abc--
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn context_break_multiple_heading() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .heading(true)
            .separator_search(Some(b"--xyz--".to_vec()))
            .separator_context(Some(b"--abc--".to_vec()))
            .build(NoColor::new(vec![]));

        SearcherBuilder::new()
            .line_number(false)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();
        SearcherBuilder::new()
            .line_number(false)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
--abc--
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.
--xyz--
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
--abc--
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn path() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer =
            StandardBuilder::new().path(false).build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:For the Doctor Watsons of this world, as opposed to the Sherlock
5:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn separator_field() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .separator_field_match(b"!!".to_vec())
            .separator_field_context(b"^^".to_vec())
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
sherlock!!For the Doctor Watsons of this world, as opposed to the Sherlock
sherlock^^Holmeses, success in the province of detective work must always
--
sherlock^^can extract a clew from a wisp of straw or a flake of cigar ash;
sherlock!!but Doctor Watson has to have it taken out for him and dusted,
sherlock^^and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn separator_path() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .separator_path(Some(b'Z'))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "books/sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
booksZsherlock:For the Doctor Watsons of this world, as opposed to the Sherlock
booksZsherlock:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn path_terminator() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .path_terminator(Some(b'Z'))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "books/sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
books/sherlockZFor the Doctor Watsons of this world, as opposed to the Sherlock
books/sherlockZbut Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn heading() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer =
            StandardBuilder::new().heading(true).build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
sherlock
For the Doctor Watsons of this world, as opposed to the Sherlock
but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn no_heading() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer =
            StandardBuilder::new().heading(false).build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
sherlock:For the Doctor Watsons of this world, as opposed to the Sherlock
sherlock:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn no_heading_multiple() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer =
            StandardBuilder::new().heading(false).build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let matcher = RegexMatcher::new("Sherlock").unwrap();
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
sherlock:For the Doctor Watsons of this world, as opposed to the Sherlock
sherlock:but Doctor Watson has to have it taken out for him and dusted,
sherlock:For the Doctor Watsons of this world, as opposed to the Sherlock
sherlock:be, to a very large extent, the result of luck. Sherlock Holmes
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn heading_multiple() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer =
            StandardBuilder::new().heading(true).build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let matcher = RegexMatcher::new("Sherlock").unwrap();
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
sherlock
For the Doctor Watsons of this world, as opposed to the Sherlock
but Doctor Watson has to have it taken out for him and dusted,
sherlock
For the Doctor Watsons of this world, as opposed to the Sherlock
be, to a very large extent, the result of luck. Sherlock Holmes
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn trim_ascii() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .trim_ascii(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                "   Watson".as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
Watson
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn trim_ascii_multi_line() {
        let matcher = RegexMatcher::new("(?s:.{0})Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .trim_ascii(true)
            .stats(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                "   Watson".as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
Watson
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn trim_ascii_with_line_term() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .trim_ascii(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .before_context(1)
            .build()
            .search_reader(
                &matcher,
                "\n   Watson".as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1-
2:Watson
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn line_number() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:For the Doctor Watsons of this world, as opposed to the Sherlock
5:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn line_number_multi_line() {
        let matcher = RegexMatcher::new("(?s)Watson.+Watson").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:For the Doctor Watsons of this world, as opposed to the Sherlock
2:Holmeses, success in the province of detective work must always
3:be, to a very large extent, the result of luck. Sherlock Holmes
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn column_number() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer =
            StandardBuilder::new().column(true).build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
16:For the Doctor Watsons of this world, as opposed to the Sherlock
12:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn column_number_multi_line() {
        let matcher = RegexMatcher::new("(?s)Watson.+Watson").unwrap();
        let mut printer =
            StandardBuilder::new().column(true).build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
16:For the Doctor Watsons of this world, as opposed to the Sherlock
16:Holmeses, success in the province of detective work must always
16:be, to a very large extent, the result of luck. Sherlock Holmes
16:can extract a clew from a wisp of straw or a flake of cigar ash;
16:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn byte_offset() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .byte_offset(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
0:For the Doctor Watsons of this world, as opposed to the Sherlock
258:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn byte_offset_multi_line() {
        let matcher = RegexMatcher::new("(?s)Watson.+Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .byte_offset(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
0:For the Doctor Watsons of this world, as opposed to the Sherlock
65:Holmeses, success in the province of detective work must always
129:be, to a very large extent, the result of luck. Sherlock Holmes
193:can extract a clew from a wisp of straw or a flake of cigar ash;
258:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns() {
        let matcher = RegexMatcher::new("ash|dusted").unwrap();
        let mut printer = StandardBuilder::new()
            .max_columns(Some(63))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
[Omitted long matching line]
but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns_preview() {
        let matcher = RegexMatcher::new("exhibited|dusted").unwrap();
        let mut printer = StandardBuilder::new()
            .max_columns(Some(46))
            .max_columns_preview(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
but Doctor Watson has to have it taken out for [... omitted end of long line]
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns_with_count() {
        let matcher = RegexMatcher::new("cigar|ash|dusted").unwrap();
        let mut printer = StandardBuilder::new()
            .stats(true)
            .max_columns(Some(63))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
[Omitted long line with 2 matches]
but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns_with_count_preview_no_match() {
        let matcher = RegexMatcher::new("exhibited|has to have it").unwrap();
        let mut printer = StandardBuilder::new()
            .stats(true)
            .max_columns(Some(46))
            .max_columns_preview(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
but Doctor Watson has to have it taken out for [... 0 more matches]
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns_with_count_preview_one_match() {
        let matcher = RegexMatcher::new("exhibited|dusted").unwrap();
        let mut printer = StandardBuilder::new()
            .stats(true)
            .max_columns(Some(46))
            .max_columns_preview(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
but Doctor Watson has to have it taken out for [... 1 more match]
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns_with_count_preview_two_matches() {
        let matcher =
            RegexMatcher::new("exhibited|dusted|has to have it").unwrap();
        let mut printer = StandardBuilder::new()
            .stats(true)
            .max_columns(Some(46))
            .max_columns_preview(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
but Doctor Watson has to have it taken out for [... 1 more match]
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns_multi_line() {
        let matcher = RegexMatcher::new("(?s)ash.+dusted").unwrap();
        let mut printer = StandardBuilder::new()
            .max_columns(Some(63))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
[Omitted long matching line]
but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_columns_multi_line_preview() {
        let matcher =
            RegexMatcher::new("(?s)clew|cigar ash.+have it|exhibited")
                .unwrap();
        let mut printer = StandardBuilder::new()
            .stats(true)
            .max_columns(Some(46))
            .max_columns_preview(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
can extract a clew from a wisp of straw or a f [... 1 more match]
but Doctor Watson has to have it taken out for [... 0 more matches]
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_matches() {
        let matcher = RegexMatcher::new("Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .max_matches(Some(1))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_matches_context() {
        // after context: 1
        let matcher = RegexMatcher::new("Doctor Watsons").unwrap();
        let mut printer = StandardBuilder::new()
            .max_matches(Some(1))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
";
        assert_eq_printed!(expected, got);

        // after context: 4
        let mut printer = StandardBuilder::new()
            .max_matches(Some(1))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .after_context(4)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
be, to a very large extent, the result of luck. Sherlock Holmes
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);

        // after context: 1, max matches: 2
        let matcher = RegexMatcher::new("Doctor Watsons|but Doctor").unwrap();
        let mut printer = StandardBuilder::new()
            .max_matches(Some(2))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
--
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);

        // after context: 4, max matches: 2
        let mut printer = StandardBuilder::new()
            .max_matches(Some(2))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .after_context(4)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
be, to a very large extent, the result of luck. Sherlock Holmes
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_matches_multi_line1() {
        let matcher = RegexMatcher::new("(?s:.{0})Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .max_matches(Some(1))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn max_matches_multi_line2() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?(Holmeses|clearly)").unwrap();
        let mut printer = StandardBuilder::new()
            .max_matches(Some(1))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching() {
        let matcher = RegexMatcher::new("Doctor Watsons|Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:Doctor Watsons
1:57:Sherlock
3:49:Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_multi_line1() {
        let matcher =
            RegexMatcher::new(r"(?s:.{0})(Doctor Watsons|Sherlock)").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:Doctor Watsons
1:57:Sherlock
3:49:Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_multi_line2() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?(Holmeses|clearly)").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:16:Watsons of this world, as opposed to the Sherlock
2:16:Holmeses
5:12:Watson has to have it taken out for him and dusted,
6:12:and exhibited clearly
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_max_columns() {
        let matcher = RegexMatcher::new("Doctor Watsons|Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .max_columns(Some(10))
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:[Omitted long matching line]
1:57:Sherlock
3:49:Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_max_columns_preview() {
        let matcher = RegexMatcher::new("Doctor Watsons|Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .max_columns(Some(10))
            .max_columns_preview(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:Doctor Wat [... 0 more matches]
1:57:Sherlock
3:49:Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_max_columns_multi_line1() {
        // The `(?s:.{0})` trick fools the matcher into thinking that it
        // can match across multiple lines without actually doing so. This is
        // so we can test multi-line handling in the case of a match on only
        // one line.
        let matcher =
            RegexMatcher::new(r"(?s:.{0})(Doctor Watsons|Sherlock)").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .max_columns(Some(10))
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:[Omitted long matching line]
1:57:Sherlock
3:49:Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_max_columns_preview_multi_line1() {
        // The `(?s:.{0})` trick fools the matcher into thinking that it
        // can match across multiple lines without actually doing so. This is
        // so we can test multi-line handling in the case of a match on only
        // one line.
        let matcher =
            RegexMatcher::new(r"(?s:.{0})(Doctor Watsons|Sherlock)").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .max_columns(Some(10))
            .max_columns_preview(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:Doctor Wat [... 0 more matches]
1:57:Sherlock
3:49:Sherlock
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_max_columns_multi_line2() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?(Holmeses|clearly)").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .max_columns(Some(50))
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:16:Watsons of this world, as opposed to the Sherlock
2:16:Holmeses
5:12:[Omitted long matching line]
6:12:and exhibited clearly
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn only_matching_max_columns_preview_multi_line2() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?(Holmeses|clearly)").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .max_columns(Some(50))
            .max_columns_preview(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:16:Watsons of this world, as opposed to the Sherlock
2:16:Holmeses
5:12:Watson has to have it taken out for him and dusted [... 0 more matches]
6:12:and exhibited clearly
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn per_match() {
        let matcher = RegexMatcher::new("Doctor Watsons|Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:For the Doctor Watsons of this world, as opposed to the Sherlock
1:57:For the Doctor Watsons of this world, as opposed to the Sherlock
3:49:be, to a very large extent, the result of luck. Sherlock Holmes
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn per_match_multi_line1() {
        let matcher =
            RegexMatcher::new(r"(?s:.{0})(Doctor Watsons|Sherlock)").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:For the Doctor Watsons of this world, as opposed to the Sherlock
1:57:For the Doctor Watsons of this world, as opposed to the Sherlock
3:49:be, to a very large extent, the result of luck. Sherlock Holmes
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn per_match_multi_line2() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?(Holmeses|clearly)").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:16:For the Doctor Watsons of this world, as opposed to the Sherlock
2:1:Holmeses, success in the province of detective work must always
5:12:but Doctor Watson has to have it taken out for him and dusted,
6:1:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn per_match_multi_line3() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?Holmeses|always.+?be").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:16:For the Doctor Watsons of this world, as opposed to the Sherlock
2:1:Holmeses, success in the province of detective work must always
2:58:Holmeses, success in the province of detective work must always
3:1:be, to a very large extent, the result of luck. Sherlock Holmes
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn per_match_multi_line1_only_first_line() {
        let matcher =
            RegexMatcher::new(r"(?s:.{0})(Doctor Watsons|Sherlock)").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .per_match_one_line(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:9:For the Doctor Watsons of this world, as opposed to the Sherlock
1:57:For the Doctor Watsons of this world, as opposed to the Sherlock
3:49:be, to a very large extent, the result of luck. Sherlock Holmes
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn per_match_multi_line2_only_first_line() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?(Holmeses|clearly)").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .per_match_one_line(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:16:For the Doctor Watsons of this world, as opposed to the Sherlock
5:12:but Doctor Watson has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn per_match_multi_line3_only_first_line() {
        let matcher =
            RegexMatcher::new(r"(?s)Watson.+?Holmeses|always.+?be").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .per_match_one_line(true)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:16:For the Doctor Watsons of this world, as opposed to the Sherlock
2:58:Holmeses, success in the province of detective work must always
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_passthru() {
        let matcher = RegexMatcher::new(r"Sherlock|Doctor (\w+)").unwrap();
        let mut printer = StandardBuilder::new()
            .replacement(Some(b"doctah $1 MD".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .passthru(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:For the doctah Watsons MD of this world, as opposed to the doctah  MD
2-Holmeses, success in the province of detective work must always
3:be, to a very large extent, the result of luck. doctah  MD Holmes
4-can extract a clew from a wisp of straw or a flake of cigar ash;
5:but doctah Watson MD has to have it taken out for him and dusted,
6-and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement() {
        let matcher = RegexMatcher::new(r"Sherlock|Doctor (\w+)").unwrap();
        let mut printer = StandardBuilder::new()
            .replacement(Some(b"doctah $1 MD".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:For the doctah Watsons MD of this world, as opposed to the doctah  MD
3:be, to a very large extent, the result of luck. doctah  MD Holmes
5:but doctah Watson MD has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    // This is a somewhat weird test that checks the behavior of attempting
    // to replace a line terminator with something else.
    //
    // See: https://github.com/BurntSushi/ripgrep/issues/1311
    #[test]
    fn replacement_multi_line() {
        let matcher = RegexMatcher::new(r"\n").unwrap();
        let mut printer = StandardBuilder::new()
            .replacement(Some(b"?".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                "hello\nworld\n".as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "1:hello?world?\n";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_multi_line_diff_line_term() {
        let matcher = RegexMatcherBuilder::new()
            .line_terminator(Some(b'\x00'))
            .build(r"\n")
            .unwrap();
        let mut printer = StandardBuilder::new()
            .replacement(Some(b"?".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_terminator(LineTerminator::byte(b'\x00'))
            .line_number(true)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                "hello\nworld\n".as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "1:hello?world?\x00";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_multi_line_combine_lines() {
        let matcher = RegexMatcher::new(r"\n(.)?").unwrap();
        let mut printer = StandardBuilder::new()
            .replacement(Some(b"?$1".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                "hello\nworld\n".as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "1:hello?world?\n";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_max_columns() {
        let matcher = RegexMatcher::new(r"Sherlock|Doctor (\w+)").unwrap();
        let mut printer = StandardBuilder::new()
            .max_columns(Some(67))
            .replacement(Some(b"doctah $1 MD".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:[Omitted long line with 2 matches]
3:be, to a very large extent, the result of luck. doctah  MD Holmes
5:but doctah Watson MD has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_max_columns_preview1() {
        let matcher = RegexMatcher::new(r"Sherlock|Doctor (\w+)").unwrap();
        let mut printer = StandardBuilder::new()
            .max_columns(Some(67))
            .max_columns_preview(true)
            .replacement(Some(b"doctah $1 MD".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:For the doctah Watsons MD of this world, as opposed to the doctah   [... 0 more matches]
3:be, to a very large extent, the result of luck. doctah  MD Holmes
5:but doctah Watson MD has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_max_columns_preview2() {
        let matcher =
            RegexMatcher::new("exhibited|dusted|has to have it").unwrap();
        let mut printer = StandardBuilder::new()
            .max_columns(Some(43))
            .max_columns_preview(true)
            .replacement(Some(b"xxx".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
but Doctor Watson xxx taken out for him and [... 1 more match]
and xxx clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_only_matching() {
        let matcher = RegexMatcher::new(r"Sherlock|Doctor (\w+)").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .replacement(Some(b"doctah $1 MD".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:doctah Watsons MD
1:doctah  MD
3:doctah  MD
5:doctah Watson MD
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn replacement_per_match() {
        let matcher = RegexMatcher::new(r"Sherlock|Doctor (\w+)").unwrap();
        let mut printer = StandardBuilder::new()
            .per_match(true)
            .replacement(Some(b"doctah $1 MD".to_vec()))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1:For the doctah Watsons MD of this world, as opposed to the doctah  MD
1:For the doctah Watsons MD of this world, as opposed to the doctah  MD
3:be, to a very large extent, the result of luck. doctah  MD Holmes
5:but doctah Watson MD has to have it taken out for him and dusted,
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn invert() {
        let matcher = RegexMatcher::new(r"Sherlock").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .invert_match(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
2:Holmeses, success in the province of detective work must always
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
6:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn invert_multi_line() {
        let matcher = RegexMatcher::new(r"(?s:.{0})Sherlock").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .invert_match(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
2:Holmeses, success in the province of detective work must always
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
6:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn invert_context() {
        let matcher = RegexMatcher::new(r"Sherlock").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .invert_match(true)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1-For the Doctor Watsons of this world, as opposed to the Sherlock
2:Holmeses, success in the province of detective work must always
3-be, to a very large extent, the result of luck. Sherlock Holmes
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
6:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn invert_context_multi_line() {
        let matcher = RegexMatcher::new(r"(?s:.{0})Sherlock").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .invert_match(true)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1-For the Doctor Watsons of this world, as opposed to the Sherlock
2:Holmeses, success in the province of detective work must always
3-be, to a very large extent, the result of luck. Sherlock Holmes
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
6:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn invert_context_only_matching() {
        let matcher = RegexMatcher::new(r"Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .invert_match(true)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1-Sherlock
2:Holmeses, success in the province of detective work must always
3-Sherlock
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
6:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn invert_context_only_matching_multi_line() {
        let matcher = RegexMatcher::new(r"(?s:.{0})Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .multi_line(true)
            .line_number(true)
            .invert_match(true)
            .before_context(1)
            .after_context(1)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
1-Sherlock
2:Holmeses, success in the province of detective work must always
3-Sherlock
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
6:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn regression_search_empty_with_crlf() {
        let matcher =
            RegexMatcherBuilder::new().crlf(true).build(r"x?").unwrap();
        let mut printer = StandardBuilder::new()
            .color_specs(ColorSpecs::default_with_color())
            .build(Ansi::new(vec![]));
        SearcherBuilder::new()
            .line_terminator(LineTerminator::crlf())
            .build()
            .search_reader(&matcher, &b"\n"[..], printer.sink(&matcher))
            .unwrap();

        let got = printer_contents_ansi(&mut printer);
        assert!(!got.is_empty());
    }

    #[test]
    fn regression_after_context_with_match() {
        let haystack = "\
a
b
c
d
e
d
e
d
e
d
e
";

        let matcher = RegexMatcherBuilder::new().build(r"d").unwrap();
        let mut printer = StandardBuilder::new()
            .max_matches(Some(1))
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .after_context(2)
            .build()
            .search_reader(
                &matcher,
                haystack.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "4:d\n5-e\n6:d\n";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn regression_crlf_preserve() {
        let haystack = "hello\nworld\r\n";
        let matcher =
            RegexMatcherBuilder::new().crlf(true).build(r".").unwrap();
        let mut printer = StandardBuilder::new().build(NoColor::new(vec![]));
        let mut searcher = SearcherBuilder::new()
            .line_number(false)
            .line_terminator(LineTerminator::crlf())
            .build();

        searcher
            .search_reader(
                &matcher,
                haystack.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();
        let got = printer_contents(&mut printer);
        let expected = "hello\nworld\r\n";
        assert_eq_printed!(expected, got);

        let mut printer = StandardBuilder::new()
            .replacement(Some(b"$0".to_vec()))
            .build(NoColor::new(vec![]));
        searcher
            .search_reader(
                &matcher,
                haystack.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();
        let got = printer_contents(&mut printer);
        let expected = "hello\nworld\r\n";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn no_ensure_eol() {
        assert_eq!('.', SHERLOCK.chars().last().unwrap()); // No EOL at the end of sherlock
        let matcher = RegexMatcher::new(".").unwrap();

        let mut printer = StandardBuilder::new()
            .ensure_eol(true)
            .build(NoColor::new(vec![]));
        let mut sink = printer.sink(&matcher);
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
            .unwrap();
        assert_eq_printed!(
            SHERLOCK.to_owned() + "\n",
            printer_contents(&mut printer)
        );

        let mut printer = StandardBuilder::new()
            .ensure_eol(false)
            .replacement(Some(b"$0".to_vec()))
            .build(NoColor::new(vec![]));
        let mut sink = printer.sink(&matcher);
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
            .unwrap();
        assert_eq_printed!(SHERLOCK, printer_contents(&mut printer));
    }

    #[test]
    fn no_ensure_eol_only_matching() {
        let matcher = RegexMatcher::new("Doctor Watsons|Sherlock").unwrap();
        let mut printer = StandardBuilder::new()
            .only_matching(true)
            .ensure_eol(false)
            .column(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "1:9:Doctor Watsons1:57:Sherlock3:49:Sherlock";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn no_ensure_eol_heading() {
        let matcher = RegexMatcher::new("Watson").unwrap();
        let mut printer = StandardBuilder::new()
            .heading(true)
            .ensure_eol(false)
            .only_matching(true)
            .build(NoColor::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let matcher = RegexMatcher::new("Sherlock").unwrap();
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink_with_path(&matcher, "sherlock"),
            )
            .unwrap();

        let got = printer_contents(&mut printer);
        let expected = "\
sherlock
WatsonWatsonsherlock
SherlockSherlock";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn no_ensure_eol_ansi() {
        let no_eol = SHERLOCK.trim_end();
        assert_eq!('.', no_eol.chars().last().unwrap());
        let matcher = RegexMatcher::new("and").unwrap();
        let mut printer = StandardBuilder::new()
            .color_specs(ColorSpecs::default_with_color())
            .ensure_eol(false)
            .build(Ansi::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(&matcher, no_eol.as_bytes(), printer.sink(&matcher))
            .unwrap();

        let got = printer_contents_ansi(&mut printer);
        let expected = "\
but Doctor Watson has to have it taken out for him \x1b[0m\x1b[1m\x1b[31mand\x1b[0m dusted,
\x1b[0m\x1b[1m\x1b[31mand\x1b[0m exhibited clearly, with a label attached.";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn no_ensure_eol_max_columns() {
        let matcher = RegexMatcher::new("exhibited|dusted").unwrap();
        let mut printer = StandardBuilder::new()
            .color_specs(ColorSpecs::default_with_color())
            .max_columns(Some(46))
            .max_columns_preview(true)
            .ensure_eol(false)
            .build(Ansi::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents_ansi(&mut printer);
        let expected = "\
but Doctor Watson has to have it taken out for [... 1 more match]
and \x1b[0m\x1b[1m\x1b[31mexhibited\x1b[0m clearly, with a label attached.";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn no_ensure_eol_multi_line_ansi() {
        let matcher = RegexMatcher::new("(?s)dusted.+attached").unwrap();
        let mut printer = StandardBuilder::new()
            .color_specs(ColorSpecs::default_with_color())
            .ensure_eol(false)
            .build(Ansi::new(vec![]));
        SearcherBuilder::new()
            .line_number(false)
            .multi_line(true)
            .build()
            .search_reader(
                &matcher,
                SHERLOCK.as_bytes(),
                printer.sink(&matcher),
            )
            .unwrap();

        let got = printer_contents_ansi(&mut printer);
        let expected = "\
but Doctor Watson has to have it taken out for him and \x1b[0m\x1b[1m\x1b[31mdusted,
\x1b[0m\x1b[0m\x1b[1m\x1b[31mand exhibited clearly, with a label attached\x1b[0m.";
        assert_eq_printed!(expected, got);
    }

    #[test]
    fn passthru_only_matching() {
        let matcher = RegexMatcher::new(r"Doctor Watson").unwrap();
        let mut printer =
            StandardBuilder::new().stats(true).build_no_color(vec![]);
        let mut sink = printer.sink(&matcher);
        let not_sherlock = "Some text about Dr Watson";
        let mut searcher = SearcherBuilder::new()
            .line_number(true)
            .before_context(usize::MAX)
            .after_context(usize::MAX)
            .build();

        searcher
            .search_reader(&matcher, not_sherlock.as_bytes(), &mut sink)
            .unwrap();
        assert_eq!(sink.has_match(), false);

        searcher
            .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
            .unwrap();
        assert_eq!(sink.has_match(), true);

        let stats = sink.stats().unwrap().clone();
        let got = printer_contents(&mut printer);
        let expected = "\
1:For the Doctor Watsons of this world, as opposed to the Sherlock
2-Holmeses, success in the province of detective work must always
3-be, to a very large extent, the result of luck. Sherlock Holmes
4-can extract a clew from a wisp of straw or a flake of cigar ash;
5:but Doctor Watson has to have it taken out for him and dusted,
6-and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);

        assert_eq!(stats.searches(), 2);
        assert_eq!(stats.searches_with_match(), 1);
        assert_eq!(
            stats.bytes_searched(),
            (SHERLOCK.len() + not_sherlock.len()) as u64
        );
        assert_eq!(stats.bytes_printed(), got.len() as u64);
        assert_eq!(stats.matched_lines(), 2);
        assert_eq!(stats.matches(), 2);
    }

    #[test]
    fn passthru_only_matching_invert() {
        let matcher = RegexMatcher::new(r"Doctor Watson").unwrap();
        let mut printer =
            StandardBuilder::new().stats(true).build(NoColor::new(vec![]));
        let mut sink = printer.sink(&matcher);
        let not_sherlock = "Some text about Dr Watson";
        let mut searcher = SearcherBuilder::new()
            .line_number(true)
            .before_context(usize::MAX)
            .after_context(usize::MAX)
            .invert_match(true)
            .build();

        searcher
            .search_reader(&matcher, not_sherlock.as_bytes(), &mut sink)
            .unwrap();
        assert_eq!(sink.has_match(), true);

        searcher
            .search_reader(&matcher, SHERLOCK.as_bytes(), &mut sink)
            .unwrap();
        assert_eq!(sink.has_match(), true);

        let stats = sink.stats().unwrap().clone();
        let got = printer_contents(&mut printer);
        let expected = "\
1:Some text about Dr Watson
1-For the Doctor Watsons of this world, as opposed to the Sherlock
2:Holmeses, success in the province of detective work must always
3:be, to a very large extent, the result of luck. Sherlock Holmes
4:can extract a clew from a wisp of straw or a flake of cigar ash;
5-but Doctor Watson has to have it taken out for him and dusted,
6:and exhibited clearly, with a label attached.
";
        assert_eq_printed!(expected, got);

        assert_eq!(stats.searches(), 2);
        assert_eq!(stats.searches_with_match(), 2);
        assert_eq!(
            stats.bytes_searched(),
            (SHERLOCK.len() + not_sherlock.len()) as u64
        );
        assert_eq!(stats.bytes_printed(), got.len() as u64);
        assert_eq!(stats.matched_lines(), 5);
        assert_eq!(stats.matches(), 0);
    }

    #[test]
    fn strict_no_binary() {
        let matcher = RegexMatcher::new(".").unwrap();
        for strict_no_binary in [true, false] {
            let binary = if strict_no_binary {
                grep_searcher::BinaryDetection::strict_quit(b'\x00')
            } else {
                grep_searcher::BinaryDetection::quit(b'\x00')
            };
            let searcher = SearcherBuilder::new()
                .line_number(false)
                .binary_detection(binary)
                .build();

            for max_matches in [None, Some(1)] {
                let mut printer = StandardBuilder::new()
                    .stats(true)
                    .max_matches(max_matches)
                    .build(NoColor::new(vec![]));
                let mut sink = printer.sink(&matcher);
                let mut searcher = searcher.clone();
                searcher
                    .search_reader(&matcher, &b"X\n\n\x00Y"[..], &mut sink)
                    .unwrap();
                if strict_no_binary {
                    assert_eq!(sink.binary_byte_offset(), Some(3));
                    assert_eq!(sink.has_match(), false);
                } else if max_matches.is_some() {
                    assert_eq!(sink.binary_byte_offset(), None);
                    assert_eq!(sink.has_match(), true);
                } else {
                    assert_eq!(sink.binary_byte_offset(), Some(3));
                    assert_eq!(sink.has_match(), true);
                }

                searcher
                    .search_reader(&matcher, &b"Z\n\n"[..], &mut sink)
                    .unwrap();
                assert_eq!(sink.binary_byte_offset(), None);
                assert_eq!(sink.has_match(), true);

                let stats = sink.stats().unwrap().clone();
                let got = printer_contents(&mut printer);
                let expected = if strict_no_binary {
                    r#"Z
"#
                } else if max_matches.is_some() {
                    r#"X
Z
"#
                } else {
                    r#"X
WARNING: stopped searching binary file after match (found "\0" byte around offset 3)
Z
"#
                };
                assert_eq_printed!(expected, got);
                assert_eq!(stats.bytes_printed(), got.len() as u64);
                assert_eq!(stats.searches(), 2);
                if strict_no_binary {
                    assert_eq!(stats.searches_with_match(), 1);
                    assert_eq!(
                        stats.bytes_searched(),
                        (b"X\n\n".len() + b"Z\n\n".len()) as u64
                    );
                    assert_eq!(stats.matched_lines(), 1);
                    assert_eq!(stats.matches(), 1);
                } else {
                    assert_eq!(stats.searches_with_match(), 2);
                    assert_eq!(
                        stats.bytes_searched(),
                        if max_matches.is_some() {
                            0 // It probably shouldn't be zero, but that's what it's returning
                        } else {
                            (b"X\n\n".len() + b"Z\n\n".len()) as u64
                        }
                    );
                    assert_eq!(stats.matched_lines(), 2);
                    assert_eq!(stats.matches(), 2);
                }
            }
        }
    }
}
