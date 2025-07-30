use crate::counter::CounterWriter;
use std::{
    fs::{self, File},
    io::{BufWriter, Error, Result, Write},
    mem,
    path::{Path, PathBuf},
};
use termcolor::{Ansi, Buffer, NoColor, WriteColor};

#[derive(Debug, Clone)]
pub(crate) struct MultiWriter<W: WriteColor> {
    stdout: CounterWriter<W>,
    current: WriteTo,
    cancellable: bool,
    output_directory: Option<PathBuf>, // If we are outputting to a file somewhere
    buffer_files: bool,
    input_path: Option<PathBuf>, // Path of the current file we are searching (if any)
    output_path: Option<PathBuf>, // Path of the current file we will be writing to (if any)
}

#[derive(Debug)]
enum WriteTo {
    Stdout,
    Nowhere,
    Buffer(Buffer),
    File(FileWriter),
    Uninitialized,
}
impl Clone for WriteTo {
    fn clone(&self) -> Self {
        match self {
            WriteTo::Uninitialized => WriteTo::Uninitialized,
            // I could do a clone in the non-File cases, but I don't think it's necessary
            _ => panic!("Cannot clone in-use MultiWriter"),
        }
    }
}

// map_error!(result, "format", args...) alters any error returned by result
// with additional information given by format!("format", args...)
// (the path of the currently input file is always printed by std::core::main::search and main::search_parallel,
// so this needn't be included in the given args).
macro_rules! map_error {
    ($result:expr, $fmt:literal $(,$args:expr)*) => {
        $result.map_err(|err| Error::new(err.kind(), format!(concat!($fmt, ": {}") $(,$args)*, err)))
    };
}

// Fun
macro_rules! other_error {
    ($fmt:literal $(,$args:expr)*) => {
        Err(Error::other(format!($fmt $(,$args)*)))
    };
}

// trace_with_path!(file_paths, "format", args...) is like log::trace!
// but prints the path stored in object (if any)
macro_rules! trace_with_path {
    ($file_paths:expr, $fmt:literal $(,$args:expr)*) => {
        match $file_paths.input_path() {
            Some(p) => log::trace!(concat!("{}: ", $fmt), p.display() $(,$args)*),
            None => log::trace!($fmt $(,$args)*),
        }
    }
}

// For use with trace_with_path!
trait FilePaths {
    /// The path of the file we are searching
    fn input_path(&self) -> Option<&PathBuf>;
    /// The path of the file we will be writing to
    fn output_path(&self) -> Option<&PathBuf>;

    fn make_output_directories(&self) -> Result<()> {
        if let Some(prefix) = self.output_path().unwrap().parent() {
            trace_with_path!(
                self,
                "ensuring directories exist: {}",
                prefix.display()
            );
            map_error!(
                fs::create_dir_all(prefix),
                "failed to create directories: {}",
                prefix.display()
            )
        } else {
            Ok(())
        }
    }

    // Check that the input and output paths appear to be different files
    // (it's not perfect, but should catch most common cases)
    // (assumes make_output_directories has allready been called)
    fn check_non_overwriting(&self) -> Result<()> {
        let output_path = self.output_path().unwrap();
        let output_path = if output_path.exists() {
            // It's an already existing file, canonicalize the path for that
            map_error!(
                output_path.canonicalize(),
                "failed to canonicalize output file path: {}",
                output_path.display()
            )?
        } else {
            // Otherwise, the directory should exist, so canonicalize that
            let dirname = output_path.parent().map_or(
                other_error!(
                    "output file path has no parent: {}",
                    output_path.display()
                ),
                Result::Ok,
            )?;
            let basename = output_path.file_name().map_or(
                other_error!(
                    "output file path has no file name: {}",
                    output_path.display()
                ),
                Result::Ok,
            )?;
            let dirname = map_error!(
                dirname.canonicalize(),
                "failed to canonicalize output folder: {}",
                dirname.display()
            )?;
            dirname.join(basename)
        };
        let input_path = map_error!(
            self.input_path().unwrap().canonicalize(),
            "failed to canonicalize input file path"
        )?;

        if input_path == output_path {
            other_error!(
                "Refusing to overwrite input file in -O/--write-to mode: {}",
                input_path.display()
            )
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
enum FileBuffer {
    Buffered(BufWriter<File>),
    Unbuffered(File),
}
impl FileBuffer {
    pub fn new(file: File, buffer: bool) -> FileBuffer {
        if buffer {
            FileBuffer::Buffered(BufWriter::new(file))
        } else {
            FileBuffer::Unbuffered(file)
        }
    }
    // Calling this will flush the bufwriter (if any), and thus may give an error
    pub fn into_inner(self) -> Result<File> {
        match self {
            FileBuffer::Buffered(buffer) =>
            // Extract out the inner IO error, if any
            {
                buffer.into_inner().map_err(|err| err.into_error())
            }
            FileBuffer::Unbuffered(file) => Ok(file),
        }
    }
}

/// `map_file_buffer!(file_buffer, |pat| body)` evaluates `body`, but where `pat` is bound
/// to the underlying writer (either a BufWriter<...> or File)
/// This is similar to either::for_both
macro_rules! map_file_buffer {
    ($file_buffer:expr, |$wtr:ident| $body:expr) => {
        match $file_buffer {
            FileBuffer::Buffered($wtr) => $body,
            FileBuffer::Unbuffered($wtr) => $body,
        }
    };
}

#[derive(Debug)]
/// Like a file, but it only opens if you try to write data to it
/// (Note: writing a zero-byte array counts as writing)
struct LazyFile {
    input_path: PathBuf, // Use only for messages
    output_path: PathBuf,
    buffer: bool,
    state: FileState,
}
#[derive(Debug)]
enum FileState {
    Unopened,
    Opened(FileBuffer),
    Closed,
}
impl LazyFile {
    pub fn new(
        input_path: PathBuf,
        output_path: PathBuf,
        buffer: bool,
    ) -> LazyFile {
        LazyFile {
            input_path,
            output_path,
            buffer,
            state: FileState::Unopened,
        }
    }

    #[inline]
    /// Open the file for writing (if it isn't already open)
    fn open(&mut self) -> Result<&mut FileBuffer> {
        if matches!(self.state, FileState::Unopened) {
            // The borrow checker is really unhappy if I try and put this code inside
            // the following match
            self.make_output_directories()?;
            // LazyFile is only used in the --write-to case, so check we aren't overwriting the input file
            self.check_non_overwriting()?;

            trace_with_path!(
                self,
                "opening file for writing: {}",
                self.output_path.display()
            );
            let file = map_error!(
                File::create(&self.output_path),
                "failed to open file for for writing: {}",
                self.output_path.display()
            )?;
            self.state = FileState::Opened(FileBuffer::new(file, self.buffer));
        }
        match &mut self.state {
            FileState::Opened(file_buffer) => Ok(file_buffer),
            FileState::Closed => panic!("File already closed, can't write."),
            FileState::Unopened => unreachable!(),
        }
    }
    pub fn close(mut self) -> Result<()> {
        // Need mem::replace so we can take ownership if the contained file
        match mem::replace(&mut self.state, FileState::Closed) {
            FileState::Closed | FileState::Unopened => Ok(()), // Nothing to do
            FileState::Opened(file_buffer) => {
                trace_with_path!(
                    self,
                    "flushing and closing file: {}",
                    self.output_path.display()
                );
                // No map_error! here, for consistency with errors returned by .write and the like
                let mut file = file_buffer.into_inner()?;
                map_error!(
                    file.flush(),
                    "failed to flush file: {}",
                    self.output_path.display()
                )?;
                // Actually close the file, reporting any errors
                map_error!(
                    file.sync_all(),
                    "failed to sync file: {}",
                    self.output_path.display()
                )
            }
        }
    }
}
impl FilePaths for LazyFile {
    fn input_path(&self) -> Option<&PathBuf> {
        Some(&self.input_path)
    }
    fn output_path(&self) -> Option<&PathBuf> {
        Some(&self.output_path)
    }
}

impl Write for LazyFile {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        map_file_buffer!(self.open()?, |wtr| wtr.write(buf))
    }
    #[inline]
    fn write_vectored(
        &mut self,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Result<usize> {
        map_file_buffer!(self.open()?, |wtr| wtr.write_vectored(bufs))
    }
    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<()> {
        map_file_buffer!(self.open()?, |wtr| wtr.write_all(buf))
    }
    #[inline]
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> Result<()> {
        map_file_buffer!(self.open()?, |wtr| wtr.write_fmt(args))
    }
    #[inline]
    fn flush(&mut self) -> Result<()> {
        map_file_buffer!(self.open()?, |wtr| wtr.flush())
    }
}

#[derive(Debug)] // Not cloneable as it contains a File
enum FileWriter {
    Uncolored(NoColor<CounterWriter<LazyFile>>),
    Colored(Ansi<CounterWriter<LazyFile>>),
}
/// `map_file_writer!(file_writer, |wtr| body)` executes body, where `wtr` is set to the `Ansi``, or `NoColor`` stored in the given `file_writer`.
/// This is a macro, and not a method as `wtr` can have different types (`NoColor<CounterWriter<W>>`, `Ansi<CounterWriter<W>>`, or their `&`, and `&mut` variants)
/// A method `map_file_writer<F: FnOnce(&dyn WriteColor) -> R>(file_writer: &FileWriter, f: F) -> R`
/// Wouldn't be as usefull for two reasons: it dosn't work if `f` needs a `&mut` reference to the underyling writer
/// and it doesn't work when trying to call methods that are common to both `NoColor` and `Ansi`, but aren't in a trait (e.g. `get_ref`)
macro_rules! map_file_writer {
    ($file_writer:expr, |$wtr:ident| $body:expr) => {
        match $file_writer {
            FileWriter::Uncolored($wtr) => $body,
            FileWriter::Colored($wtr) => $body,
        }
    };
}
impl FileWriter {
    fn new(
        input_path: PathBuf,
        output_path: PathBuf,
        color: bool,
        buffer: bool,
    ) -> FileWriter {
        let counter =
            CounterWriter::new(LazyFile::new(input_path, output_path, buffer));
        if color {
            FileWriter::Colored(Ansi::new(counter))
        } else {
            FileWriter::Uncolored(NoColor::new(counter))
        }
    }
    /// Get the count of bytes written to the file
    pub fn count(&self) -> u64 {
        // We never call `reset_count` on CounterWriter, so total_count should always equal count
        debug_assert_eq!(
            map_file_writer!(self, |wtr| wtr.get_ref().total_count()),
            map_file_writer!(self, |wtr| wtr.get_ref().count()),
        );
        map_file_writer!(self, |wtr| wtr.get_ref().count())
    }
    /// Extract out the underlying lazy file
    pub fn into_inner(self) -> LazyFile {
        map_file_writer!(self, |wtr| wtr.into_inner().into_inner())
    }
}

impl<W: WriteColor> FilePaths for MultiWriter<W> {
    fn input_path(&self) -> Option<&PathBuf> {
        self.input_path.as_ref()
    }
    fn output_path(&self) -> Option<&PathBuf> {
        self.output_path.as_ref()
    }
}

impl<W: WriteColor> MultiWriter<W> {
    #[inline]
    pub(crate) fn get_mut(&mut self) -> &mut W {
        self.stdout.get_mut()
    }

    #[inline]
    pub(crate) fn into_inner(self) -> W {
        self.stdout.into_inner()
    }

    // Amount of data that has been written to a file or buffer
    fn other_count(&self) -> u64 {
        match &self.current {
            WriteTo::File(file) => file.count(),
            WriteTo::Buffer(b) => b.len() as u64,
            _ => 0,
        }
    }

    /// Returns the total number of bytes written since construction.
    /// (minus any bytes that werre cancalled via self.cancel)
    pub(crate) fn total_count(&self) -> u64 {
        self.stdout.total_count() + self.other_count()
    }

    /// Returns the total number of bytes written since construction or the last time `begin` was called.
    /// (minus any bytes that werre cancalled via self.cancel)
    pub(crate) fn count(&self) -> u64 {
        self.stdout.count() + self.other_count()
    }

    fn assert_same_color_features<O: WriteColor>(&self, other: &O) {
        // Ensure that other supports the same colour features as self claims to
        debug_assert_eq!(self.supports_color(), other.supports_color());
        debug_assert_eq!(
            self.supports_hyperlinks(),
            other.supports_hyperlinks()
        );
        debug_assert_eq!(self.is_synchronous(), other.is_synchronous());
    }

    pub(crate) fn cancel(&mut self) {
        match self.current {
            WriteTo::Stdout => panic!("Too late to cancel, MultiWriter has already been writing to stdout"),
            WriteTo::Nowhere => {}, // Already cancelled
            WriteTo::Buffer(_) => if self.cancellable {
                // Discard the buffer, and any future writes
                self.current = WriteTo::Nowhere
            } else {
                // This is likely an error, even though I could cancel
                // the same as above
                panic!("MultiWriter was not created as cancellable")
            },
            WriteTo::File(_) => panic!("Too late to cancel, MultiWriter has been writing to a file"),
            WriteTo::Uninitialized => panic!("MultiWriter::begin has not been called yet"),
        }
    }

    // Because an Option<&PathBuf> is easier to use than an &Option<PathBuf>
    fn output_directory(&self) -> Option<&PathBuf> {
        if self.input_path.is_none() {
            // This will occur if the input was <stdin>
            // in which case we pretend there's no output_directory
            // so we don't write output to a file (and just write to stdout instead)
            None
        } else {
            self.output_directory.as_ref()
        }
    }

    // Was --write-to passed on the command line?
    fn is_write_to(&self) -> bool {
        self.output_directory().is_some() && !self.is_write_replace()
    }

    // Was --write-replace passed on the command line?
    fn is_write_replace(&self) -> bool {
        self.output_directory().is_some_and(|path| path.as_os_str().is_empty())
    }

    pub(crate) fn begin(&mut self, path: Option<&Path>) -> Result<()> {
        if !matches!(self.current, WriteTo::Uninitialized) {
            panic!("MultiWriter::finish hasn't been called!")
        }

        self.stdout.reset_count(); // No need to reset any FileWriter counts, as those are set to 0 when created

        // Have to make a copy of the path as it may not live long enough
        // (Do NOT call self.output_directory() here, as that checks that self.path.is_some())
        if self.output_directory.is_some() && path.is_none() {
            panic!("MultiWriter::begin was not provided a path!")
        }

        self.input_path = match path {
            Some(p) if p == Path::new("<stdin>") => None, // Clear the path as it wasn't a real path
            Some(p) => Some(p.to_path_buf()), // Have to make a clone, as lifetimes are too difficuilt
            None => None,
        };

        // Update output_path
        if let Some(directory) = self.output_directory() {
            let output_path = directory.join(self.input_path().expect(
                "MultiWriter::new was passed output_directory, but no path was provided to MultiWriter::begin"));
            if output_path == *self.input_path().unwrap() && self.is_write_to()
            {
                // If we don't throw an error hear, the searched file will definitely be overwritten
                // which is not what write-to is supposed to do!
                // (No need to report the output_path, as ripgrep prints that anyway)
                return other_error!("Searching absolute paths with -O/--write-to is not supported.");
            }
            self.output_path = Some(output_path);
        }

        // We need to buffer if we might cancel before outputing anything
        // or we are going to write to the file we are currently reading to
        if self.cancellable || self.is_write_replace() {
            trace_with_path!(self, "creating in-memory buffer");
            // Make sure we provide the correct color support
            let buffer = if self.supports_color() {
                Buffer::ansi()
            } else {
                Buffer::no_color()
            };
            self.assert_same_color_features(&buffer);
            self.current = WriteTo::Buffer(buffer);
        } else if self.is_write_to() {
            // The file will be created only when we try to write to it
            let file_writer = FileWriter::new(
                // Need clones, so as to prevent self from being borried by the FileWriter
                // (which would prevent mutating self)
                self.input_path().unwrap().clone(),
                self.output_path().unwrap().clone(),
                self.supports_color(),
                self.buffer_files,
            );
            map_file_writer!(&file_writer, |wtr| self
                .assert_same_color_features(wtr));
            self.current = WriteTo::File(file_writer);
        } else {
            // Write directly to stdout, no funny business
            self.current = WriteTo::Stdout;
        }
        Ok(())
    }

    // Must call self.begin before you attempt to write again
    pub(crate) fn finish(&mut self) -> Result<()> {
        // mem::replace is needed so we can obtain ownership of self.current
        let current = mem::replace(&mut self.current, WriteTo::Uninitialized);
        match current {
            WriteTo::Stdout => Ok(()), // We've already been writing to self.stdout
            WriteTo::Nowhere => Ok(()), // Writing has been cancelled, don't try to override this
            WriteTo::Buffer(buffer) => {
                if self.output_directory().is_some() {
                    // Do not write empty output
                    // (this will either be because the file didn't match, or it was empty
                    // ripgrep does not distinguish the two cases, so to be safe, we don't write anything)
                    if buffer.is_empty() {
                        return Ok(());
                    }

                    // We need to write to the file
                    self.make_output_directories()?;
                    if self.is_write_to() {
                        // Even though writing to the input file should work
                        // (as it we will have finished reading from it,
                        // unlike --write-replace, the output of --write-to could be very
                        // different from the input file)
                        self.check_non_overwriting()?;
                    }

                    // Can't actually call self.output_path() here as calling a function borrows the entirety of self
                    // (which this prevents us from mutating self.stdout below)
                    let output_path = self.output_path.as_ref().unwrap();
                    trace_with_path!(
                        self,
                        "writing buffered data to file: {}",
                        output_path.display()
                    );
                    self.stdout.add_count(buffer.len() as u64);

                    // No neet to use FileWriter, as the buffer will have already captured any color sequences
                    map_error!(
                        fs::write(output_path, buffer.into_inner()),
                        "failed to open file for for writing: {}",
                        output_path.display()
                    )
                } else {
                    // Output to stdout (by going through the CounterWriter).
                    self.stdout.write_all(buffer.as_slice())
                }
            }
            WriteTo::File(file) => {
                self.stdout.add_count(file.count()); // Preserve the count
                file.into_inner().close() // Ensure all data is indead written
            }
            WriteTo::Uninitialized => {
                panic!("MultiWriter::begin has not been called yet")
            }
        }
    }

    pub(crate) fn new_with_file_output(
        wtr: W,
        cancellable: bool,
        output_directory: Option<PathBuf>,
        buffer_files: bool,
    ) -> MultiWriter<W> {
        MultiWriter {
            stdout: CounterWriter::new(wtr),
            current: WriteTo::Uninitialized,
            cancellable,
            output_directory,
            buffer_files,
            input_path: None,
            output_path: None,
        }
    }

    pub(crate) fn new(wtr: W, cancellable: bool) -> MultiWriter<W> {
        MultiWriter::new_with_file_output(wtr, cancellable, None, false)
    }
}

/// `map_multi_writer!(multi_writer, |wtr| body, nowhere_body)` is like `map_file_writer!`
/// Except that nowhere_body is used (with no `wtr` variable) if the multi_writer is Writing to Nowhere.
/// (We can't just set `wtr` to std::io::empty(), as that doesn't implement WriteColor, so the nowhere case
/// has to be fully specified)
/// An additional argument, `unitialized_body` can be provided to give the code to perform when unitialised
macro_rules! map_multi_writer {
    ($multi_writer:expr, |$wtr:ident| $body:expr, $nowhere_body:expr) => {
        map_multi_writer!($multi_writer, |$wtr| $body, $nowhere_body, panic!("MultiWriter::begin has not been called yet"))
    };
    ($multi_writer:expr, |$wtr:ident| $body:expr, $nowhere_body:expr, $unitialized_body:expr) => {
        match $multi_writer {
            MultiWriter { current: WriteTo::Stdout, stdout: $wtr, .. } =>
                // Writes will go to stdout (via the CounterWriter wrapper)
                $body,
            MultiWriter { current: WriteTo::Nowhere, .. } =>
                // Writes should simply be no-ops
                $nowhere_body,
            MultiWriter { current: WriteTo::Buffer($wtr), .. } =>
                // Writes will go to the Buffer
                $body,
            MultiWriter { current: WriteTo::File(file_writer), .. } =>
                // Writes will go to the file (via the Ansi/NoColor and CounterWriter wrappes)
                map_file_writer!(file_writer, |$wtr| $body),
            MultiWriter { current: WriteTo::Uninitialized, .. } =>
                // We don't know what to write to yet
                $unitialized_body
        }
    };
}

impl<W: WriteColor> Write for MultiWriter<W> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        map_multi_writer!(self, |wtr| wtr.write(buf), Ok(buf.len()))
    }
    #[inline]
    fn write_vectored(
        &mut self,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Result<usize> {
        map_multi_writer!(
            self,
            |wtr| wtr.write_vectored(bufs),
            Ok(bufs.iter().map(|b| b.len()).sum())
        )
    }
    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<()> {
        map_multi_writer!(self, |wtr| wtr.write_all(buf), Ok(()))
    }
    #[inline]
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> Result<()> {
        map_multi_writer!(self, |wtr| wtr.write_fmt(args), Ok(()))
    }
    #[inline]
    fn flush(&mut self) -> Result<()> {
        map_multi_writer!(self, |wtr| wtr.flush(), Ok(()))
    }
    // Don't forward Write::by_ref as it's return type is &mut Self
}

impl<W: WriteColor> WriteColor for MultiWriter<W> {
    #[inline]
    fn set_color(&mut self, spec: &termcolor::ColorSpec) -> Result<()> {
        map_multi_writer!(self, |wtr| wtr.set_color(spec), Ok(()))
    }

    #[inline]
    fn set_hyperlink(
        &mut self,
        link: &termcolor::HyperlinkSpec,
    ) -> Result<()> {
        map_multi_writer!(self, |wtr| wtr.set_hyperlink(link), Ok(()))
    }

    #[inline]
    fn reset(&mut self) -> Result<()> {
        map_multi_writer!(self, |wtr| wtr.reset(), Ok(()))
    }

    #[inline]
    // The result of this function should never change
    fn supports_color(&self) -> bool {
        // Get whether the target stdout has colour support
        // All the WriteColor implementations in termcolor return a constant value here
        // So we want MultiWriter to do the same (and not have the result depend on self.current)
        self.stdout.supports_color()
    }

    #[inline]
    fn supports_hyperlinks(&self) -> bool {
        self.stdout.supports_hyperlinks()
    }

    #[inline]
    fn is_synchronous(&self) -> bool {
        // Note: unlike supports_color and supports_hyperlinks
        // This may return true, even if it returns false on the current writer
        self.stdout.is_synchronous()
    }
}
