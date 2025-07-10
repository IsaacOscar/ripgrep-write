use grep::printer::WritePath;
use std::{
    fs::{self, File},
    io::{self, BufWriter, Error, Result, Write},
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    process::{Child, ChildStdin, Command, Stdio},
};
use tempfile::{NamedTempFile, TempPath};
use termcolor::{Ansi, NoColor, WriteColor};

//////////////////////////////////////////////////////////////////
pub(crate) struct Output<'a, W: WriteColor> {
    pub(crate) stdout: W, // This is needed for printing stats, even if the main output goes somewhere else
    postprocessor: Option<&'a Path>, // If present, output will be piped to invocations of this command
    output_directory: Option<&'a Path>, // If present, the directory to write output files to, could be the empty string
    // Note these two do NOT affect output to stdout
    buffer: bool, // Wether to block buffer any output to a postprocessor or file
    color: bool,  // Wether to color any output to a postprocessor or file

    // The following are all initially None
    path: Option<PathBuf>, // Path of the current file we are outputing data for (only needed if we are outputing to a postprocessor or file)
    temporary_file: Option<TempPath>, // Path to a temporary file, needed when we are outputing data to the same file as we are searching
    file_writer: Option<ColorWriter<BufferWriter<File>>>, // Writer for the file we are currently writing to (if any)
    child: Option<Child>, // Handle to current postprocessor process
    child_stdin: Option<ColorWriter<BufferWriter<ChildStdin>>>, // Writer for the postprocessor's stdin pipe
}
impl<'a, W: WriteColor> std::fmt::Debug for Output<'a, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output_path = match self {
            Output { temporary_file: Some(t), .. } => Some(format!(
                "{} ({})",
                self.output_file_path().display(),
                t.display()
            )),
            Output { output_directory: Some(_), path: Some(_), .. } => {
                Some(format!("{}", self.output_file_path().display()))
            }
            Output { output_directory: Some(dir), .. } => {
                Some(format!("{}/???", dir.display()))
            } // Output::set_path hasn't been called yet
            Output { .. } => None,
        };
        match &self {
            Output {
                postprocessor: Some(_),
                output_directory: Some(_),
                ..
            } => write!(
                f,
                "|{:?} >{:?}",
                self.postprocessor.unwrap().display(),
                output_path.unwrap()
            ),
            Output { postprocessor: Some(_), .. } => {
                write!(f, "|{:?}", self.postprocessor.unwrap().display())
            }
            Output { output_directory: Some(_), .. } => {
                write!(f, "{}", output_path.unwrap())
            }
            Output { stdout, .. } => write!(f, "{}", dbg(stdout)),
        }
    }
}
fn dbg<T>(_: &T) -> String {
    format!("<{}>", std::any::type_name::<T>())
}
#[cfg(debug_assertions)]
impl<'a, W: WriteColor> Drop for Output<'a, W> {
    fn drop(&mut self) {
        match self {
            // These need to be closed properly, I don't want to rely on their drop implementations however as that won't print any error messages
            // So they are explicitly closed in Output::save
            Output{file_writer: Some(_), ..} => panic!("File output hasn't been saved (did you forget to call Output::save or Output::cleanup?)"),
            Output{temporary_file: Some(_), ..} => panic!("Temporary file not destroyed (did you forget to call Output::save or Output::cleanup?)"),
            Output{child: Some(_), ..} => panic!("Postprocessor output hasn't been handled yet (did you forget to call Output::save or Output::cleanup?)"),
            Output{child_stdin: Some(_), ..} => panic!("Postprocessor stdin hasn't been closed yet (did you forget to call Output::save or Output::cleanup?)"),
            Output{..} => {}
        }
    }
}

// A wrapper over the given writer, may or may not colour output
enum ColorWriter<W: Write> {
    Uncolored(NoColor<W>),
    Colored(Ansi<W>),
}
// Usage: match_color_writer!(color_writer, |wtr| expression), which evaluates expression with the wtr variable set appropriately
// This is a macro, and not a method as wtr can have different types (&NoColor<W>, &Ansi<W>, or their &mut variants)
// A method 'match_color<F: FnOnce(&dyn WriteColor) -> R>(cw: &ColorWriter, f: F) -> R'
// Wouldn't be as usefull for two reasons: it dosn't work if f needs a &mut reference to the underyling writer
// and it doesn't work when trying to call methods that are common to both NoColor and Ansi, but aren't in a trait (e.g. get_ref)
macro_rules! match_color_writer {
    ($color_writer:expr, |$wtr:ident| $body:expr) => {
        match $color_writer {
            ColorWriter::Uncolored($wtr) => $body,
            ColorWriter::Colored($wtr) => $body,
        }
    };
}

// A wrapper over the given writer, which may or may not buffer
enum BufferWriter<W: Write> {
    Unbuffered(W),
    Buffered(BufWriter<W>),
}
// Usage: match_buffer_writer!(buffer_writer, |wtr| expression) is like match_color_writer!, but where wtr is a W or BufWriter<W>
macro_rules! match_buffer_writer {
    ($buffer_writer:expr, |$wtr:ident| $body:expr) => {
        match $buffer_writer {
            BufferWriter::Unbuffered($wtr) => $body,
            BufferWriter::Buffered($wtr) => $body,
        }
    };
}
// Usage: match_output_writer!(output, |wtr| expression) is like match_color_writer! and match_buffer_writer!
// But wtr could be a W, NoColor<BufferWriter<File>>, Ansi<BufferWriter<File>>, NoColor<BufferWriter<ChildStdin>>, or Ansi<BufferWriter<ChildStdin>>
// In particular, if we are postprocessing the wtr will be that of output.child_stdin, if we are outputing to a file (and not postprocessing)
// it will be from output.file_writer, otherwise it will be output.stdout.
macro_rules! match_output_writer {
    ($output:expr, |$wtr:ident| $body:expr) => {
        match $output {
            Output { child_stdin: Some(color_writer), .. } => {
                match_color_writer!(color_writer, |$wtr| $body)
            }
            Output { file_writer: Some(color_writer), .. } => {
                match_color_writer!(color_writer, |$wtr| $body)
            }
            Output { stdout: $wtr, .. } => $body,
        }
    };
}

// So we can go *color_writer  to get the underlying writer
impl<W: Write> Deref for ColorWriter<W> {
    type Target = W;
    #[inline(always)]
    fn deref(&self) -> &W {
        match_color_writer!(self, |writer| writer.get_ref())
    }
}
impl<W: Write> DerefMut for ColorWriter<W> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut W {
        match_color_writer!(self, |writer| writer.get_mut())
    }
}

impl<W: Write> Deref for BufferWriter<W> {
    type Target = W;
    #[inline(always)]
    fn deref(&self) -> &W {
        // Can't use match_buffer_writer! here as the two branches have different code
        match self {
            BufferWriter::Unbuffered(writer) => writer,
            BufferWriter::Buffered(buffer) => buffer.get_ref(),
        }
    }
}
impl<W: Write> DerefMut for BufferWriter<W> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut W {
        match self {
            BufferWriter::Unbuffered(writer) => writer,
            BufferWriter::Buffered(buffer) => buffer.get_mut(),
        }
    }
}

// other_error!("format", args...) is just like format!, but ereturns an 'other' io::Error
// Note that unlike format!, attempting to use a {variable} format specifier gives an error:
//   to avoid ambiguity, `format_args!` cannot capture variables when the format string is expanded from a macro
macro_rules! other_error {
    ($($args:expr),+) => { Error::other(format!($($args),+)) }
}
// wrap_error!(err, "format", args...) returns an error of the same kind as err
// but with the additional information given by format!("format", args...)
macro_rules! wrap_error {
    ($err:expr, $fmt:literal $(,$args:expr)*) => {
        { let err = $err; Error::new(err.kind(), format!(concat!($fmt, ": {}") $(,$args)*, err)) }
    }
}

// trace_path!(self, "format", args..) is like log::trace!("format", args..) but it also prints the path of the file we are working on
// Note: this will panic if self.path hasn't been set yet
macro_rules! trace_output {
    ($output:expr, $fmt:literal $(,$args:expr),*) => {
        log::trace!(concat!("{}: ", $fmt), $output.path.as_ref().unwrap().display() $(,$args)*) }
}
impl<'a, W: WriteColor> Output<'a, W> {
    pub(crate) fn new(
        stdout: W,
        postprocessor: Option<&'a Path>,
        output_directory: Option<&'a Path>,
        buffer: bool,
        color: bool,
    ) -> Output<'a, W> {
        Output {
            stdout,
            postprocessor,
            output_directory,
            buffer,
            color,
            // Depending on if postprocessor and/or output_directory is None, these will be set on each call to Output::set_path
            path: None,
            temporary_file: None,
            file_writer: None,
            child: None,
            child_stdin: None,
        }
    }

    // Cleanup any open resources now (rather than wait till later)
    pub(crate) fn cleanup(&mut self) {
        self.temporary_file.take();
        self.file_writer.take();
        self.child.take();
        self.child_stdin.take();
    }

    // Main body for <Output as WritePath>::set_path
    #[inline(always)]
    fn do_set_path(&mut self, path: &Path) -> Result<()> {
        self.path = Some(path.to_owned());
        // Do we need to output to a file?
        let mut file = if let Some(output_directory) = self.output_directory {
            self.save()?; // Commit results for previous path (if any)
            let output_path = self.output_file_path();
            if output_path == path {
                if !output_directory.as_os_str().is_empty() {
                    // If we don't throw an error hear, the searched file will definitely be overwritten
                    // which is not what write-to is supposed to do! (Unless you deliberatly passed the empty string to write-to)
                    return Err(other_error!("Searching absolute path {} with -O/--write-to is not supported.s", path.display()));
                }
                let (file, temporary) = NamedTempFile::new()
                    .map_err(|err| {
                        wrap_error!(err, "failed to create temporary file")
                    })?
                    .into_parts();
                trace_output!(
                    self,
                    "creating temporary file: {}",
                    temporary.display()
                );
                self.temporary_file = Some(temporary);
                Some(file)
            } else {
                // Of course the paths may still be the same here, but I'll blame that on the user
                trace_output!(self, "creating output file and any needed parent directories: {}", output_path.display());
                if let Some(prefix) = output_path.parent() {
                    fs::create_dir_all(prefix).map_err(|err| {
                        wrap_error!(
                            err,
                            "failed to create directories: {}",
                            prefix.display()
                        )
                    })?;
                }
                Some(File::create(output_path.clone()).map_err(|err| // Clone needed so we can print it in the error handler
                    wrap_error!(err, "failed to create output file: {}", output_path.display()))?)
            }
        } else {
            None
        };

        // Are we passing output to a postprocessor? (this needs to be done after the file's above are created/opened)
        if let Some(postprocessor) = self.postprocessor {
            trace_output!(self, "spawning '{:?}'", postprocessor);
            let stdout = if let Some(file) = file.take() {
                // Take the file as we won't need it anymore (the child process will be responsible for it)
                // Redirect the postprocessor's stdout to the file we are writing to if present
                Stdio::from(file)
            } else {
                // Otherwise, just use the current process's stdout
                Stdio::inherit()
            };
            // Spawn the postprocessor, passing the current path as an argument, and a new pipe for stdin
            // Stderr will be inheri
            let mut command = Command::new(postprocessor);
            let mut child = command
                .arg(path)
                .stdin(Stdio::piped())
                .stdout(stdout)
                .stderr(Stdio::inherit())
                .spawn()
                .map_err(|err| {
                    wrap_error!(
                        err,
                        "postprocessor command could not start: '{:?}'",
                        command
                    )
                })?;

            self.child_stdin =
                Some(self.wrap_writer(child.stdin.take().unwrap()));
            self.child = Some(child);
        }

        // This needs to be done after both the file is created, and after was possibly taken for the postprocessor
        if let Some(file) = file {
            self.file_writer = Some(self.wrap_writer(file));
        }
        Ok(())
    }

    // Wait for any postprocessing step to finish, and save any files we were writing to
    pub(crate) fn save(&mut self) -> Result<()> {
        // Make sure cleanup occurs if an error
        // (needs to be a seperate method to do_save due to the rules for the ? operator)
        self.do_save().map_err(|err| {
            self.cleanup();
            err
        })
    }
    #[inline(always)]
    fn do_save(&mut self) -> Result<()> {
        // Are we running a postprocessor?
        if let Some(mut child) = self.child.take() {
            // Ensure all output is written (DO NOT call child_stdin.flush(), as that won't work if we're buffering through a BufWriter

            let flush_error = self.flush();
            self.child_stdin.take(); // Make sure stdin pipe is dropped, so that it closes properly, even if an error occured while flushing
            match flush_error {
                // Ignore broken pipe errors as that's probably just the child process closing the pipe
                Err(err) if err.kind() != std::io::ErrorKind::BrokenPipe => {
                    return Err(wrap_error!(
                        err,
                        "failed to flush postprocessor stdin: '{:?}'",
                        self.postprocessor.unwrap().display()
                    ));
                }
                _ => {}
            };
            trace_output!(
                self,
                "waiting for postprocessor to finish: {}",
                self.postprocessor.unwrap().display()
            );
            let exit_status = child.wait().map_err(|err| {
                wrap_error!(
                    err,
                    "failed to wait on postprocessor command: '{:?}'",
                    self.postprocessor.unwrap()
                )
            })?;
            if !exit_status.success() {
                return Err(other_error!("postprocessor command '{:?}' returned exit status: {exit_status}", self.postprocessor.unwrap()));
            }
        }
        // Have we been writing to a file?
        if self.file_writer.is_some() {
            // Can't take or borrow self.file_writer here as it's needed by the self.flush call
            // Ensure all output is written, and the file is closed properly
            self.flush()
                .and_then(|_| self.file_writer.take().unwrap().sync_all())
                .map_err(|err| {
                    wrap_error!(
                        err,
                        "failed to close file: {}",
                        self.file_writer_path().display()
                    )
                })?;
        }
        // Have we or the postprocessor been writing to a temporary file? (must be done after we've watten for any postprocessor to finish)
        if let Some(temporary_file) = self.temporary_file.take() {
            // (the file itself will have been flushed/closed in the above if)
            trace_output!(
                self,
                "replacing with temporary file: {}",
                temporary_file.display()
            );
            let target = self.output_file_path();

            // Try rename first as it's faster, but it can fail if the files are on different filesystems
            // (On linux this does preserve the permisions of the target file)
            if let Err(_) = fs::rename(&temporary_file, &target) {
                // Target probably already exists, but just in case it's been deleted this will create it
                let mut target = File::create(target).map_err(|err|
                    // No need to display target as it will equal the path of the searched file, which is added to the message by main::search
                    wrap_error!(err, "failed to open file for writing"))?;
                // Open back the temporary_file (don't try and reuse self.file_writer as that was opened in write mode
                // and may be None if we are using a postprocessor)
                // (the & is needed to prevent temporary_file from being moved)
                let mut source = File::open(&temporary_file).map_err(|err|
                    // No need to display target as it will equal the path of the searched file, which is added to the message by main::search
                    wrap_error!(err, "failed to open temporary file: {}", temporary_file.display()))?;

                // Do NOT use fs::copy as that will modify the permissions of target
                // On *nix fs::copy internally calls io::copy anyway, so this this shouldn't be any slower
                io::copy(&mut source, &mut target).map_err(|err| {
                    wrap_error!(
                        err,
                        "failed to copy over temporary file: {}",
                        temporary_file.display()
                    )
                })?;

                // The temporary file will be automatically destroyed when temporary_file is dropped
                // but calling close explicitly allows us to get any error messages produced
                // (don't call close if the rename succeeded however as the file will already be deleted)
                let temporary_path = temporary_file.to_path_buf(); // Needed as calling close destroys temporary_file
                temporary_file.close().map_err(|err| {
                    wrap_error!(
                        err,
                        "failed to destroy temporary file: {}",
                        temporary_path.display()
                    )
                })?;
            };
        }
        Ok(())
    }

    // Wrap the given writer into the appropriate ColorWriter and BufferWriter
    #[inline(always)]
    fn wrap_writer<WW: Write>(
        &mut self,
        writer: WW,
    ) -> ColorWriter<BufferWriter<WW>> {
        let buffer_writer = if self.buffer {
            BufferWriter::Buffered(BufWriter::new(writer))
        } else {
            BufferWriter::Unbuffered(writer)
        };
        if self.color {
            ColorWriter::Colored(Ansi::new(buffer_writer))
        } else {
            ColorWriter::Uncolored(NoColor::new(buffer_writer))
        }
    }

    // The file path the file_writer is writing to
    #[inline(always)]
    fn file_writer_path(&self) -> PathBuf {
        match self {
            Output { temporary_file: Some(temporary_file), .. } => {
                temporary_file.to_path_buf()
            }
            Output { .. } => self.output_file_path(),
        }
    }

    // The path we will write any final data to (not counting any intermediary temporary file)
    #[inline(always)]
    fn output_file_path(&self) -> PathBuf {
        match self {
            Output{output_directory: Some(output_directory), path: Some(path), ..} => output_directory.join(path),
            _ => unreachable!("Output::output_file_path called when we are not outputing to a file")
        }
    }
}
impl<'a, W: WriteColor + Clone> Clone for Output<'a, W> {
    fn clone(&self) -> Output<'a, W> {
        // Only allow cloning if Output::set_path hasn't been called yet
        // Otherwise, we may have data that doesn't make sense to clone (specifically, File's, TempPath's, and Child's)
        debug_assert!(
            self.path.is_none()
                && self.temporary_file.is_none()
                && self.file_writer.is_none()
                && self.child.is_none()
                && self.child_stdin.is_none(),
            "Output::clone cannot be called after Output::set_path"
        );
        Output::new(
            self.stdout.clone(),
            self.postprocessor.clone(),
            self.output_directory.clone(),
            self.buffer,
            self.color,
        )
    }
}
impl<'a, W: WriteColor> WritePath for Output<'a, W> {
    fn set_path(&mut self, path: &Path) -> Result<()> {
        self.do_set_path(path).map_err(|err| {
            // cleanup on error
            self.cleanup();
            err
        })
    }
}

// Implementations for Write and WriteColor for Output which just forward via match_output_writer
// to the underlying writer (self.child_stdin, or self.file_writer if that is None, or self.stdout if both are None)
impl<'a, W: WriteColor> Write for Output<'a, W> {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        //      let selfy = format!("{:?}", self);
        match_output_writer!(self, |wtr| {
            //          eprintln!("{}: {} Writing {:?}", selfy, dbg(wtr), str::from_utf8(buf).unwrap());
            wtr.write(buf)
        })
        .map_err(|err| {
            self.cleanup();
            err
        })
    }
    #[inline(always)]
    fn flush(&mut self) -> Result<()> {
        //      let selfy = format!("{:?}", self);
        match_output_writer!(self, |wtr| {
            //          eprintln!("{}: {} flushing", selfy, dbg(wtr));
            wtr.flush()
        })
        .map_err(|err| {
            self.cleanup();
            err
        })
    }
    //  #[inline(always)] fn write_all(&mut self, buf: &[u8]) -> Result<()> { match_output_writer!(self, |wtr| wtr.write_all(buf)) }
    //  #[inline(always)] fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> Result<()>  { match_output_writer!(self, |wtr| wtr.write_fmt(args)) }
}
impl<'a, W: WriteColor> WriteColor for Output<'a, W> {
    #[inline(always)]
    fn supports_color(&self) -> bool {
        match_output_writer!(self, |wtr| wtr.supports_color())
    }
    #[inline(always)]
    fn supports_hyperlinks(&self) -> bool {
        match_output_writer!(self, |wtr| wtr.supports_hyperlinks())
    }
    #[inline(always)]
    fn set_color(&mut self, color: &termcolor::ColorSpec) -> Result<()> {
        match_output_writer!(self, |wtr| wtr.set_color(color)).map_err(|err| {
            self.cleanup();
            err
        })
    }
    #[inline(always)]
    fn set_hyperlink(
        &mut self,
        color: &termcolor::HyperlinkSpec,
    ) -> Result<()> {
        match_output_writer!(self, |wtr| wtr.set_hyperlink(color)).map_err(
            |err| {
                self.cleanup();
                err
            },
        )
    }
    #[inline(always)]
    fn reset(&mut self) -> Result<()> {
        match_output_writer!(self, |wtr| wtr.reset()).map_err(|err| {
            self.cleanup();
            err
        })
    }
    #[inline(always)]
    fn is_synchronous(&self) -> bool {
        match_output_writer!(self, |wtr| wtr.is_synchronous())
    }
}

// Write needs to be implemented for BufferWriter as it will get wrapped by Ansi or NoColor
impl<W: Write> Write for BufferWriter<W> {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        match_buffer_writer!(self, |wtr| wtr.write(buf))
    }
    #[inline(always)]
    fn flush(&mut self) -> Result<()> {
        match_buffer_writer!(self, |wtr| wtr.flush())
    }
    #[inline(always)]
    fn write_all(&mut self, buf: &[u8]) -> Result<()> {
        match_buffer_writer!(self, |wtr| wtr.write_all(buf))
    }
    #[inline(always)]
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> Result<()> {
        match_buffer_writer!(self, |wtr| wtr.write_fmt(args))
    }
}
