use crate::counter::CounterWriter;
use std::io::{self, Result, Write};
use std::mem;
use termcolor::{Buffer, WriteColor};

#[derive(Debug, Clone)]
pub(crate) struct CancellableWriter<W: WriteColor> {
    target_writer: CounterWriter<W>,
    state: State,
}

// Can't use std::io::Empty as that doesn't implement WriteColor
#[derive(Debug, Clone)]
struct EmptyColor;
impl_write!(_this: EmptyColor => &mut io::empty());
impl WriteColor for EmptyColor {
    fn supports_color(&self) -> bool {
        false
    }
    fn supports_hyperlinks(&self) -> bool {
        false
    }
    fn is_synchronous(&self) -> bool {
        false
    }
    fn set_color(&mut self, _: &termcolor::ColorSpec) -> Result<()> {
        Ok(())
    }
    fn set_hyperlink(
        &mut self,
        _: &termcolor::HyperlinkSpec,
    ) -> ::std::io::Result<()> {
        Ok(())
    }
    fn reset(&mut self) -> Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum State {
    NotCancellable,
    Cancelled(EmptyColor),
    Bufferring(Buffer),
    Commited,
}

impl<W: WriteColor> CancellableWriter<W> {
    #[inline(always)]
    pub(crate) fn get_mut(&mut self) -> &mut W {
        self.target_writer.get_mut()
    }

    #[inline(always)]
    pub(crate) fn into_inner(self) -> W {
        self.target_writer.into_inner()
    }

    #[inline(always)]
    pub(crate) fn count(&self) -> u64 {
        self.target_writer.count()
    }

    #[inline(always)]
    pub(crate) fn has_written(&self) -> bool {
        self.target_writer.total_count() > 0
    }

    pub(crate) fn cancel(&mut self) {
        match self.state {
            State::Bufferring(_) => self.state = State::Cancelled(EmptyColor), // Discard the buffer, and any future writes
            State::Cancelled(_) => {}, // Already cancelled
            State::NotCancellable => panic!("CancellableWriter was not created as cancellable"),
            State::Commited => panic!("Too late to cancel, CancellableWriter::commit has allready been called"),
        }
    }

    fn new_buffer(target: &W) -> Buffer {
        // Assume that self.target_writer's WriteColor implementation works the same as the ones
        // in the termcolor crate.
        if target.supports_color() && target.is_synchronous() {
            assert!(cfg!(windows)); // is_synchronous() only returns true on Windows
            #[cfg(windows)]
            return Buffer::console();
            unreachable!()
        } else if target.supports_color() {
            Buffer::ansi()
        } else {
            // is_synchronous() only returns true if supports_color() does
            assert!(!target.is_synchronous());
            Buffer::no_color()
        }
    }

    pub(crate) fn reset_state(&mut self) {
        match self.state {
            State::NotCancellable => {} // State doesn't change
            State::Bufferring(_) | State::Cancelled(_) | State::Commited => {
                self.state = State::Bufferring(CancellableWriter::new_buffer(
                    &self.target_writer,
                ))
            }
        };
        self.target_writer.reset_count()
    }

    pub(crate) fn commit(&mut self) -> Result<()> {
        match self.state {
            State::Commited | State::NotCancellable => Ok(()), // We've already been writing to self.target_writer
            State::Cancelled(_) => Ok(()), // Writing has been cancelled, don't try to override this
            State::Bufferring(_) => {
                // Can't capture buffer here as the we will modify `self.state``
                // Sets self.state = State::Commited, returning the buffer
                if let State::Bufferring(buffer) =
                    mem::replace(&mut self.state, State::Commited)
                {
                    // Write back all the data
                    self.target_writer.write_all(buffer.as_slice())
                } else {
                    unreachable!() // The outer match and Rust's thread safety ensures this cannot happen
                }
            }
        }
    }

    pub(crate) fn new(wtr: W, cancellable: bool) -> CancellableWriter<W> {
        let state = if cancellable {
            State::Bufferring(CancellableWriter::new_buffer(&wtr))
        } else {
            State::NotCancellable
        };
        CancellableWriter { target_writer: CounterWriter::new(wtr), state }
    }
}

macro_rules! match_cancellable_writer {
    ($cancellable_writer:expr, |$wtr:ident| $body:expr) => {
        match $cancellable_writer {
            CancellableWriter { state: State::Cancelled($wtr), .. } => $body, // Future writes will be ignored
            CancellableWriter { state: State::Bufferring($wtr), .. } => $body, // Writes will go to the Buffer
            CancellableWriter {
                state: State::NotCancellable | State::Commited,
                target_writer: $wtr,
            } => $body, // Writes will go directly to target_writer
        }
    };
}

impl_write!([W: WriteColor] this: CancellableWriter<W> => [forward, arg]
    match_cancellable_writer!(this, |wtr| forward(wtr, arg))
);
impl_write_color!([W: WriteColor] this: CancellableWriter<W> => [forward, arg]
    // For the operations that query support for colour features, use the underyling writer
    forward(&this.target_writer, arg),
    match_cancellable_writer!(this, |wtr| forward(wtr, arg))
);
