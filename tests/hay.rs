pub const SHERLOCK_NO_EOL: &'static str = "\
For the Doctor Watsons of this world, as opposed to the Sherlock
Holmeses, success in the province of detective work must always
be, to a very large extent, the result of luck. Sherlock Holmes
can extract a clew from a wisp of straw or a flake of cigar ash;
but Doctor Watson has to have it taken out for him and dusted,
and exhibited clearly, with a label attached.";

pub const SHERLOCK: &'static str =
    const_format::concatc!(SHERLOCK_NO_EOL, "\n");

pub const SHERLOCK_CRLF: &'static str =
    const_format::str_replace!(SHERLOCK, "\n", "\r\n");

pub const SHERLOCK_MIXED_CRLF: &'static str =
    const_format::str_replace!(SHERLOCK, "s\n", "s\r\n");

pub const BOM: char = '\u{FEFF}';
pub const SHERLOCK_BOM: &'static str = const_format::concatc!(BOM, SHERLOCK);

// Has a NUL byte somwheree
pub const SHERLOCK_NUL: &'static str = include_str!("./data/sherlock-nul.txt");
