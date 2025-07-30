#![allow(unused)]
// TODO: remove 'allows'
use crate::hay::*;
use crate::util::{cmd_exists, nice_err, Dir, TestCommand};
use encoding_rs::mem as encoding;
use regex::Regex;
use std::{
    borrow::Cow,
    fs::{self, File},
    io::Result,
    path::Path,
    thread,
    time::{Duration, SystemTime},
};

fn locksher(str: &str) -> String {
    str.replace("Sherlock", "Locksher")
}

/// A test that the main -W / --write-replace flag works with a simple replacement, and with files with some unusual bytes
rgtest!(write_replace_strange, |dir: Dir, _| {
    // Test twice, once when we disable the new flags that fix issues
    for disable_fixes in [false, true] {
        let mut cmd = dir.clean_command();
        dir.create("sherlock", SHERLOCK);
        dir.create("sherlock.crlf", SHERLOCK_MIXED_CRLF);
        dir.create("sherlock.bom", SHERLOCK_BOM);
        dir.create("sherlock.no-eol", SHERLOCK_NO_EOL);
        dir.create("sherlock.bin", SHERLOCK_NUL);

        // Run ripgrep, which should only print stats to stdout
        // The --no-mmap is needed for the --stats to be deterministic
        // The -j1 is to ensure that context seperators ("--" by default) are NOT printed
        cmd.args([
            "--no-mmap",
            "-j1",
            "--stats",
            "--crlf",
            "Sherlock",
            "-W",
            "Locksher",
        ]);
        if disable_fixes {
            // Disable ripgrep-write's new options that prevent the file from changing in strange ways
            cmd.args(["--ensure-eol", "--no-binary"]);
        }

        let stdout = cmd.stdout();
        // Remove the exact timing from reported stats, as they won't be deterministic
        let stdout = Regex::new(r"[0-9.]+ seconds")
            .unwrap()
            .replace_all(&stdout, "<time> seconds");

        // for some reason there is a blank line at the start
        let expected = if disable_fixes {
            "\n\
            15 matches\n\
            15 matched lines\n\
            5 files contained matches\n\
            5 files searched\n\
            67035 bytes printed\n\
            66944 bytes searched\n\
            <time> seconds spent searching\n\
            <time> seconds\n"
        } else {
            "\n\
            8 matches\n\
            8 matched lines\n\
            4 files contained matches\n\
            5 files searched\n\
            1472 bytes printed\n\
            66944 bytes searched\n\
            <time> seconds spent searching\n\
            <time> seconds\n"
        };

        eqnice!(expected, stdout);
        eqnice!(locksher(SHERLOCK), dir.read("sherlock")); // Output for stdin always goes to stdout
        eqnice_repr!(locksher(SHERLOCK_MIXED_CRLF), dir.read("sherlock.crlf"));
        // Bom is never stripped (it is stripped in the usual --encoding=auto mode, but that's incompatible with
        // --write-to)
        eqnice_repr!(locksher(SHERLOCK_BOM), dir.read("sherlock.bom"));

        // SHERLOCK_NO_EOL neds --no-ensure-eol to not add a \r\n at the end (or \n if --crlf isn't used)
        eqnice_repr!(
            if disable_fixes {
                locksher(SHERLOCK_NO_EOL) + "\r\n"
            } else {
                locksher(SHERLOCK_NO_EOL)
            },
            dir.read("sherlock.no-eol")
        );

        // SHERLOCK_NUL needs --ensure-no-binary to be skipped (or --text to process it fully)
        eqnice_repr!(
            if disable_fixes {
                // The first 65472 bytes are replaced, and then a warning message truncating the rest of the file
                locksher(&SHERLOCK_NUL[..65472]) +
                "WARNING: stopped searching binary file after match (found \"\\0\" byte around offset 77041)\n"
            } else {
                SHERLOCK_NUL.to_string()
            },
            dir.read("sherlock.bin")
        );
    }
});

/// A test that --write-replace preserves permissions
/// (Rusts's standard library doesn't expose a way to change file attirbutes on Windows)
#[cfg(unix)]
rgtest!(write_replace_permissions, |dir: Dir, mut cmd: TestCommand| {
    dir.create("sherlock", SHERLOCK);
    let path = dir.path_of("sherlock");

    use std::{fs::Permissions, os::unix::fs::PermissionsExt};
    let mode = 0o100755; // Mark the file as world-executable
    nice_err(&path, fs::set_permissions(&path, Permissions::from_mode(mode)));

    cmd.args(["", "-W", "", "sherlock"]); // Trivial operation that should write the same file back
    eqnice!("", cmd.stdout()); // Run ripgrep and check it silently suceeds
    eqnice!(SHERLOCK, dir.read("sherlock")); // Contents should be the same

    // Check the permisions haven't changed
    let perms = nice_err(&path, fs::metadata(&path)).permissions();
    assert_eq!(mode, perms.mode());
});

// A test to see what happens when tricking --write-replace/--write-to into simultatenously write to the same file using a symlink.
// It crashes with a SIGBUS (an alignment error?) on Linux, // but exits gracefully on Windows.
// TODO: better comment
// LINUX: status: signal: 7 (SIGBUS) (core dumped)
//status: exit code: 2
// stderr: rg: sherlock.first: failed to open file for for writing: sherlock.first: The requested operation cannot be performed on a file with a user-mapped section open. (os error 1224)
rgtest!(
    #[ignore]
    write_same_file,
    |dir: Dir, _| {
        let filesize_multiplier = 1_000; // Smaller numbers are less likely to consistently give an error
        let big_sherlock = SHERLOCK.repeat(filesize_multiplier);
        let expected = locksher(SHERLOCK).repeat(filesize_multiplier);

        // Since this test often fails, this will "randomly" shuffle which of the two cases is tested first
        let mut write_args = vec!["-WLocksher", "-O../out"];
        let time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        if time % 2 == 1 {
            write_args.reverse();
        }
        for write_arg in write_args {
            let mut cmd = dir.clean_command();
            dir.create_dir("in"); // The folder we will search

            // The list of regular files we will create
            let mut regular_files = vec![];
            // A list of <symlink> -> <regular> pairs of symlinks to create
            let mut symlink_files = vec![];
            if write_arg.starts_with("-W") {
                regular_files.extend(["in/sherlock.first"]);
                symlink_files
                    .extend([("in/sherlock.second", "in/sherlock.first")]);
            } else {
                dir.create_dir("out");
                regular_files
                    .extend(["in/sherlock.first", "in/sherlock.second"]);
                symlink_files.extend([
                    ("out/sherlock.first", "in/sherlock.second"),
                    ("out/sherlock.second", "in/sherlock.first"),
                ]);
            };
            for regular in &regular_files {
                dir.create(regular, &big_sherlock);
            }
            for (symlink, regular) in &symlink_files {
                dir.link_file(regular, symlink);
            }

            cmd.current_dir(dir.path_of("in"));
            // The -j2 forces it to work in parrallel. Withought the --mmap, you don't get a crash
            cmd.arg(write_arg)
                .args(["--mmap", "-j2", "-L", "Sherlock", "-R", "Locksher"]);

            eqnice!("", cmd.stdout());
            // Where the files modified correctly?
            for regular in &regular_files {
                eqnice!(expected, dir.read(regular));
            }
            // The symlinks themselves shouldn't be modified
            for (symlink, regular) in &symlink_files {
                eqnice_repr!(dir.path_of(regular), dir.read_link(symlink));
            }
        }
    }
);

/// A test to show that --write-replace and --write-to will write
/// to the target of symbolic links, and not touch the links themselves
rgtest!(write_symlinks, |dir: Dir, _| {
    let sherlock2 = SHERLOCK.to_string().to_uppercase();
    for write_args in [["--mmap", "-W"], ["-Oout", "-R"]] {
        let mut cmd = dir.clean_command();
        dir.create_dir("dir");
        dir.create("dir/sherlock2.real", &sherlock2);

        dir.create_dir("in");
        dir.create("sherlock.real", SHERLOCK);

        dir.link_file("sherlock.real", "in/sherlock.link");
        dir.link_dir("dir", "in/dir.link");

        if write_args[0].starts_with("-O") {
            dir.create_dir("out");
            dir.create_dir("out-in");
            dir.link_dir("out-in", "out/in");
            dir.link_file("sherlock.out", "out-in/sherlock.link");
        }
        cmd.args(write_args).args(["<Replaced>", "-iL", "Sherlock", "in"]);
        let mut expected_files = dir.list();

        eqnice!("", cmd.stdout());

        let sherlock_out = SHERLOCK.replace("Sherlock", "<Replaced>");
        let sherlock2_out = sherlock2.replace("SHERLOCK", "<Replaced>");
        if write_args[1] == "-W" {
            eqnice!(sherlock_out, dir.read("sherlock.real"));
            eqnice!(sherlock2_out, dir.read("dir/sherlock2.real"));
        } else {
            eqnice!(SHERLOCK, dir.read("sherlock.real"));
            eqnice!(sherlock2, dir.read("dir/sherlock2.real"));
            eqnice!(sherlock_out, dir.read("sherlock.out"));
            eqnice!(sherlock2_out, dir.read("out-in/dir.link/sherlock2.real"));

            eqnice_repr!(dir.path_of("out-in"), dir.read_link("out/in"));
            eqnice_repr!(
                dir.path_of("sherlock.out"),
                dir.read_link("out-in/sherlock.link")
            );
            // New files that should be created
            expected_files.extend([
                "out-in/dir.link".to_string(),
                "out-in/dir.link/sherlock2.real".to_string(),
                "sherlock.out".to_string(),
            ]);
            expected_files.sort();
        }
        // Symlinks should not be modified
        eqnice_repr!(
            dir.path_of("sherlock.real"),
            dir.read_link("in/sherlock.link")
        );
        eqnice_repr!(dir.path_of("dir"), dir.read_link("in/dir.link"));

        eqnice_repr!(expected_files, dir.list());
    }
});

/// A test that --write-to refuses to write to it's input files
rgtest!(write_to_no_overwrite, |dir: Dir, _| {
    use std::ffi::OsStr;
    // Tests rg -L "Sherlock" <input_file> -O <output_folder>, after executing prepare.
    // Prepare should return the working directory to use (relative to dir)
    // <input_realpath> should be the realpath of the input file relative to dir.
    // As a sanity check, it also runs rg  -L "Sherlock" <input_file> -W Locksher,
    // to verify that the input file can sucesfully be overrwritten by -W.
    fn test<'a, F: FnOnce() -> &'a str>(
        dir: &Dir,
        input_file: &str,
        output_folder: &str,
        input_realpath: &str,
        prepare: F,
    ) {
        let mut cmd = dir.clean_command();
        let cwd = dir.path_of(prepare());
        let files = dir.list();
        // Make some of the tests use --ensure-no-binary, and others not use it
        // (this is to test different code paths in multi_writer.rs);
        let binary = if output_folder.starts_with(".") {
            "--ensure-no-binary"
        } else {
            "--text"
        };
        for write_args in [["-O", output_folder], ["-W", "Locksher"]] {
            cmd.current_dir(&cwd);
            cmd.args([binary, "-L", "Sherlock", input_file]).args(write_args);
            if write_args[0] == "-O" {
                let expected = format!(
                    "rg: {input_file}: Refusing to overwrite input file in -O/--write-to mode: {}\n",
                    // Need to make sure path is correct for windows
                    dir.path_of(input_realpath.replace("/", std::path::MAIN_SEPARATOR_STR)).display());
                eqnice!(expected, cmd.stderr(2));
            } else {
                let expected = locksher(SHERLOCK);
                eqnice!("", cmd.stdout());
                eqnice!(expected, dir.read(cwd.join(input_file)));
            }

            eqnice_repr!(files, dir.list()); // No files should be created or deleted
            cmd = dir.command(); // Reset command (but not dir) for next iteration
        }
    };
    // Using . as the output directory
    test(&dir, "file", ".", "file", || {
        dir.create("file", SHERLOCK);
        "."
    });
    // Using an absolute path to the current directory
    let dir_path = dir.path().to_str().unwrap();
    test(&dir, "file", dir_path, "file", || {
        dir.create("file", SHERLOCK);
        "."
    });
    // If the output file already exists as a symlink to the input file
    test(&dir, "file", "../out", "in/file", || {
        dir.create_dir("in");
        dir.create("in/file", SHERLOCK);
        dir.create_dir("out");
        dir.link_file("in/file", "out/file");
        "in"
    });
    // If the output directory is a symlink to the input directory
    test(&dir, "file", "out", "file", || {
        dir.create("file", SHERLOCK);
        dir.link_dir(".", "out");
        "."
    });
    // If the input file is a symlink to the output file
    test(&dir, "file", "out", "out/file", || {
        dir.create_dir("out");
        dir.create("out/file", SHERLOCK);
        dir.link_file("out/file", "file");
        "."
    });
    // If the input directory is a symlink to the output directory
    test(&dir, "file", "../out", "out/file", || {
        dir.create_dir("out");
        dir.create("out/file", SHERLOCK);
        dir.link_dir("out", "in");
        "in"
    });
    // If the input file and output files are both symlinks to the same file
    test(&dir, "file", "../out", "real_file", || {
        dir.create("real_file", SHERLOCK);
        dir.create_dir("in");
        dir.link_file("real_file", "in/file");
        dir.create_dir("out");
        dir.link_file("real_file", "out/file");
        "in"
    });
    // If the input folder and output folders are both symlinks to the same folder
    test(&dir, "file", "../out", "real_folder/file", || {
        dir.create_dir("real_folder");
        dir.create("real_folder/file", SHERLOCK);
        dir.link_dir("real_folder", "in");
        dir.link_dir("real_folder", "out");
        "in"
    });
});

/// A test that the main --write-replace flag touches file time stamps in a reasonable way
rgtest!(write_replace_timestamps, |dir: Dir, _| {
    // Do the test twice, as the behaviour is different with --passthru vs --passthru-only-matching
    for only_matching in [true, false] {
        let mut cmd = dir.clean_command();

        let watson = "Dr Watson is here";
        dir.create("sherlock", SHERLOCK);
        dir.create("watson", watson);

        let sherlock_before = dir.read_timestamps("sherlock");
        let watson_before = dir.read_timestamps("watson");

        // Wait a bit so anything ripgrep does happens at a 'later' time
        thread::sleep(Duration::from_millis(100));

        if only_matching {
            cmd.args(["--mmap", "Sherlock", "-W", "$0"]);
        } else {
            cmd.args(["--mmap", "Sherlock", "-W", "$0", "--passthru"]);
        }
        eqnice!("", cmd.stdout());

        eqnice!(SHERLOCK, dir.read("sherlock"));
        eqnice!(watson, dir.read("watson"));

        let sherlock_after = dir.read_timestamps("sherlock");
        let watson_after = dir.read_timestamps("watson");

        // The files should not have been recreated
        assert_eq!(sherlock_before.created, sherlock_after.created);
        assert_eq!(watson_before.created, watson_after.created);

        // Both files should have been accessed
        assert!(sherlock_before.accessed < sherlock_after.accessed);
        assert!(watson_before.accessed < watson_after.accessed);

        // Sherlock matches, so it should have been modified (even though the contents are the same)
        assert!(sherlock_before.modified < sherlock_after.modified);
        if only_matching {
            assert_eq!(watson_before.modified, watson_after.modified);
        } else {
            // Watson will be modified, even though it didn't match
            assert!(watson_before.modified < watson_after.modified);
        }
    }
});

// Test that --write-replace/write-to work with stdin and multiline searchers
// (stdin searches will write to stdout, instead of an <stdin> file)
// (this test exercises the different branches in search_file_maybe_path, search_file, and search_reader)
rgtest!(write_stdin_multiline, |dir: Dir, _| {
    for stdin in [false, true] {
        for regex in
            [vec!["Sherlock"], vec!["-U", "of this world(.|\n)*with a label"]]
        {
            for write_args in
                [["--mmap", "-W", "<deleted>"], ["-OF", "-r", "<deleted>"]]
            {
                let mut cmd = dir.clean_command();
                cmd.args(&regex).args(write_args);

                let expected = if regex[0] == "-U" {
                    "For the Doctor Watsons <deleted> attached.\n".to_string()
                } else if write_args[0].starts_with("-O") {
                    "For the Doctor Watsons of this world, as opposed to the <deleted>\n\
                    be, to a very large extent, the result of luck. <deleted> Holmes\n".to_string()
                } else {
                    SHERLOCK.replace("Sherlock", "<deleted>")
                };

                if stdin {
                    eqnice!(expected, cmd.pipe(SHERLOCK.as_bytes()));
                    // No files should be written
                    eqnice_list!(=> dir.list())
                } else {
                    dir.create("sherlock", SHERLOCK);
                    eqnice!("", cmd.stdout());
                    if write_args[1] == "-W" {
                        eqnice!(expected, dir.read("sherlock"));
                        eqnice_list!("sherlock" => dir.list());
                    } else {
                        eqnice!(SHERLOCK, dir.read("sherlock"));
                        eqnice!(expected, dir.read("F/sherlock"));
                        eqnice_list!("F", "F/sherlock", "sherlock" => dir.list());
                    }
                }
            }
        }
    }
});

// A test that --write-replace refuses to work on UTF-16 files, as doing so would change the encoding
rgtest!(write_replace_utf16, |dir: Dir, _| {
    for encoding_args in [vec![], vec!["--encoding=utf-16le"]] {
        for write_args in
            [vec!["-W"], vec!["--write-to=folder", "--passthru", "-r"]]
        {
            let mut cmd = dir.clean_command();
            // A BOM is needed to auto-detect the encoding
            let sherlock_utf8 =
                if encoding_args.is_empty() { SHERLOCK_BOM } else { SHERLOCK };
            // Add a random nom-BMP character to test encoding works
            let sherlock_utf8 = sherlock_utf8.to_string() + "😃\n";

            // Can't use encoding_rs::UTF_16LE.encode at that actually encodes to UTF-8
            // (the +1 below is required by the convert_utf8_to_utf16 documentation)
            let mut sherlock_utf16 = vec![0; sherlock_utf8.len() + 1];
            let utf16_length = encoding::convert_utf8_to_utf16(
                sherlock_utf8.as_bytes(),
                &mut sherlock_utf16,
            );
            assert!(utf16_length < sherlock_utf16.len()); // Otherwise the buffer was too small

            // Convert to little endian bytes (for cross-platform consistency)
            let sherlock_utf16: Vec<u8> = sherlock_utf16
                .iter()
                .take(utf16_length)
                .flat_map(|pair| pair.to_le_bytes())
                .collect();

            dir.create_bytes("sherlock.utf16", &sherlock_utf16);
            cmd.args(&encoding_args)
                .args(&write_args)
                .args(["Locksher", "Sherlock"]);

            if write_args[0] == "-W" {
                if encoding_args.is_empty() {
                    eqnice!("", cmd.stderr(1));
                } else {
                    let expected = "rg: Using --encoding/--no-encoding with -W/--write-replace is currently not supported\n";
                    eqnice!(expected, cmd.stderr(2));
                }

                // Files should be untouched
                eqnice_repr!(sherlock_utf16, dir.read_bytes("sherlock.utf16"));
                eqnice_list!("sherlock.utf16" => dir.list());
            } else {
                // --write-to should work, but stripts the BOM and converts to utf-8
                eqnice!("", cmd.stdout());
                eqnice!(
                    locksher(&sherlock_utf8).replace(BOM, ""),
                    dir.read("folder/sherlock.utf16")
                );
                eqnice_repr!(sherlock_utf16, dir.read_bytes("sherlock.utf16"));
                eqnice_list!("folder", "folder/sherlock.utf16", "sherlock.utf16" => dir.list());
            }
        }
    }
});

// A test that --write-to doesn't create output files for non-matching inputs
// And writes the same output as --no-filename would.
rgtest!(write_to_non_matching, |dir: Dir, _| {
    for out_arg in ["--no-filename", "-O../out"] {
        let mut cmd = dir.clean_command();

        dir.create_dir("in");
        let watson = "Only Dr Watson is Here.";
        dir.create("in/sherlock", SHERLOCK);
        dir.create("in/watson", watson);
        cmd.current_dir(dir.path_of("in"));

        // Make sure it works with line buffering (the default is block buffering)
        // It shouldn't affect the output
        cmd.arg(out_arg).args(["-n", "--line-buffered", "Sherlock"]);
        let stdout = cmd.stdout();

        let expected = "\
            1:For the Doctor Watsons of this world, as opposed to the Sherlock\n\
            3:be, to a very large extent, the result of luck. Sherlock Holmes\n";

        // Input files should not be modified
        eqnice!(SHERLOCK, dir.read("in/sherlock"));
        eqnice!(watson, dir.read("in/watson"));
        if out_arg.starts_with("-O") {
            eqnice!("", stdout);
            eqnice!(expected, dir.read("out/sherlock"));
            // Verify no unexpected files/directories were created
            // (in particular, there should be no out/watson file as it didn't match)
            eqnice_list!("in", "in/sherlock", "in/watson", "out", "out/sherlock" => dir.list());
        } else {
            eqnice!(expected, stdout);
            eqnice_list!("in", "in/sherlock", "in/watson" => dir.list());
        }
    }
});

// A test that --write-to refuses to work on absolute paths
// (as it won't know where to put the output file)
rgtest!(write_to_absolute, |dir: Dir, mut cmd: TestCommand| {
    dir.create_dir("in");
    dir.create("in/sherlock", SHERLOCK);
    cmd.current_dir(dir.path_of("in"));
    let absolute = dir.path_of("in/sherlock");
    let absolute = absolute.to_str().unwrap();
    cmd.args(["-O../out", "Sherlock", absolute]);
    let expected = format!("rg: {}: Searching absolute paths with -O/--write-to is not supported.\n", absolute);
    eqnice!(expected, cmd.stderr(2));

    // Verify nothing was modified
    eqnice_list!("in", "in/sherlock" => dir.list());
    eqnice!(SHERLOCK, dir.read("in/sherlock"));
});

/// A test that --write-replace and --write-to don't output in color when in --color=auto mode,
/// even if stdout is a terminal (I don't have the faintest idea how to test this on Windows)
#[cfg(unix)]
rgtest!(write_no_color, |dir: Dir, _| {
    use std::ffi::CStr;
    use std::io::IsTerminal;

    // ctermid returns a path to the process's tty, if any (it's usually /dev/tty)
    // SAFETY: the documentation for ctermid says it accepts null pointers,
    // returns a string, is thread safe, and doesn't mention any error/undefined-behaviour posibilities.
    let tty_path = unsafe { CStr::from_ptr(libc::ctermid(0 as *mut i8)) };
    // Convert the path to a UTF-8 string
    let tty_path = tty_path.to_str().unwrap();

    for write_arg in ["--write-replace=Sherlock", "--write-to=out"] {
        let mut cmd = dir.clean_command();

        dir.create("sherlock", SHERLOCK);
        cmd.arg(write_arg).args(["--color=auto", "Sherlock"]);

        // Open the terminal
        let tty = nice_err(tty_path.as_ref(), File::open(tty_path));
        // Check that we did indeed get a tty back
        assert!(tty.is_terminal());

        // Redirect stdout to the tty
        cmd.cmd().stdout(tty);

        // Note that because stdout was redirected above,
        // the following will always pass, even if ripgrep does print something
        eqnice!("", cmd.stdout());

        if write_arg.starts_with("--write-replace") {
            eqnice!(SHERLOCK, dir.read("sherlock"));
            eqnice_list!("sherlock" => dir.list());
        } else {
            let expected = "\
                For the Doctor Watsons of this world, as opposed to the Sherlock\n\
                be, to a very large extent, the result of luck. Sherlock Holmes\n";
            eqnice!(expected, dir.read("out/sherlock"));
            eqnice_list!("out", "out/sherlock", "sherlock" => dir.list());
        }
    }
});

// A test that --write-replace/write-to ignore empty files, even if they do match the regex
// (it would be better if it did create the empty file, but ripgrep
// always reports empty files as non-matching)
rgtest!(write_empty, |dir: Dir, _| {
    for write_arg in ["-Wreplacement", "-Oout"] {
        let mut cmd = dir.clean_command();
        dir.create("empty", "");
        cmd.arg(write_arg).arg(""); // Trivial regex that should match, but ripgrep disagrees
        eqnice!("", cmd.stderr(1)); // No-match occured

        // Nothing should have been modified
        eqnice!("", dir.read("empty"));
        eqnice_list!("empty" => dir.list());
    }
});

// A test that the empty regex does in fact match a file with a single null byte
rgtest!(write_almost_empty, |dir: Dir, _| {
    for write_arg in ["-Wreplacement", "-Oout"] {
        let mut cmd = dir.clean_command();
        dir.create("almost_empty", "\x00");
        cmd.arg(write_arg).args(["--text", ""]);
        assert_eq!("", cmd.stdout());
        if write_arg.starts_with("-W") {
            eqnice_repr!("replacement\x00", dir.read("almost_empty"));
            eqnice_list!("almost_empty" => dir.list());
        } else {
            eqnice_repr!("\x00", dir.read("almost_empty"));
            eqnice_repr!("\x00\n", dir.read("out/almost_empty"));
            eqnice_list!("almost_empty", "out", "out/almost_empty" => dir.list());
        }
    }
});

// A test that various output controlling flags are ignored in -W/--write-replace mode
// (not ignoring them would likely leed to inappropriate file modifications)
// But they do work with --write-to
rgtest!(write_bad_flags, |dir: Dir, _| {
    for write_to in [false, true] {
        let mut cmd = dir.clean_command();
        dir.create("sherlock", SHERLOCK);
        cmd.args([
            "--mmap",
            "Sherlock",
            "-W",
            "SHERLOCK",
            "-A1",
            "-B3",
            "--color=always",
            "-bnoH",
            "--column",
            "-M10",
            "--vimgrep",
            "--context-separator=--",
        ]);
        if write_to {
            cmd.arg("--write-to=o");
        }
        eqnice!("", cmd.stdout()); // Should silently succeed

        if write_to {
            // Ansi color codes for things
            let ansi = |code: usize| format!("\u{1b}[{code}m");
            let reset = ansi(0);
            let bold = ansi(1);

            // Bold red, magenta (on Windows, cyan), green, and reset
            let matched = format!(
                "{reset}{bold}{color}SHERLOCK{reset}",
                color = ansi(31)
            );
            let path = format!(
                "{reset}{color}sherlock{reset}",
                color = ansi(if cfg!(windows) { 36 } else { 35 })
            );
            let line_num = |n: usize| {
                format!("{reset}{color}{n}{reset}", color = ansi(32))
            };
            let col_num = |n: usize| format!("{reset}{n}{reset}");

            let make_line = |line: usize,
                             col_start: usize,
                             col_end: Option<usize>,
                             body: &str| {
                let s = if col_end.is_some() { ":" } else { "-" };
                let suffix = col_end.map_or("".to_string(), |col_end| {
                    format!("{}{s}", col_num(col_end))
                });
                format!(
                    "{path}{s}{line}{s}{col_start}{s}{suffix}{body}",
                    line = line_num(line),
                    col_start = col_num(col_start)
                )
            };
            // due to the -o flag, there's no newline at the end of lines with matched
            let mut expected = String::new()
                + &make_line(1, 57, Some(56), &matched)
                + &make_line(2, 65, None, "[Omitted long context line]\n")
                + &make_line(3, 49, Some(177), &matched)
                + &make_line(4, 193, None, "[Omitted long context line]\n");
            eqnice!(expected, dir.read("o/sherlock"));
            eqnice_list!("o", "o/sherlock", "sherlock" => dir.list());
        } else {
            // in --write-replace mode, the fancy output settings are ignored
            eqnice!(
                SHERLOCK.replace("Sherlock", "SHERLOCK"),
                dir.read("sherlock")
            );
            eqnice_list!("sherlock" => dir.list());
        }
    }
});

// A test that --search-zip and --pre don't work with -write-replace, but do with --write-to
rgtest!(write_fatal_flags, |dir: Dir, _| {
    for pre_arg in ["-z", "--pre=zcat"] {
        for write_args in [["-W", "SHERLOCK"], ["-OO", "-RSHERLOCK"]] {
            if pre_arg == "--pre=zcat" && !cmd_exists("zcat") {
                continue;
            }
            let mut cmd = dir.clean_command();
            let gz = include_bytes!("./data/sherlock.gz");
            dir.create_bytes("sherlock.gz", gz);
            cmd.arg(pre_arg).args(write_args).arg("Sherlock");
            if write_args[0] == "-W" {
                let expected = if pre_arg == "-z" {
                    "rg: Using -z/--search-zip with -W/--write-replace is currently not supported\n"
                } else {
                    "rg: Using --pre with -W/--write-replace is currently not supported\n"
                };
                eqnice!(expected, cmd.stderr(2));
                eqnice_list!("sherlock.gz" => dir.list());
            } else {
                eqnice!("", cmd.stdout());
                // Output is NOT compressed
                eqnice!(
                    SHERLOCK.replace("Sherlock", "SHERLOCK"),
                    dir.read("O/sherlock.gz")
                );
                eqnice_list!("O", "O/sherlock.gz", "sherlock.gz" => dir.list());
            }
            eqnice_repr!(gz, dir.read_bytes("sherlock.gz"));
        }
    }
});

// A simple test that -W/--write-replace works with --trim
// (as that may be usefull)
rgtest!(write_replace_trim, |dir: Dir, mut cmd: TestCommand| {
    dir.create("padded", "  text  \n");
    cmd.args(["--mmap", "--trim", "", "-W", ""]); // Just trim
    eqnice!("", cmd.stdout());
    // For some reason, --trim only trims the start of each line
    eqnice_repr!("text  \n", dir.read("padded"));
});

// A test that --quiet overrides --write-replace and --write-to
// (i.e. nothing is actually written)
rgtest!(write_quiet, |dir: Dir, _| {
    for write_args in [["-O", "out"], ["-W", "SHERLOCK"]] {
        let mut cmd = dir.clean_command();
        dir.create("sherlock", SHERLOCK);
        cmd.args(["-q", "Sherlock"]).args(write_args);
        eqnice!("", cmd.stdout());
        eqnice!(SHERLOCK, dir.read("sherlock"));
        eqnice_list!("sherlock" => dir.list());
    }
});

// A test of a former bug with --mmap and --ensure-no-binary
rgtest!(write_mmap_ensure_no_binary, |dir: Dir, mut cmd: TestCommand| {
    dir.create("sherlock", SHERLOCK);
    dir.create("sherlock.bin", SHERLOCK_NUL);
    cmd.args(["--mmap", "--ensure-no-binary", "Sherlock"]);
    let expected = "\
        sherlock:For the Doctor Watsons of this world, as opposed to the Sherlock\n\
        sherlock:be, to a very large extent, the result of luck. Sherlock Holmes\n";
    eqnice!(expected, cmd.stdout());

    // And with -O
    cmd.args(["-O", "out"]);
    eqnice!("", cmd.stdout());
    eqnice_list!(
        "out", "out/sherlock", "sherlock", "sherlock.bin"
        => dir.list()
    );
    let expected = expected.replace("sherlock:", "");
    eqnice!(expected, dir.read("out/sherlock"));
    dir.remove("out");

    // Again, but with --no-binary to restore default behaviour
    cmd.args(["--no-binary"]);
    eqnice!("", cmd.stdout());
    eqnice_list!(
        "out",
        "out/sherlock",
        "out/sherlock.bin",
        "sherlock",
        "sherlock.bin"
        => dir.list()
    );
    eqnice!(expected, dir.read("out/sherlock"));
    let expected = "don't know Sherlock Holmes yet,\" he said; \"perhaps you would not care\n".repeat(7)
        + "WARNING: stopped searching binary file after match (found \"\\0\" byte around offset 77041)\n";
    eqnice!(expected, dir.read("out/sherlock.bin"));
});
