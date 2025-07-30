#![allow(dead_code)]
use std::env;
use std::error;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::{self, ErrorKind, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Command};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::time::{Duration, SystemTime};

use bstr::ByteSlice;

static TEST_DIR: &'static str = "ripgrep-tests";
static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

/// Setup an empty work directory and return a command pointing to the ripgrep
/// executable whose CWD is set to the work directory.
///
/// The name given will be used to create the directory. Generally, it should
/// correspond to the test name.
pub fn setup(test_name: &str) -> (Dir, TestCommand) {
    let dir = Dir::new(test_name);
    let cmd = dir.command();
    (dir, cmd)
}

/// Like `setup`, but uses PCRE2 as the underlying regex engine.
pub fn setup_pcre2(test_name: &str) -> (Dir, TestCommand) {
    let mut dir = Dir::new(test_name);
    dir.pcre2(true);
    let cmd = dir.command();
    (dir, cmd)
}

/// Break the given string into lines, sort them and then join them back
/// together. This is useful for testing output from ripgrep that may not
/// always be in the same order.
pub fn sort_lines(lines: &str) -> String {
    let mut lines: Vec<&str> = lines.trim().lines().collect();
    lines.sort();
    format!("{}\n", lines.join("\n"))
}

/// Returns true if and only if the given program can be successfully executed
/// with a `--help` flag.
pub fn cmd_exists(program: &str) -> bool {
    Command::new(program).arg("--help").output().is_ok()
}

/// Dir represents a directory in which tests should be run.
///
/// Directories are created from a global atomic counter to avoid duplicates.
#[derive(Clone, Debug)]
pub struct Dir {
    /// The directory in which this test executable is running.
    root: PathBuf,
    /// The directory in which the test should run. If a test needs to create
    /// files, they should go in here. This directory is also used as the CWD
    /// for any processes created by the test.
    dir: PathBuf,
    /// Set to true when the test should use PCRE2 as the regex engine.
    pcre2: bool,
}

/// FileTimestamps represents the times associated with a file
#[derive(Clone, Debug)]
pub struct FileTimestamps {
    pub created: SystemTime,
    pub accessed: SystemTime,
    pub modified: SystemTime,
}
impl Dir {
    /// Create a new test working directory with the given name. The name
    /// does not need to be distinct for each invocation, but should correspond
    /// to a logical grouping of tests.
    pub fn new(name: &str) -> Dir {
        let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
        let root = env::current_exe()
            .unwrap()
            .parent()
            .expect("executable's directory")
            .to_path_buf();
        let dir =
            env::temp_dir().join(TEST_DIR).join(name).join(&format!("{}", id));
        if dir.exists() {
            nice_err(&dir, fs::remove_dir_all(&dir));
        }
        nice_err(&dir, repeat(|| fs::create_dir_all(&dir)));
        Dir { root, dir, pcre2: false }
    }

    /// Use PCRE2 for this test.
    pub fn pcre2(&mut self, yes: bool) {
        self.pcre2 = yes;
    }

    /// Returns true if and only if this test is configured to use PCRE2 as
    /// the regex engine.
    pub fn is_pcre2(&self) -> bool {
        self.pcre2
    }

    /// Create a new file with the given name and contents in this directory,
    /// or panic on error.
    pub fn create<P: AsRef<Path>>(&self, name: P, contents: &str) {
        self.create_bytes(name, contents.as_bytes());
    }

    /// Returns the contents of the file with the given name (panics if it's not valid UTF-8)
    pub fn read<P: AsRef<Path>>(&self, name: P) -> String {
        let path = self.dir.join(name);
        nice_err(&path, fs::read_to_string(&path))
    }

    /// Returns the bytes storded in the file with the given name
    pub fn read_bytes<P: AsRef<Path>>(&self, name: P) -> Vec<u8> {
        let path = self.dir.join(name);
        nice_err(&path, fs::read(&path))
    }

    /// Returns the target of the symbolic link with the given name
    pub fn read_link<P: AsRef<Path>>(&self, name: P) -> PathBuf {
        let path = self.dir.join(name);
        nice_err(&path, fs::read_link(&path))
    }

    /// Returns a sorted list of all files/directories recursively
    pub fn list(&self) -> Vec<String> {
        let mut res = Vec::new();
        self.list_recur("".as_ref(), &mut res);
        // Sort it in case the OS didn't do that for us.
        res.sort();
        res
    }
    fn list_recur(&self, prefix: &Path, res: &mut Vec<String>) {
        let path = self.dir.join(prefix);
        for entry in nice_err(&path, fs::read_dir(&path)) {
            let entry = nice_err(&path, entry);
            let new_prefix = prefix.join(entry.file_name());
            if !entry.path().is_symlink() && entry.path().is_dir() {
                self.list_recur(&new_prefix, res)
            }
            // Ensure path seperator is the same accross platforms
            res.push(new_prefix.to_str().unwrap().replace("\\", "/"));
        }
    }
    /// Gets the created, accessed, & modified times of the file with the given name
    pub fn read_timestamps<P: AsRef<Path>>(&self, name: P) -> FileTimestamps {
        let path = self.dir.join(name);
        let metadata = nice_err(&path, fs::metadata(&path));
        // According to the rust documentation, some OS's don't support these metadata fields
        // (so the nice_err calls will fail with an error). This seems to work on Linux and Windows
        FileTimestamps {
            created: nice_err(&path, metadata.created()),
            accessed: nice_err(&path, metadata.accessed()),
            modified: nice_err(&path, metadata.modified()),
        }
    }

    /// Try to create a new file with the given name and contents in this
    /// directory.
    #[allow(dead_code)] // unused on Windows
    pub fn try_create<P: AsRef<Path>>(
        &self,
        name: P,
        contents: &str,
    ) -> io::Result<()> {
        let path = self.dir.join(name);
        self.try_create_bytes(path, contents.as_bytes())
    }

    /// Create a new file with the given name and size.
    pub fn create_size<P: AsRef<Path>>(&self, name: P, filesize: u64) {
        let path = self.dir.join(name);
        let file = nice_err(&path, File::create(&path));
        nice_err(&path, file.set_len(filesize));
    }

    /// Create a new file with the given name and contents in this directory,
    /// or panic on error.
    pub fn create_bytes<P: AsRef<Path>>(&self, name: P, contents: &[u8]) {
        let path = self.dir.join(&name);
        nice_err(&path, self.try_create_bytes(name, contents));
    }

    /// Try to create a new file with the given name and contents in this
    /// directory.
    pub fn try_create_bytes<P: AsRef<Path>>(
        &self,
        name: P,
        contents: &[u8],
    ) -> io::Result<()> {
        let path = self.dir.join(name);
        let mut file = File::create(path)?;
        file.write_all(contents)?;
        file.flush()
    }

    /// Remove a file with the given name from this directory.
    pub fn remove<P: AsRef<Path>>(&self, name: P) {
        let path = self.dir.join(name);
        nice_err(&path, fs::remove_file(&path));
    }

    /// Remove a file or directory with the given name from this directory.
    pub fn remove_maybe_dir<P: AsRef<Path>>(&self, name: P) {
        let path = self.dir.join(name);
        // Sadly fs::remove_file only works on files, and fs::remove_dir_all only works on directories
        // So just try both (remove_dir_all must come first on Windows, as remove_fiel gives an access denied)
        let res = match fs::remove_dir_all(&path) {
            Err(e) if e.kind() == ErrorKind::NotADirectory => {
                fs::remove_file(&path)
            }
            res => res,
        };
        nice_err(&path, res);
    }

    /// Create a new directory with the given path (and any directories above
    /// it) inside this directory.
    pub fn create_dir<P: AsRef<Path>>(&self, path: P) {
        let path = self.dir.join(path);
        nice_err(&path, repeat(|| fs::create_dir_all(&path)));
    }

    /// Creates a new command that is set to use the ripgrep executable in
    /// this working directory.
    ///
    /// This also:
    ///
    /// * Unsets the `RIPGREP_CONFIG_PATH` environment variable.
    /// * Sets the `--path-separator` to `/` so that paths have the same output
    ///   on all systems. Tests that need to check `--path-separator` itself
    ///   can simply pass it again to override it.
    pub fn command(&self) -> TestCommand {
        let mut cmd = self.bin();
        cmd.env_remove("RIPGREP_CONFIG_PATH");
        cmd.current_dir(&self.dir);
        cmd.arg("--path-separator").arg("/");
        if self.is_pcre2() {
            cmd.arg("--pcre2");
        }
        TestCommand { dir: self.clone(), cmd }
    }

    /// Like command, but also ensures the working directory is clean
    /// (i.e. deletes everything in `self.path()`)
    pub fn clean_command(&self) -> TestCommand {
        // Just delete the whole directory and recreate it
        nice_err(&self.dir, fs::remove_dir_all(&self.dir));
        nice_err(&self.dir, fs::create_dir(&self.dir));
        self.command()
    }

    /// Returns the path to the ripgrep executable.
    pub fn bin(&self) -> process::Command {
        let rg = self.root.join(format!("../rg{}", env::consts::EXE_SUFFIX));
        match cross_runner() {
            None => process::Command::new(rg),
            Some(runner) => {
                let mut cmd = process::Command::new(runner);
                cmd.arg(rg);
                cmd
            }
        }
    }

    /// Returns the path to this directory.
    pub fn path(&self) -> &Path {
        &self.dir
    }

    /// Returns the path to the given file.
    pub fn path_of<P: AsRef<Path>>(&self, name: P) -> PathBuf {
        self.dir.join(name)
    }

    /// Creates a directory symlink to the src with the given target name
    /// in this directory.
    #[cfg(not(windows))]
    pub fn link_dir<S: AsRef<Path>, T: AsRef<Path>>(&self, src: S, target: T) {
        use std::os::unix::fs::symlink;
        let src = self.dir.join(src);
        let target = self.dir.join(target);
        let _ = fs::remove_file(&target);
        nice_err(&target, symlink(&src, &target));
    }

    /// Creates a directory symlink to the src with the given target name
    /// in this directory.
    #[cfg(windows)]
    pub fn link_dir<S: AsRef<Path>, T: AsRef<Path>>(&self, src: S, target: T) {
        use std::os::windows::fs::symlink_dir;
        let src = self.dir.join(src);
        let target = self.dir.join(target);
        let _ = fs::remove_dir(&target);
        nice_err(&target, symlink_dir(&src, &target));
    }

    /// Creates a file symlink to the src with the given target name
    /// in this directory.
    #[cfg(not(windows))]
    pub fn link_file<S: AsRef<Path>, T: AsRef<Path>>(
        &self,
        src: S,
        target: T,
    ) {
        self.link_dir(src, target);
    }

    /// Creates a file symlink to the src with the given target name
    /// in this directory.
    #[cfg(windows)]
    #[allow(dead_code)] // unused on Windows
    pub fn link_file<S: AsRef<Path>, T: AsRef<Path>>(
        &self,
        src: S,
        target: T,
    ) {
        use std::os::windows::fs::symlink_file;
        let src = self.dir.join(src);
        let target = self.dir.join(target);
        let _ = fs::remove_file(&target);
        nice_err(&target, symlink_file(&src, &target));
    }
}

/// A simple wrapper around a process::Command with some conveniences.
#[derive(Debug)]
pub struct TestCommand {
    /// The dir used to launched this command.
    dir: Dir,
    /// The actual command we use to control the process.
    cmd: Command,
}

impl TestCommand {
    /// Returns a mutable reference to the underlying command.
    pub fn cmd(&mut self) -> &mut Command {
        &mut self.cmd
    }

    /// Add an argument to pass to the command.
    pub fn arg<A: AsRef<OsStr>>(&mut self, arg: A) -> &mut TestCommand {
        self.cmd.arg(arg);
        self
    }

    /// Add any number of arguments to the command.
    pub fn args<I, A>(&mut self, args: I) -> &mut TestCommand
    where
        I: IntoIterator<Item = A>,
        A: AsRef<OsStr>,
    {
        self.cmd.args(args);
        self
    }

    /// Set the working directory for this command.
    ///
    /// Note that this does not need to be called normally, since the creation
    /// of this TestCommand causes its working directory to be set to the
    /// test's directory automatically.
    pub fn current_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut TestCommand {
        self.cmd.current_dir(dir);
        self
    }

    /// Runs and captures the stdout of the given command.
    pub fn stdout(&mut self) -> String {
        let o = self.output();
        String::from_utf8_lossy(&o.stdout).into_owned()
    }

    /// Pipe `input` to a command, and collect the output.
    pub fn pipe(&mut self, input: &[u8]) -> String {
        self.cmd.stdin(process::Stdio::piped());
        self.cmd.stdout(process::Stdio::piped());
        self.cmd.stderr(process::Stdio::piped());

        let mut child = self._spawn().unwrap();

        // Pipe input to child process using a separate thread to avoid
        // risk of deadlock between parent and child process.
        let mut stdin = child.stdin.take().expect("expected standard input");
        let input = input.to_owned();
        let worker = thread::spawn(move || stdin.write_all(&input));

        let output = self.expect_success(child.wait_with_output().unwrap());
        worker.join().unwrap().unwrap();

        String::from_utf8_lossy(&output.stdout).into_owned()
    }

    /// Gets the output of a command. If the command failed, then this panics.
    /// It also panics if anything was written to stderr
    pub fn output(&mut self) -> process::Output {
        let output = self.raw_output();
        self.expect_success(output)
    }

    /// Gets the raw output of a command after filtering nonsense like jemalloc
    /// error messages from stderr.
    pub fn raw_output(&mut self) -> process::Output {
        let mut output = self._output().unwrap();
        output.stderr = strip_jemalloc_nonsense(&output.stderr);
        output
    }

    pub fn _output(&mut self) -> std::io::Result<std::process::Output> {
        eprintln!("Running {:?}", self.cmd);
        self.cmd.output()
    }
    pub fn _spawn(&mut self) -> std::io::Result<std::process::Child> {
        eprintln!("Running {:?}", self.cmd);
        self.cmd.spawn()
    }

    /// Runs the command and asserts that it resulted in an error exit code.
    pub fn assert_err(&mut self) {
        let o = self.raw_output();
        if o.status.success() {
            panic!(
                "\n\n===== {:?} =====\n\
                 command succeeded but expected failure!\
                 \n\ncwd: {}\
                 \n\ndir list: {:?}\
                 \n\nstatus: {}\
                 \n\nstdout: {}\n\nstderr: {}\
                 \n\n=====\n",
                self.cmd,
                self.dir.dir.display(),
                dir_list(&self.dir.dir),
                o.status,
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            );
        }
    }

    /// Runs the command and asserts that its exit code matches expected exit
    /// code.
    pub fn assert_exit_code(&mut self, expected_code: i32) {
        let o = self.raw_output();
        let code = o.status.code().unwrap();
        assert_eq!(
            expected_code,
            code,
            "\n\n===== {:?} =====\n\
             expected exit code did not match\
             \n\ncwd: {}\
             \n\ndir list: {:?}\
             \n\nexpected: {}\
             \n\nfound: {}\
             \n\nstdout: {}\n\nstderr: {}\
             \n\n=====\n",
            self.cmd,
            self.dir.dir.display(),
            dir_list(&self.dir.dir),
            expected_code,
            code,
            String::from_utf8_lossy(&o.stdout),
            String::from_utf8_lossy(&o.stderr)
        );
    }

    /// Runs the command, asserts that its exit code matches expected exit
    /// code, and returns stderr
    pub fn stderr(&mut self, expected_code: i32) -> String {
        let o = self.raw_output();
        let code = o.status.code().unwrap();
        assert_eq!(
            expected_code,
            code,
            "\n\n===== {:?} =====\n\
             expected exit code did not match\
             \n\ncwd: {}\
             \n\ndir list: {:?}\
             \n\nexpected: {}\
             \n\nfound: {}\
             \n\nstdout: {}\n\nstderr: {}\
             \n\n=====\n",
            self.cmd,
            self.dir.dir.display(),
            dir_list(&self.dir.dir),
            expected_code,
            code,
            String::from_utf8_lossy(&o.stdout),
            String::from_utf8_lossy(&o.stderr)
        );
        String::from_utf8_lossy(&o.stderr).into_owned()
    }

    /// Runs the command and asserts that something was printed to stderr.
    pub fn assert_non_empty_stderr(&mut self) {
        let o = self._output().unwrap();
        if o.status.success() || o.stderr.is_empty() {
            panic!(
                "\n\n===== {:?} =====\n\
                 command succeeded but expected failure!\
                 \n\ncwd: {}\
                 \n\ndir list: {:?}\
                 \n\nstatus: {}\
                 \n\nstdout: {}\n\nstderr: {}\
                 \n\n=====\n",
                self.cmd,
                self.dir.dir.display(),
                dir_list(&self.dir.dir),
                o.status,
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            );
        }
    }

    fn expect_success(&self, o: process::Output) -> process::Output {
        if !o.status.success() || !o.stderr.is_empty() {
            let suggest = if o.stderr.is_empty() {
                "\n\nDid your search end up with no results?".to_string()
            } else {
                "".to_string()
            };

            panic!(
                "\n\n==========\n\
                    command failed but expected success!\
                    {}\
                    \n\ncommand: {:?}\
                    \n\ncwd: {}\
                    \n\ndir list: {:?}\
                    \n\nstatus: {}\
                    \n\nstdout: {}\
                    \n\nstderr: {}\
                    \n\n==========\n",
                suggest,
                self.cmd,
                self.dir.dir.display(),
                dir_list(&self.dir.dir),
                o.status,
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            );
        }
        o
    }
}

pub fn nice_err<T, E: error::Error>(path: &Path, res: Result<T, E>) -> T {
    match res {
        Ok(t) => t,
        Err(err) => panic!("{}: {:?}", path.display(), err),
    }
}

fn repeat<F: FnMut() -> io::Result<()>>(mut f: F) -> io::Result<()> {
    let mut last_err = None;
    for _ in 0..10 {
        if let Err(err) = f() {
            last_err = Some(err);
            thread::sleep(Duration::from_millis(500));
        } else {
            return Ok(());
        }
    }
    Err(last_err.unwrap())
}

/// Return a recursive listing of all files and directories in the given
/// directory. This is useful for debugging transient and odd failures in
/// integration tests.
fn dir_list<P: AsRef<Path>>(dir: P) -> Vec<String> {
    walkdir::WalkDir::new(dir)
        .follow_links(true)
        .into_iter()
        .map(|result| result.unwrap().path().to_string_lossy().into_owned())
        .collect()
}

/// When running tests with cross, we need to be a bit smarter about how we
/// run our `rg` binary. We can't just run it directly since it might be
/// compiled for a totally different target. Instead, it's likely that `cross`
/// will have setup qemu to run it. While this is integrated into the Rust
/// testing by default, we need to handle it ourselves for integration tests.
///
/// Now thankfully, cross sets `CROSS_RUNNER` to point to the right qemu
/// executable. Or so one thinks. But it seems to always be set to `qemu-user`
/// and I cannot find `qemu-user` anywhere in the Docker image. Awesome.
///
/// Thers is `/linux-runner` which seems to work sometimes? But not always.
///
/// Instead, it looks like we have to use `qemu-aarch64` in the `aarch64`
/// case. Perfect, so just get the current target architecture and append it
/// to `qemu-`. Wrong. Cross (or qemu or whoever) uses `qemu-ppc64` for
/// `powerpc64`, so we can't just use the target architecture as Rust knows
/// it verbatim.
///
/// So... we just manually handle these cases. So fucking fun.
fn cross_runner() -> Option<String> {
    let runner = std::env::var("CROSS_RUNNER").ok()?;
    if runner.is_empty() || runner == "empty" {
        return None;
    }
    if cfg!(target_arch = "powerpc64") {
        Some("qemu-ppc64".to_string())
    } else if cfg!(target_arch = "x86") {
        Some("i386".to_string())
    } else {
        // Make a guess... Sigh.
        Some(format!("qemu-{}", std::env::consts::ARCH))
    }
}

/// Returns true if the test setup believes Cross is running and `qemu` is
/// needed to run ripgrep.
///
/// This is useful because it has been difficult to get some tests to pass
/// under Cross.
pub fn is_cross() -> bool {
    std::env::var("CROSS_RUNNER").ok().map_or(false, |v| !v.is_empty())
}

/// Strips absolutely fucked `<jemalloc>:` lines from the output.
///
/// In theory this only happens under qemu, which is where our tests run under
/// `cross`. But is messes with our tests, because... they don't expect the
/// allocator to fucking write to stderr. I mean, what the fuck? Who prints a
/// warning message with absolutely no instruction for what to do with it or
/// how to disable it. Absolutely fucking bonkers.
fn strip_jemalloc_nonsense(data: &[u8]) -> Vec<u8> {
    let lines = data
        .lines_with_terminator()
        .filter(|line| !line.starts_with_str("<jemalloc>:"));
    bstr::concat(lines)
}
