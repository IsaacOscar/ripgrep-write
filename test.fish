#!/usr/bin/fish
cd /ripgrep
/ripgrep/_build.fish debug test --all $argv &&
cargo fmt --all --check &&
RUSTDOCFLAGS='-D warnings' cargo doc --no-deps --document-private-items &&
ci/test-complete