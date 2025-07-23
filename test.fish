#!/usr/bin/fish
cd /ripgrep
function print_stderr
	set R 0
	for F in (find target/debug -name stderr)
		if test -s $F
			echo_colour red "==============================="
			cat $F
			set R 1
		end
	end
	return $R
end

/ripgrep/_build.fish debug test --all $argv &&
print_stderr &&
cargo fmt --all --check &&
RUSTDOCFLAGS='-D warnings' cargo doc --no-deps --document-private-items &&
ci/test-complete