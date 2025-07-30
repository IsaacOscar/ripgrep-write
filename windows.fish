#!/usr/bin/fish
clear &&
cd /ripgrep
cargo fmt --all && # Don't use windows version for consistency
echo_colour blue "Rsyncing..." &&
rsync -a --delete /ripgrep/{crates,tests,*.toml,*.lock,*.rs,pkg} ~/ripgrep-windows/ --info=progress2 &&
cd ~/ripgrep-windows &&
echo_colour blue "Cargoing..." &&
if cargo.exe +stable-msvc --color=always test --features pcre2 --all $argv
	chmod +x /rg.exe &&
	cargo.exe +stable-msvc fmt --all --check &&
	RUSTDOCFLAGS='-D warnings' cargo.exe +stable-msvc doc --no-deps --document-private-items 
	#&& ci/test-complete
else
	chmod +x /rg.exe # Always run this, even if test failed
end