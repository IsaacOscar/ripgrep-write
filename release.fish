#!/usr/bin/fish
clear
cd /ripgrep
# I don't know what these options do, but it's what the openSUSE ripgrep package does
CARGO_AUDITABLE=auditable CARGO_INCREMENTAL=0 CARGO_FEATURE_VENDORED=1 RUSTFLAGS=' -Clink-arg=-Wl,-z,relro,-z,now -C debuginfo=2 -C strip=none' \
	/ripgrep/_build.fish release auditable build -j28 --offline --release $argv
