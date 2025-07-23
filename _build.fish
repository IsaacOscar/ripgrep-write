#!/usr/bin/fish
clear
cd /ripgrep
ln -sf /ripgrep/target/$argv[1]/rg /rg
cargo fmt --all &&
cargo --color=always $argv[2] --features pcre2 $argv[3..]
exit $pipestatus[1]