#!/usr/bin/fish
clear &&
cd /ripgrep &&
cargo fmt --all &&
echo_colour blue "Rsyncing..." &&
rsync -a /ripgrep/{crates,tests,*.toml,*.lock,*.rs,pkg} ~/ripgrep-windows/ --info=progress2 &&
cd ~/ripgrep-windows &&
# --target x86_64-pc-windows-msvc C:\\Program\ Files\\Microsoft\ Visual\ Studio\\2022\\Enterprise\\VC\\Tools\\MSVC\\14.41.34120\\bin\\Hostx64\\x64 
cargo.exe  +stable-msvc --color=always $argv[1] --features pcre2 $argv[2..]
