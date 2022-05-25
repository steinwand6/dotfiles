backup:
	pacman -Qqen > ./pkglist.txt
	pacman -Qqem > ./aurlist.txt

update:
	sudo pacman -Syyu

pacman:
	pacman -S emacs
	pacman -S rustup
	pacman -S go

rust:
	rustup default stable
	cargo install cargo-edit
	cargo install cargo-expand
	cargo install cargo-watch
	cargo install cargo-tarpaulin
	cargo install cargo-audit
	cargo install cargo-asm
