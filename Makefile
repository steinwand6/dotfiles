backup:
	pacman -Qqen > ./pkglist.txt
	pacman -Qqem > ./aurlist.txt

update:
	sudo pacman -Syyu
	rustup update

pacman:
	pacman -S emacs
	pacman -S rustup
	pacman -S go
	pacman -S snapd
	pacman -S rlwrap
	pacman -S sbcl

rust:
	rustup default stable
	cargo install cargo-edit
	cargo install cargo-expand
	cargo install cargo-watch
	cargo install cargo-tarpaulin
	cargo install cargo-audit
	cargo install cargo-asm
	cargo install rust-script
	rustup component add rust-analyzer

c:
	systemctl enable --now snapd.socket
	ln -s /var/lib/snapd/snap /snap
	snap install ccls --classic

hacking:
	pacman -Sy nmap
