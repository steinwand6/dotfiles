backup:
	pacman -Qqen > ./pkglist.txt
	pacman -Qqem > ./aurlist.txt

update:
	sudo pacman -Syyu
	curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/bin/rust-analyzer
