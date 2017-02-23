function nix-unpack
	set -l tmpdir (mktemp -d)
	aunpack (nix-build -A $argv[1].src ~/nixpkgs) $tmpdir
	echo $tmpdir
end
