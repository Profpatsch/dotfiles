function nei --description 'wrapped nix-env -iA' --argument pkg
	nix-env -iA "nixos.pkgs.$pkg"
end
