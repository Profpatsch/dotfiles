function haskellEnv --description 'start a nix-shell with the given Haskell packages'
	nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ $argv ])"
end
