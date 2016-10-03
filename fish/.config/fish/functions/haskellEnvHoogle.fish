function haskellEnvHoogle --description 'start a nix-shell with the given Haskell packages and hoogle docs'
	nix-shell -p "haskellPackages.ghcWithHoogle (pkgs: with pkgs; [ $argv ])"
end
