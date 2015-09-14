function nps --description 'nix-env package search'
	nix-env -qaP | grep -i $argv;
end
