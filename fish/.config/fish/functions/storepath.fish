function storepath --description 'returns nix store path for given command in PATH' --argument path_cmd
	dirname (dirname (readlink (type -p "$path_cmd")))
end
