function nspman
	if test -z "$argv[2]"
    set cmd "$argv[1]"
  else
    set cmd "$argv[2]"
  end
  nix-shell -p "$argv[1]" --run "man $cmd"
end
