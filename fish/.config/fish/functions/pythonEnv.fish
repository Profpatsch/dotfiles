function pythonEnv --description 'start a nix-shell with the given python packages' --argument pythonVersion
	if set -q argv[2]
    set argv $argv[2..-1]
  end
  set argv python $argv
  for el in $argv
    set ppkgs $ppkgs "python"$pythonVersion"Packages.$el"
  end

  nix-shell -p pythonFull $ppkgs
end
