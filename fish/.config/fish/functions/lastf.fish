function lastf --description 'print the file last created in dir or pwd' --argument dir
	if test -z $dir
    set dir (pwd)
  end
  echo (dirname "$dir")/(basename "$dir")/(ls -t "$dir" | head -n1)
end
