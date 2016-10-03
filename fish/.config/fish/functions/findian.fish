function findian --description 'find case insensitive anywhere (globbing) in folder' --argument folder search
	# prevent out-of bounds if only two arguments
  if set -q argv[3]
    set more $argv[2..-1]
  end

  command find "$folder" -iname "*$search*" $more
end
