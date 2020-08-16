# Defined in /home/philip/.config/fish/functions/findiap.fish @ line 2
function findiap --description 'find case insensitive anywhere (globbing) in folder (path)' --argument folder search
	# prevent out-of bounds if only two arguments
    if set -q argv[3]
        set more $argv[2..-1]
    end

    command find "$folder" -ipath "*$search*" $more
end
