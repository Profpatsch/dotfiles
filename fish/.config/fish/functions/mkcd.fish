function mkcd --description 'make folder and cd into it' --argument dir
	mkdir -p "$dir"
    and cd "$dir"
end
