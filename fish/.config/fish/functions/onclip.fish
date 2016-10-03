function onclip --argument cmd
	eval $cmd \"(xclip -o -selection clipboard)\"
end
