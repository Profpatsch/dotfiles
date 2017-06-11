function mkPlaylist
	set FILE (mktemp --suffix=".m3u")
	find "$argv[1]" -type f > "$FILE"
	echo "$FILE"
end
