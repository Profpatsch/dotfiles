# Defined in /tmp/fish.6dQrtG/mkPlaylist.fish @ line 2
function mkPlaylist
	set FILE (mktemp --suffix=".m3u")
  for dir in $argv
    find "$dir" \
      -type f \
      -and \
      \( -not \
         \( -iname "*.cue" \
        -or -iname "*.log" \
        -or -iname "*.png" \
        -or -iname "*.jpg" \
        -or -iname "*.mp4" \
        -or -iname "*.mkv" \) \) \
      >> "$FILE"
  end
	echo "$FILE"
end
