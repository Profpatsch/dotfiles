function mpvpls
	mpv --shuffle --playlist (mkPlaylist "$argv[1]" | psub)
end
