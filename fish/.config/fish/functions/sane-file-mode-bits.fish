function sane-file-mode-bits --argument path
	find "$path" -type d -exec chmod 755 "{}" +
    find "$path" -type f -exec chmod 644 "{}" +
end
