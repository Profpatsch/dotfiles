# Defined in /tmp/fish.Z71VmL/ytextr.fish @ line 2
function ytextr
	nix run -f channel:nixos-unstable youtube-dl \
    -c youtube-dl \
      --no-playlist \
      --write-sub \
      --all-subs \
      --embed-subs \
      --merge-output-format mkv \
      -f 'bestvideo[height<=?1080]+bestaudio/best' \
      $argv
  # youtube-dl --no-playlist -x --audio-format=vorbis "$argv[1]"
end
