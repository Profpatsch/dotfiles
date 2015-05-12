# Oh My Zsh!
source ~/.zshrc.oh-my-zsh

setopt extendedglob # Heck yeah.

export PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/bin:/opt/android-sdk/platform-tools:/opt/android-sdk/tools:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/philip/.neo:/home/philip/scripts:/home/philip/.gem/ruby/1.9.1/bin/

# colorize stderr in red
 if [ -f "/usr/lib/libstderred.so" ]; then
	 export LD_PRELOAD="/usr/lib/libstderred.so"
	     fi


# set up mime types as suffix aliases
autoload -U zsh-mime-setup
zsh-mime-setup

# fasd
eval "$(fasd --init auto)"
alias v='f -e choose-editor' # quick opening files with the enabled editor
alias m='f -e vlc' # quick opening files with mplayer
alias o='a -e xdg-open' # quick opening files with xdg-open

# Own aliases
# Convenience
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias findn='find . -iname '
alias diff='colordiff '
#alias cl='wc -l ' # Count Lines
# Own “programs” (fancy for: cheap alias names)
alias cd2flac='abcde -c ~/.abcde_flac.conf'
alias copy='rsync -a --progress --stats '
alias move='rsync -a --progress --stats --remove-source-files '
alias deb='cgdb ' # I can’t remember that name…
alias follow='tail -n50 -f '
alias followl='tail -n50 -f ~/.logs/*'
alias syncnyx='unison nyx -logfile ~/.logs/unison.log'
alias busy='rnd_file=$(find /usr/include -type f -size +5k | sort -R | head -n 1) && vim +$((RANDOM%$(wc -l $rnd_file | cut -f1 -d" "))) $rnd_file' # makes you look busy
alias cf='find | wc -l ' # Count Files in directory recursively
alias tb='nc termbin.com 9999'
alias xclipc='xclip -selection clipboard '
alias imclip='function { imup "$1" | xclipc }'

# Global convenience stuff
alias -g G='| grep '

# Make these programs act sane
# cp
alias cp='cp -r '
# Git
# gm stays graphics magick, please.
unalias gm
alias gs='git status '
#show only staged (first m) and changed files
alias gss='gs -s| grep "^[^?][^?]"'
alias gh='git hist '
alias gha='git hist --all '
alias ga='git add '
alias gb='git branch '
alias gc='git commit '
alias gcm='git commit -m '
alias gd='git diff '
#alias go='git checkout '
alias gk='gitk --all&'
alias gx='gitx --all '
alias gcam='git commit -a -m '
alias gl='git lola'
alias gpo='git push origin'
alias gpom='git push origin master '
# git-svn
alias gsd='git svn dcommit'
alias gsr='git svn rebase'
alias grc='git rebase --continue'
alias gra='git rebase --abort'
# svn
alias sc='svn commit '
alias scm='svn commit -m '
#alias ss='svn status ' # Don’t need no sockets. ;)
# vim
# open new files in already running session of gvim
alias gvim='choose-editor '
# open the session file (I always name these .sess)
alias vims='vim -S .sess '
# graphics magick
alias gmc='gm convert '
alias crop='gm convert - -gravity Center -crop '

# typos
alias l¿='ls ' # Haha, Neo. ;)

# system
alias standby='systemctl suspend'
#alias sudo='sudo '

# Arch
alias pacclean='pacman -Rs $(pacman -Qqtd)' # Removes all orphan packages
compdef _yaourt yaourt=pacman

# Debian-like
alias sa='sudo aptitude '
alias sapti='sudo aptitude install '

# ls colors solarized
eval `dircolors ~/.config/terminator/solarized/dircolors.ansi-dark`

# C
go_libs="-lm"
go_flags="-g -Wall -include ~/scripts/allheaders.h -O3"
alias goc="c99 -xc '-' $go_libs $go_flags"

# OPAM configuration
. /home/philip/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

## Functions

#f5# Create Directoy and \kbd{cd} to it
mkcd() {
    if (( ARGC != 1 )); then
        printf 'usage: mkcd <new-directory>\n'
        return 1;
    fi
    if [[ ! -d "$1" ]]; then
        command mkdir -p "$1"
    else
        printf '`%s'\'' already exists: cd-ing.\n' "$1"
    fi
    builtin cd "$1"
}

