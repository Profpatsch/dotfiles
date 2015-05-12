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

# Python
export WORKON_HOME=~/.virtualenvs
source /usr/bin/virtualenvwrapper.sh

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


# utility functions
# this function checks if a command exists and returns either true
# or false. This avoids using 'which' and 'whence', which will
# avoid problems with aliases for which on certain weird systems. :-)
# Usage: check_com [-c|-g] word
#   -c  only checks for external commands
#   -g  does the usual tests and also checks for global aliases
check_com() {
    emulate -L zsh
    local -i comonly gatoo

    if [[ $1 == '-c' ]] ; then
        (( comonly = 1 ))
        shift
    elif [[ $1 == '-g' ]] ; then
        (( gatoo = 1 ))
    else
        (( comonly = 0 ))
        (( gatoo = 0 ))
    fi

    if (( ${#argv} != 1 )) ; then
        printf 'usage: check_com [-c] <command>\n' >&2
        return 1
    fi

    if (( comonly > 0 )) ; then
        [[ -n ${commands[$1]}  ]] && return 0
        return 1
    fi

    if   [[ -n ${commands[$1]}    ]] \
      || [[ -n ${functions[$1]}   ]] \
      || [[ -n ${aliases[$1]}     ]] \
      || [[ -n ${reswords[(r)$1]} ]] ; then

        return 0
    fi

    if (( gatoo > 0 )) && [[ -n ${galiases[$1]} ]] ; then
        return 0
    fi

    return 1
}


# Usage: simple-extract <file>
# Using option -d deletes the original archive file.
#f5# Smart archive extractor
simple-extract() {
    emulate -L zsh
    setopt extended_glob noclobber
    local DELETE_ORIGINAL DECOMP_CMD USES_STDIN USES_STDOUT GZTARGET WGET_CMD
    local RC=0
    zparseopts -D -E "d=DELETE_ORIGINAL"
    for ARCHIVE in "${@}"; do
        case $ARCHIVE in
            *(tar.bz2|tbz2|tbz))
                DECOMP_CMD="tar -xvjf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *(tar.gz|tgz))
                DECOMP_CMD="tar -xvzf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *(tar.xz|txz|tar.lzma))
                DECOMP_CMD="tar -xvJf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *tar)
                DECOMP_CMD="tar -xvf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *rar)
                DECOMP_CMD="unrar x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *lzh)
                DECOMP_CMD="lha x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *7z)
                DECOMP_CMD="7z x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *(zip|jar))
                DECOMP_CMD="unzip"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *deb)
                DECOMP_CMD="ar -x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *bz2)
                DECOMP_CMD="bzip2 -d -c -"
                USES_STDIN=true
                USES_STDOUT=true
                ;;
            *(gz|Z))
                DECOMP_CMD="gzip -d -c -"
                USES_STDIN=true
                USES_STDOUT=true
                ;;
            *(xz|lzma))
                DECOMP_CMD="xz -d -c -"
                USES_STDIN=true
                USES_STDOUT=true
                ;;
            *)
                print "ERROR: '$ARCHIVE' has unrecognized archive type." >&2
                RC=$((RC+1))
                continue
                ;;
        esac

        if ! check_com ${DECOMP_CMD[(w)1]}; then
            echo "ERROR: ${DECOMP_CMD[(w)1]} not installed." >&2
            RC=$((RC+2))
            continue
        fi

        GZTARGET="${ARCHIVE:t:r}"
        if [[ -f $ARCHIVE ]] ; then

            print "Extracting '$ARCHIVE' ..."
            if $USES_STDIN; then
                if $USES_STDOUT; then
                    ${=DECOMP_CMD} < "$ARCHIVE" > $GZTARGET
                else
                    ${=DECOMP_CMD} < "$ARCHIVE"
                fi
            else
                if $USES_STDOUT; then
                    ${=DECOMP_CMD} "$ARCHIVE" > $GZTARGET
                else
                    ${=DECOMP_CMD} "$ARCHIVE"
                fi
            fi
            [[ $? -eq 0 && -n "$DELETE_ORIGINAL" ]] && rm -f "$ARCHIVE"

        elif [[ "$ARCHIVE" == (#s)(https|http|ftp)://* ]] ; then
            if check_com curl; then
                WGET_CMD="curl -L -k -s -o -"
            elif check_com wget; then
                WGET_CMD="wget -q -O - --no-check-certificate"
            else
                print "ERROR: neither wget nor curl is installed" >&2
                RC=$((RC+4))
                continue
            fi
            print "Downloading and Extracting '$ARCHIVE' ..."
            if $USES_STDIN; then
                if $USES_STDOUT; then
                    ${=WGET_CMD} "$ARCHIVE" | ${=DECOMP_CMD} > $GZTARGET
                    RC=$((RC+$?))
                else
                    ${=WGET_CMD} "$ARCHIVE" | ${=DECOMP_CMD}
                    RC=$((RC+$?))
                fi
            else
                if $USES_STDOUT; then
                    ${=DECOMP_CMD} =(${=WGET_CMD} "$ARCHIVE") > $GZTARGET
                else
                    ${=DECOMP_CMD} =(${=WGET_CMD} "$ARCHIVE")
                fi
            fi

        else
            print "ERROR: '$ARCHIVE' is neither a valid file nor a supported URI." >&2
            RC=$((RC+8))
        fi
    done
    return $RC
}
