# Linux

Package {
    ensure => "installed",
}

package { "tmux": }
package { "zsh": }
package { "vim": }
package { "git": }


exec { "update_apt":
    command => "/usr/bin/apt-get update --fix-missing", }
exec { "binaries_to_path"
    command => "export PATH=$PATH:/vagrant", }

# Clojure
exec { "install_leiningen":
    command => "/usr/bin/wget -O /usr/bin/lein https://raw.github.com/technomancy/leiningen/stable/bin/lein && /bin/chmod a+x /usr/bin/lein",
    creates => "/usr/bin/lein",
}

exec { "install_drip":
    command => "/usr/bin/curl -L http://drip.flatland.org > /usr/bin/drip && /bin/chmod 755 /usr/bin/drip && echo \"export LEIN_JAVA_CMD=/usr/bin/drip\" >> /etc/profile",
    creates => "/usr/bin/drip",
}

package {"install_jre":
    name => "default-jdk",
}

#exec { "cljs-repl_to_path":
    #command => "echo \"export PATH=${PATH}:/vagrant/cljs-repl\" >> /etc/profile",
    #onlyif  => "[ -n \"$(which cljs-repl)\" ]",
    #path    => [],
#}
