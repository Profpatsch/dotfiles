# These are…

… my humble dotfiles and general home-directory-shenanigans.  
Feel free to look around, I try to keep them clean and commented.

[**Arch**][arch] or [**NixOS**][nixos] **users** might find them especially
useful. Lots of tooling/config already went into my [nix config][vuizvui], so
that might be worth a look as well.

They are managed with [GNU stow][stow]. To try out the config files for one 
program, pull the repo, cd into it and type `stow <name of program>`. It will 
inform you if there are existing config files you need to backup. Dotfiles with
sensitive contents are in a different repo, managed with stow as well, meaning
some parts of a working config might be missing here.

I’m a native user of the [neo layout][neowiki], so some shortcuts could reflect 
that. ([Neo homepage][neo] (German))  On another note, why are you still typing 
querty anyway? It’s a big pile of shit, to put it nicely.

## License

Since I publish these files in the hope that they are useful, everything (unless
otherwise noted) is under

```
Creative Commons CC0 1.0
```

Full license text: https://creativecommons.org/publicdomain/zero/1.0/legalcode

[arch]: https://archlinux.org/
[nixos]: https://nixos.org/
[vuizvui]: https://github.com/openlab-aux/vuizvui/blob/master/machines/profpatsch/katara.nix
[neo]:  https://neo-layout.org/
[neowiki]: https://en.wikipedia.org/wiki/Keyboard_layout#Neo
[stow]: https://www.gnu.org/software/stow/
