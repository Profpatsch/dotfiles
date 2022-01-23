{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.ghc.withHoogle (hps: [
      hps.xmonad
      hps.xmonad-contrib
    ]))
    pkgs.haskell-language-server
  ];
}
