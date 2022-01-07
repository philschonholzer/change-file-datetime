with import <nixpkgs> {};

mkShell {
  buildInputs = [
    cabal-install
    ghcid
  ];
}