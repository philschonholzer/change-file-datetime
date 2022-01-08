with import <nixpkgs> {};

mkShell {
  buildInputs = [
    cabal-install
    ghcid
  ];
  shellHook = ''
    export IN_NIX_SHELL=""
  '';
}