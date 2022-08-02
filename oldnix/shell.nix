let
  packages = import ./.;
  inherit (packages) pkgs cardano-dex-contracts;
  inherit (cardano-dex-contracts) haskell;

in
haskell.project.shellFor {
  withHoogle = false;

  nativeBuildInputs = with cardano-dex-contracts; [
    hlint
    cabal-install
    haskell-language-server
    stylish-haskell
    pkgs.niv
    cardano-repo-tool
    pkgs.ghcid
    # HACK: This shouldn't need to be here.
    pkgs.lzma.dev
  ];
}
