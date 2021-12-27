let
  packages = import ./.;
  inherit (packages) pkgs cardano-dex-core;
  inherit (cardano-dex-core) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with cardano-dex-core; [
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
