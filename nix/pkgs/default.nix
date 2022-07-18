{ pkgs
, sources
, haskell-nix
}:
let
  gitignore-nix = pkgs.callPackage pkgs."gitignore.nix" { };

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix;
    inherit (pkgs) libsodium-vrf;
    inherit pkgs;
  };

  hlint = pkgs.hlint;

  cabal-install = pkgs.cabal-install;

  stylish-haskell = pkgs.stylish-haskell;

  haskell-language-server = pkgs.haskell.haskell-language-server;

  cardano-repo-tool = pkgs.cardano-repo-tool;
in
{
  inherit haskell hlint cabal-install stylish-haskell haskell-language-server cardano-repo-tool;
}
