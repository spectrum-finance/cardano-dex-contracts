let
  # Pratically, the only needed dependency is the plutus repository.
  sources = import ./sources.nix {};

  # We're going to get everything from the main plutus repository. This ensures
  # we're using the same version of multiple dependencies such as nipxkgs,
  # haskell-nix, cabal-install, compiler-nix-name, etc.
  haskell-nix = import sources.haskellNix {};
  pkgs = import haskell-nix.sources.nixpkgs-unstable haskell-nix.nixpkgsArgs;

  cardano-dex-contracts = import ./pkgs {
    inherit pkgs sources haskell-nix;
  };

in
{
  inherit pkgs cardano-dex-contracts;
}
