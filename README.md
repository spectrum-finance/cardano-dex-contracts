# Spectrum Finance contracts implementation for Cardano

This repository contains implementations of the following scripts:
* CFMM DEX scripts
   - Pool
   - Swap proxy
   - Deposit proxy
   - Redeem proxy

## Setting up

### Cabal+Nix build

Alternatively, use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and run: 

- `nix develop .#onchain` for the onchain part of the project (plutarch)
- `nix develop .#offchain` for the offchain part of the project
- `nix develop .#tooling` to be dropped into a generic tooling flake that provides formatters/ linters/ ...


It is strongly recommend to use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```bash
$ echo "use flake .#tooling" > .envrc # Or manually add "use flake .#tooling" in .envrc if you already have one
# ^ in the root directory of the project 
$ direnv allow

$ echo "use flake .#onchain" > .envrc 
# ^ in the onchain directory of the project (and in the same way for the offchain project)
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter the corresponding directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

To build the project with nix, run: 
- `nix build .#"cardano-dex-contracts-onchain:lib:cardano-dex-contracts-onchain"` for the onchain part
- `nix build .#"cardano-dex-contracts-offchain:lib:cardano-dex-contracts-offchain"` for the offchain part
