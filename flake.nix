{
  description = "ErgoDEX Cardano Dex Contracts";

  inputs = {
    # general inputs 
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-upstream.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "plutarch/nixpkgs";
    };

    ply = {
      url = "github:mlabs-haskell/ply/staging";
      inputs.extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };


    # onchain inputs
    plutarch.url = "github:Plutonomicon/plutarch/staging";

    # offchain inputs
    # plutip.url = "github:mlabs-haskell/plutip/gergely/vasil";

    # NOTE: this is the pendant to what was specified in the `cabal.project` file
    #       We just want the directory at the specific commit, so we set `flake = false`
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix/9a604d01bd4420ab7f396f14d1947fbe2ce7db8b";
      flake = false;
    };

    flat = {
      url = "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };

    plutus-apps = {
      url = "github:input-output-hk/plutus-apps/c330fc60d0cd174743c95de71e5c404b5008f186";
      flake = false;
    };

    purescript-bridge = {
      url = "github:input-output-hk/purescript-bridge/366fc70b341e2633f3ad0158a577d52e1cd2b138";
      flake = false;
    };

    servant-purescript = {
      url = "github:input-output-hk/servant-purescript/ebea59c7bdfc0338d83fca772b9a57e28560bcde";
      flake = false;
    };

    cardano-crypto = {
      url = "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };

    cardano-base = {
      url = "github:input-output-hk/cardano-base/654f5b7c76f7cc57900b4ddc664a82fc3b925fb0";
      flake = false;
    };

    cardano-prelude = {
      url = "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };

    cardano-addresses = {
      url = "github:input-output-hk/cardano-addresses/d2f86caa085402a953920c6714a0de6a50b655ec";
      flake = false;
    };

    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet/760140e238a5fbca61d1b286d7a80ece058dc729";
      flake = false;
    };

    ouroboros-network = {
      url = "github:input-output-hk/ouroboros-network/d613de3d872ec8b4a5da0c98afb443f322dc4dab";
      flake = false;
    };

    iohk-monitoring-framework = {
      url = "github:input-output-hk/iohk-monitoring-framework/46f994e216a1f8b36fe4669b47b2a7011b0e153c";
      flake = false;
      /*
        Are you thinking of updating this tag to some other commit?  Please
        ensure that the commit you are about to use is the latest one from
        the *develop* branch of this repo:
        * <https://github.com/input-output-hk/iohk-monitoring-framework/commits/develop>
        (not master!)
      
        In particular we rely on the code from this PR:
        * <https://github.com/input-output-hk/iohk-monitoring-framework/pull/622>
        being merged.
      */
    };

    cardano-ledger = {
      url = "github:input-output-hk/cardano-ledger/bf008ce028751cae9fb0b53c3bef20f07c06e333";
      flake = false;
    };

    cardano-node = {
      url = "github:input-output-hk/cardano-node/4f65fb9a27aa7e3a1873ab4211e412af780a3648";
      flake = false;
    };

    win32-network = {
      url = "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };

    goblins = {
      url = "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };

    plutus = {
      url = "github:input-output-hk/plutus/184f27c67dc696f4dfd558e0ccdfef0f054b519b";
      flake = false;
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , nixpkgs-upstream
    , haskell-nix
    , haskell-nix-extra-hackage
    , plutarch
    , ply
    , pre-commit-hooks
    , ...
    }:
    let
      # GENERAL

      plainNixpkgsFor = system: import nixpkgs-upstream { inherit system; };

      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      fourmoluFor = system: (plainNixpkgsFor system).haskell.packages.ghc923.fourmolu;


      # This adds 
      # - a shellHook (pre-commit-check.shellHook) attribute you can just add to your shell with the // operator 
      #   if you add the shellHook, in your shell a new command becomes available "`pre-commit`" which you can run 
      #   by hand with `pre-commit run --all` and gets run automatically before each commit
      # - a check (pre-commit-check) that will check formatting according to what you specified
      # - for further tooling this makes available please refer to https://github.com/cachix/pre-commit-hooks.nix
      #   (it is also quite easy to add tools by yourself)
      preCommitCheckFor = system:
        pre-commit-hooks.lib.${system}.run
          {
            src = ./.;
            hooks = {
              cabal-fmt.enable = true;
              fourmolu.enable = true;
              markdownlint.enable = true;
              nixpkgs-fmt.enable = true;
              shellcheck.enable = true;
              # hlint.enable = true;
              # statix.enable = true;
            };

            tools = {
              fourmolu = fourmoluFor system;
              hlint = (plainNixpkgsFor system).haskellPackages.hlint_3_4_1;
            };
          };

      # ONCHAIN
      # everything that is part of the onchain project; 
      # we have to use ghc923 because all other ghc versions are unsupported by 
      # plutarch
      onchain' = rec {
        nixpkgsFor = system: import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay (import "${plutarch.inputs.iohk-nix}/overlays/crypto") ];
        };

        ghcVersion = "923";
        compiler-nix-name = "ghc" + ghcVersion;

        myhackages = system: compiler-nix-name: haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name
          [
            "${plutarch}"
            "${plutarch}/plutarch-extra"
            "${plutarch}/plutarch-test"
            "${ply}/ply-core"
            "${ply}/ply-plutarch"
          ];

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = plainNixpkgsFor system;
            hackages = myhackages system compiler-nix-name;
          in
          pkgs.haskell-nix.cabalProject' (plutarch.applyPlutarchDep pkgs {
            src = ./cardano-dex-contracts-onchain;
            inherit compiler-nix-name;
            inherit (hackages) extra-hackages extra-hackage-tarballs modules;
            shell = {
              inherit (preCommitCheckFor system) shellHook;
              withHoogle = true;
              exactDeps = true;
              nativeBuildInputs = [
                pkgs'.cabal-install
                pkgs'.hlint
                pkgs'.haskellPackages.cabal-fmt
                pkgs'.nixpkgs-fmt
                plutarch.project.${system}.hsPkgs.hspec-discover.components.exes.hspec-discover
                (fourmoluFor system)
                (plutarch.hlsFor compiler-nix-name system)
              ];
            };
          });
      };

      # OFFCHAIN 
      # - plutip, bpi, PlutusTx, ...
      # - we can not use the same dependencies that 
      #   plutarch uses as they're usually are way more 
      #   current
      offchain' = rec {
        nixpkgsFor = system: import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay (import "${inputs.iohk-nix}/overlays/crypto") ];
        };

        ghcVersion = "8107";
        compiler-nix-name = "ghc" + ghcVersion;

        myhackages = system: compiler-nix-name: haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name
          [
            "${ply}/ply-core"

            "${inputs.cardano-addresses}/command-line"
            "${inputs.cardano-addresses}/core"
            "${inputs.cardano-base}/base-deriving-via"
            "${inputs.cardano-base}/binary"
            "${inputs.cardano-base}/binary/test"
            "${inputs.cardano-base}/cardano-crypto-class"
            "${inputs.cardano-base}/cardano-crypto-praos"
            "${inputs.cardano-base}/cardano-crypto-tests"
            "${inputs.cardano-base}/measures"
            "${inputs.cardano-base}/orphans-deriving-via"
            "${inputs.cardano-base}/slotting"
            "${inputs.cardano-base}/strict-containers"
            "${inputs.cardano-crypto}"
            "${inputs.cardano-prelude}/cardano-prelude"
            "${inputs.cardano-prelude}/cardano-prelude-test"
            "${inputs.cardano-wallet}/lib/cli"
            "${inputs.cardano-wallet}/lib/core"
            "${inputs.cardano-wallet}/lib/core-integration"
            "${inputs.cardano-wallet}/lib/dbvar"
            "${inputs.cardano-wallet}/lib/launcher"
            "${inputs.cardano-wallet}/lib/numeric"
            "${inputs.cardano-wallet}/lib/shelley"
            "${inputs.cardano-wallet}/lib/strict-non-empty-containers"
            "${inputs.cardano-wallet}/lib/test-utils"
            "${inputs.cardano-wallet}/lib/text-class"
            "${inputs.flat}"
            "${inputs.ouroboros-network}/io-classes"
            "${inputs.ouroboros-network}/io-sim"
            "${inputs.ouroboros-network}/monoidal-synchronisation"
            "${inputs.ouroboros-network}/network-mux"
            "${inputs.ouroboros-network}/ntp-client"
            "${inputs.ouroboros-network}/ouroboros-consensus"
            "${inputs.ouroboros-network}/ouroboros-consensus-byron"
            "${inputs.ouroboros-network}/ouroboros-consensus-cardano"
            "${inputs.ouroboros-network}/ouroboros-consensus-shelley"
            "${inputs.ouroboros-network}/ouroboros-network"
            "${inputs.ouroboros-network}/ouroboros-network-framework"
            "${inputs.ouroboros-network}/ouroboros-network-testing"
            "${inputs.ouroboros-network}/typed-protocols"
            "${inputs.ouroboros-network}/typed-protocols-cborg"
            "${inputs.ouroboros-network}/typed-protocols-examples"
            "${inputs.plutus-apps}/freer-extras"
            "${inputs.plutus-apps}/playground-common"
            "${inputs.plutus-apps}/plutus-chain-index"
            "${inputs.plutus-apps}/plutus-chain-index-core"
            "${inputs.plutus-apps}/plutus-contract"
            "${inputs.plutus-apps}/plutus-ledger"
            "${inputs.plutus-apps}/plutus-ledger-constraints"
            "${inputs.plutus-apps}/plutus-pab"
            "${inputs.plutus-apps}/plutus-use-cases"
            "${inputs.plutus-apps}/quickcheck-dynamic"
            "${inputs.purescript-bridge}"
            "${inputs.servant-purescript}"
            "${inputs.iohk-monitoring-framework}/iohk-monitoring"
            "${inputs.iohk-monitoring-framework}/tracer-transformers"
            "${inputs.iohk-monitoring-framework}/contra-tracer"
            "${inputs.iohk-monitoring-framework}/plugins/backend-aggregation"
            "${inputs.iohk-monitoring-framework}/plugins/backend-ekg"
            "${inputs.iohk-monitoring-framework}/plugins/backend-monitoring"
            "${inputs.iohk-monitoring-framework}/plugins/backend-trace-forwarder"
            "${inputs.iohk-monitoring-framework}/plugins/scribe-systemd"
            "${inputs.cardano-ledger}/byron/ledger/impl"
            "${inputs.cardano-ledger}/cardano-ledger-core"
            "${inputs.cardano-ledger}/cardano-protocol-tpraos"
            "${inputs.cardano-ledger}/eras/alonzo/impl"
            "${inputs.cardano-ledger}/eras/byron/chain/executable-spec"
            "${inputs.cardano-ledger}/eras/byron/crypto"
            "${inputs.cardano-ledger}/eras/byron/crypto/test"
            "${inputs.cardano-ledger}/eras/byron/ledger/executable-spec"
            "${inputs.cardano-ledger}/eras/byron/ledger/impl/test"
            "${inputs.cardano-ledger}/eras/shelley/impl"
            "${inputs.cardano-ledger}/eras/shelley-ma/impl"
            "${inputs.cardano-ledger}/eras/shelley/chain-and-ledger/executable-spec"
            "${inputs.cardano-ledger}/eras/shelley/test-suite"
            "${inputs.cardano-ledger}/shelley/chain-and-ledger/shelley-spec-ledger-test"
            "${inputs.cardano-ledger}/libs/non-integral"
            "${inputs.cardano-ledger}/libs/small-steps"
            "${inputs.cardano-ledger}/libs/cardano-ledger-pretty"
            "${inputs.cardano-ledger}/semantics/small-steps-test"
            "${inputs.cardano-node}/cardano-api"
            "${inputs.win32-network}"
            "${inputs.goblins}"
            "${inputs.plutus}/plutus-tx"
            "${inputs.plutus}/plutus-tx-plugin"
            "${inputs.plutus}/plutus-ledger-api"
            "${inputs.plutus}/plutus-core"
            "${inputs.plutus}/word-array"
            "${inputs.plutus}/prettyprinter-configurable"
            "${inputs.plutus}/stubs/plutus-ghc-stub"
          ];

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = plainNixpkgsFor system;
            # NOTE: this should be done equally for the onchain attribute; 
            #       however, our nixpkgs doesn't currently include a build for ghc923
            #       and we cannot use the hls of the upstream nixpkgs because that 
            #       breaks at runtime
            hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };

            hackages = myhackages system compiler-nix-name;

            project = pkgs.haskell-nix.cabalProject' {
              src = ./cardano-dex-contracts-offchain;
              inherit compiler-nix-name;
              inherit (hackages) extra-hackages extra-hackage-tarballs;

              index-state = "2021-10-20T00:00:00Z";

              cabalProjectLocal = ''
                package ply-core
                  flags: -new-ledger-namespace

                allow-newer:
                  size-based:template-haskell
                  , ouroboros-consensus-byron:formatting
                  , beam-core:aeson
                  , beam-sqlite:aeson
                  , beam-sqlite:dlist
                  , beam-migrate:aeson

                constraints:
                  -- big breaking change here, inline-r doens't have an upper bound
                  singletons < 3.0
                  -- bizarre issue: in earlier versions they define their own 'GEq', in newer
                  -- ones they reuse the one from 'some', but there isn't e.g. a proper version
                  -- constraint from dependent-sum-template (which is the library we actually use).
                  , dependent-sum > 0.6.2.0
                  -- Newer Hashable have instances for Set, which breaks beam-migrate
                  -- which declares its own instances of Hashable Set
                  , hashable < 1.3.4.0
              '';

              modules = [
                ({ _, ... }: {
                  packages = {
                    /* FIXME: do we need this in ergolabs? 
                      template-project-offchain.components.tests.template-project-offchain-test.build-tools = [
                      project.hsPkgs.cardano-cli.components.exes.cardano-cli
                      project.hsPkgs.cardano-node.components.exes.cardano-node
                      ];
                    */

                  };
                })
              ] ++ hackages.modules;

              shell = {
                inherit (preCommitCheckFor system) shellHook;
                withHoogle = true;

                exactDeps = true;

                # We use the ones from Nixpkgs, since they are cached reliably.
                # Eventually we will probably want to build these with haskell.nix.
                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.fd
                  pkgs'.haskellPackages.apply-refact
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.haskellPackages.fourmolu
                  pkgs'.hlint
                  pkgs'.nixpkgs-fmt

                  #project.hsPkgs.cardano-cli.components.exes.cardano-cli
                  #project.hsPkgs.cardano-node.components.exes.cardano-node

                  hls
                ];

                additional = ps: [
                  ps.ply-core
                  ps.plutus-ledger-api
                  ps.plutus-tx
                  ps.plutus-tx-plugin
                ];
              };
            };
          in
          project;
      };
    in
    rec {
      # NOTE: this is convenient for use in nix repl
      inherit plainNixpkgsFor;

      onchain = rec {
        # NOTE: this is the only place where the outputs use 
        #       the "onchain'" attributeset defined in the let
        project = perSystem onchain'.projectFor;
        flake = perSystem (system: project.${system}.flake { });
      };

      offchain = rec {
        # NOTE: this is the only place where the outputs use 
        #       the "offchain'" attributeset defined in the let
        project = perSystem offchain'.projectFor;
        flake = perSystem (system: project.${system}.flake { });
      };

      packages = perSystem (system:
        onchain.flake.${system}.packages
        // offchain.flake.${system}.packages
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.flake.${system}.devShell;
        tooling =
          let
            pkgs = plainNixpkgsFor system;
          in
          pkgs.mkShell {
            inherit (preCommitCheckFor system) shellHook;
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.fd
              pkgs.haskellPackages.apply-refact
              pkgs.haskellPackages.cabal-fmt
              pkgs.hlint
              pkgs.nixpkgs-fmt
              (fourmoluFor system)
            ];
          };
      });

      checks = perSystem (system:
        self.onchain.${system}.flake.checks
        // self.offchain.${system}.flake.checks
        // { formatCheck = preCommitCheckFor system; }
      );

      hydraJobs = {
        inherit (self) checks packages devShells;
      };
    };
}
