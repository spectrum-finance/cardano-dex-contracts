############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitignore-nix
, lib
, libsodium-vrf
, pkgs
}:

let
  project = pkgs.haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "cardano-dex-contracts";
      src = ../../../.;
    };

    compiler-nix-name = "ghc8102";

    sha256map = {
      "https://github.com/Quid2/flat.git"."559617e058098b776b431e2a67346ad3adea2440" = "g+jGep1IXdw4q01W67J6f6OODY91QzIlW1+Eu8pR+u0=";
      "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
      "https://github.com/input-output-hk/cardano-base"."631cb6cf1fa01ab346233b610a38b3b4cba6e6ab" = "l0KgomRi6YhEoOlFnBYEXhnZO2+PW68rhfUrbMXjhCQ=";
      "https://github.com/input-output-hk/cardano-prelude"."713c7ae79a4d538fcd653c976a652913df1567b9" = "E+YSfUsvxdoOr7n7fz4xd7zb4z8XBRGNYOKipc2A1pw=";
      "https://github.com/vshabanov/ekg-json"."00ebe7211c981686e65730b7144fbf5350462608" = "VT8Ur585TCn03P2TVi6t92v2Z6tl8vKijICjse6ocv8=";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/plutus"."b39a526e983cb931d0cc49b7d073d6d43abd22b5" = "3EBhSroECMOSP02qZGT0Zb3QHWibI/tYjdcaT5/YotY=";
      "https://github.com/input-output-hk/cardano-ledger"."ce3057e0863304ccb3f79d78c77136219dc786c6" = "SaMhULHXgY0FiSKWc2dAYlgtbfPaFh/bUTgGqoNnMqY=";
      "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
      "https://github.com/Plutonomicon/plutarch"."9176363e8c796c97b1068aa5d96de89f5e940fe9" = "GTvWg3nMtt91mN+0nsjyGaNP8v2m+Lg+x+/B9f7Ad4I=";
    };

    modules = [
      {
        packages = {
          # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
          plutus-ledger.doHaddock = false;
          plutus-use-cases.doHaddock = false;

          # See https://github.com/input-output-hk/iohk-nix/pull/488
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      }
    ];
  };
in
  project
