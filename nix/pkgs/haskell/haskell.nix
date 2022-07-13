############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitignore-nix
, compiler-nix-name
, lib
, libsodium-vrf
}:

let
  project = haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-dex-contracts";
      src = ../../../.;
    };

    inherit compiler-nix-name;

    sha256map = {
      "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/purescript-bridge.git"."47a1f11825a0f9445e0f98792f79172efef66c00" = "0da1vn2l6iyfxcjk58qal1l4755v92zi6yppmjmqvxf1gacyf9px";
      "https://github.com/input-output-hk/servant-purescript.git"."44e7cacf109f84984cd99cd3faf185d161826963" = "10pb0yfp80jhb9ryn65a4rha2lxzsn2vlhcc6xphrrkf4x5lhzqc";
      "https://github.com/input-output-hk/cardano-crypto.git"."f73079303f663e028288f9f4a9e08bcca39a923e" = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
      "https://github.com/input-output-hk/cardano-base"."0f3a867493059e650cda69e20a5cbf1ace289a57" = "4b0keLjRaVSdEwfBXB1iT3QPlsutdxSltGfBufT4Clw=";
      "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" = "sha256-kgX3DKyfjBb8/XcDEd+/adlETsFlp5sCSurHWgsFAQI=";
      "https://github.com/input-output-hk/cardano-addresses"."b6f2f3cef01a399376064194fd96711a5bdba4a7" = "hYAvI7KlFnFRjMG8/JvDl733YnQUE1O26VMcr94h0oM=";
      "https://github.com/input-output-hk/cardano-wallet"."a73d8c9717dc4e174745f8568d6f3fe84f0f9d76" = "ncoAaIPWRhJ2FShesmrp4q5LK1PtWuzqOKuhlwerWac=";
      "https://github.com/input-output-hk/ouroboros-network"."a65c29b6a85e90d430c7f58d362b7eb097fd4949" = "bmLD5sFsiny/eRv6MHrqGvo6I4QG9pO0psiHWGFZqro=";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."066f7002aac5a0efc20e49643fea45454f226caa" = "0ia5UflYEmBYepj2gkJy9msknklI0UPtUavMEGwk3Wg=";
      "https://github.com/input-output-hk/cardano-node.git"."1.35.0" = "R7YGQ6UMG16ed9sGguDWq2cUgFnADeRdx8O2s2HqWRk=";
      "https://github.com/haskell-works/hw-aeson"."d99d2f3e39a287607418ae605b132a3deb2b753f" = "xO4/zPMBmZtBXFwHF8p3nw4TilrJHxH54mfg9CRnuO8=";
      "https://github.com/vshabanov/ekg-json"."00ebe7211c981686e65730b7144fbf5350462608" = "VT8Ur585TCn03P2TVi6t92v2Z6tl8vKijICjse6ocv8=";
      "https://github.com/input-output-hk/ekg-forward"."297cd9db5074339a2fb2e5ae7d0780debb670c63" = "jwj/gh/A/PXhO6yVESV27k4yx9I8Id8fTa3m4ofPnP0=";
      "https://github.com/input-output-hk/typed-protocols"."181601bc3d9e9d21a671ce01e0b481348b3ca104" = "5Wof5yTKb12EPY6B8LfapX18xNZZpF+rvhnQ88U6KdM=";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/hedgehog-extras"."967d79533c21e33387d0227a5f6cc185203fe658" = "sha256-TR9i1J3HUYz3QnFQbfJPr/kGDahxZPojDsorYtRZeGU=";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/plutus"."b39a526e983cb931d0cc49b7d073d6d43abd22b5" = "3EBhSroECMOSP02qZGT0Zb3QHWibI/tYjdcaT5/YotY=";
      "https://github.com/input-output-hk/quickcheck-dynamic"."c272906361471d684440f76c297e29ab760f6a1e" = "TioJQASNrQX6B3n2Cv43X2olyT67//CFQqcpvNW7N60=";
      "https://github.com/raduom/hysterical-screams"."4c523469e9efd3f0d10d17da3304923b7b0e0674" = "d4N3rUzg45BUs5Lx/kK7vXYsLMNoO15dlzo7t8lGIXA=";
      "https://github.com/input-output-hk/io-sim"."57e888b1894829056cb00b7b5785fdf6a74c3271" = "TviSvCBEYtlKEo9qJmE8pCE25nMjDi8HeIAFniunaM8=";
      "https://github.com/input-output-hk/cardano-ledger"."ce3057e0863304ccb3f79d78c77136219dc786c6" = "SaMhULHXgY0FiSKWc2dAYlgtbfPaFh/bUTgGqoNnMqY=";
      "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
      "https://github.com/Plutonomicon/plutarch"."fcc8ed8f757e9c3bcb60e83001c84b15625333fb" = "Gy2j68bl7LalFJsnH1Ie/XvzJQC/RZ1WQy7qoAsLDoI=";
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
