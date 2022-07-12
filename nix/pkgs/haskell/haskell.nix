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
      "https://github.com/input-output-hk/ekg-forward"."297cd9db5074339a2fb2e5ae7d0780debb670c63" = "1zcwry3y5rmd9lgxy89wsb3k4kpffqji35dc7ghzbz603y1gy24g";
      "https://github.com/bitcoin-core/secp256k1"."ac83be33d0956faf6b7f61a60ab524ef7d6a473a" = "xltV3ECQ0oZhPIAlmn0WU1j2se1Px3ka/HQk6GZ764c=";
      "https://github.com/input-output-hk/cardano-addresses"."71006f9eb956b0004022e80aadd4ad50d837b621" = "11dl3fmq7ry5wdmz8kw07ji8yvrxnrsf7pgilw5q9mi4aqyvnaqk";
      "https://github.com/input-output-hk/cardano-base"."631cb6cf1fa01ab346233b610a38b3b4cba6e6ab" = "l0KgomRi6YhEoOlFnBYEXhnZO2+PW68rhfUrbMXjhCQ=";
      "https://github.com/input-output-hk/cardano-config"."e9de7a2cf70796f6ff26eac9f9540184ded0e4e6" = "1wm1c99r5zvz22pdl8nhkp13falvqmj8dgkm8fxskwa9ydqz01ld";
      "https://github.com/input-output-hk/cardano-crypto"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
      "https://github.com/protolude/protolude"."3e249724fd0ead27370c8c297b1ecd38f92cbd5b" = "JyHAQfTTUswP8MeGEZibx/2/v01Q7cU5mNpnmDazh24=";
      "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5" = "0avzyiqq0m8njd41ck9kpn992yq676b1az9xs77977h7cf85y4wm";
      "https://github.com/input-output-hk/cardano-node"."73f9a746362695dc2cb63ba757fbcabb81733d23" = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
      "https://github.com/mlabs-haskell/cardano-prelude"."713c7ae79a4d538fcd653c976a652913df1567b9" = "E+YSfUsvxdoOr7n7fz4xd7zb4z8XBRGNYOKipc2A1pw=";
      "https://github.com/input-output-hk/cardano-wallet"."f6d4db733c4e47ee11683c343b440552f59beff7" = "0gb3zyv3q5v5sd8r29s02yc0brwq5a01is9c0n528391n2r8g1yy";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/ouroboros-network"."4fac197b6f0d2ff60dc3486c593b68dc00969fbf" = "1b43vbdsr9m3ry1kgag2p2ixpv54gw7a4vvmndxl6knqg8qbsb8b";
      "https://github.com/input-output-hk/plutus"."b39a526e983cb931d0cc49b7d073d6d43abd22b5" = "3EBhSroECMOSP02qZGT0Zb3QHWibI/tYjdcaT5/YotY=";
      "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00" = "0da1vn2l6iyfxcjk58qal1l4755v92zi6yppmjmqvxf1gacyf9px";
      "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" = "10pb0yfp80jhb9ryn65a4rha2lxzsn2vlhcc6xphrrkf4x5lhzqc";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/Quid2/flat"."559617e058098b776b431e2a67346ad3adea2440" = "g+jGep1IXdw4q01W67J6f6OODY91QzIlW1+Eu8pR+u0=";
      "https://github.com/Plutonomicon/plutarch-plutus"."ea2166f4bc3e825c5613e4ddd808bf559c16dfd9" = "jJkIiNdZkk5wNt3gC7+XD+XtpSZtuIl3eEgATcMP9g4=";
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
