{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  allowBroken = true;

  f = { mkDerivation, base, base16, binary, bytestring, conduit
      , conduit-extra, containers, cryptonite, groups, hpack, hspec
      , hspec-megaparsec, http-client, http-client-tls, http-conduit, lib
      , megaparsec, memory, mtl, murmur3, network, text, time, transformers
      }:
      mkDerivation {
        pname = "haskoin";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base16 binary bytestring conduit conduit-extra containers
          cryptonite groups http-client http-client-tls http-conduit
          megaparsec memory mtl murmur3 network text time transformers
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base base16 binary bytestring conduit conduit-extra containers
          cryptonite groups hspec http-client http-client-tls http-conduit
          megaparsec memory mtl murmur3 network text time transformers
        ];
        testHaskellDepends = [
          base base16 binary bytestring conduit conduit-extra containers
          cryptonite groups hspec hspec-megaparsec http-client
          http-client-tls http-conduit megaparsec memory mtl network text
          time transformers
        ];
        prePatch = "hpack";
        homepage = "https://github.com/lambdaeon/haskoin#readme";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
in

  if pkgs.lib.inNixShell then drv.env else drv
