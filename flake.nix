{
  description =
    "llvm-codegen: LLVM code generation using Haskell";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
  };
  outputs = { self, np, fu, ds }:
    with np.lib;
    with fu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        ghcVersion = "902";
        version = "${ghcVersion}.${substring 0 8 self.lastModifiedDate}.${
            self.shortRev or "dirty"
          }";
        config = { };
        overlay = final: _:
          let
            haskellPackages =
              final.haskell.packages."ghc${ghcVersion}".override {
                overrides = hf: hp: {
                  llvm-codegen =
                    (hf.callCabal2nix "llvm-codegen" ./. {
                      llvm-config = final.llvmPackages_14.llvm;
                    });
                };
              };
          in { inherit haskellPackages; };

        pkgs = import np {
          inherit system config;
          overlays = [ overlay ds.overlay ];
        };
      in with pkgs.lib; rec {
        inherit overlay;
        packages = { inherit (pkgs.haskellPackages) llvm-codegen; };
        defaultPackage = packages.llvm-codegen;
        devShell = pkgs.devshell.mkShell {
          name = "llvm-codegen";
          imports = [];
          packages = with pkgs;
            with haskellPackages; [
              pkgs.llvmPackages_14.llvm.dev
              pkgs.ghcid
              (ghcWithPackages (p:
                with p; [
                  hspec-discover
                  ghc
                  cabal-install
                  hsc2hs
                  hpack
                  haskell-language-server
                ]))
            ];
        };
      });
}
