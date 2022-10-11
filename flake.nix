{
  description = "llvm-codegen: LLVM code generation using Haskell";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
    nf.url = "github:numtide/nix-filter?ref=master";
  };
  outputs = { self, np, fu, ds, nf, ... }@inputs:
    with np.lib;
    with fu.lib;
    eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        ghcVersion = "902";
        llvmVersion = 14;
        version = "${ghcVersion}.${substring 0 8 self.lastModifiedDate}.${
            self.shortRev or "dirty"
          }";
        config = { };
        overlay = final: _:
          let
            haskellPackages =
              final.haskell.packages."ghc${ghcVersion}".override {
                overrides = with final.haskell.lib;
                  hf: hp:
                  let llvm = final."llvmPackages_${toString llvmVersion}".llvm;
                  in {
                    llvm-codegen = appendConfigureFlags
                      ((hf.callCabal2nix "llvm-codegen" (with nf.lib;
                        filter {
                          root = self;
                          exclude = [ ("Setup.hs") ];
                        }) { llvm-config = llvm; }).overrideAttrs (old: {
                               version = "${old.version}-${version}";
                             })) [
                          "--ghc-option=-optl=-L/${llvm}/lib"
                          "--ghc-option=-optl=-I/${llvm}/include"
                          "--ghc-option=-optl=-lLLVM-${toString llvmVersion}"
                        ];
                  };
              };
          in { inherit haskellPackages; };

        pkgs = import np {
          inherit config;
          system = if system == "aarch64-darwin"
                   then "x86_64-darwin"
                   else system;
          overlays = [ overlay ds.overlay (import ./nix/overlays/ghc.nix)];
        };
      in with pkgs.lib; rec {
        inherit overlay;
        packages = { inherit (pkgs.haskellPackages) llvm-codegen; };
        defaultPackage = packages.llvm-codegen;
        devShell = pkgs.devshell.mkShell {
          name = "llvm-codegen";
          imports = [ ];
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
