{
  description = "llvm-codegen: LLVM code generation using Haskell";

  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";

  outputs = { self, flake-utils, nix-filter, devshell, nixpkgs }:
    with nixpkgs.lib;
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        rmDot = replaceStrings [ "." ] [ "" ];
        supportedGHCs = [ "default" "902" "922" ];
        config = { };
        overlays.devshell = devshell.overlay;
        overlays.default = f: p:
          let
            ghcVersion = "ghc${rmDot p.haskellPackages.ghc.version}";

            mkHaskellPackages = hspkgs:
              hspkgs.extend (hf: hp:
                with f.haskell.lib;
                composeExtensions (hf: hp: {
                  llvm-codegen = disableLibraryProfiling
                    ((hf.callCabal2nix "llvm-codegen" (with nix-filter.lib;
                      filter {
                        root = self;
                        exclude = [ (matchExt "cabal") ];
                      }) { }).overrideAttrs (old: {
                        version = "${rmDot hp.ghc.version}-${old.version}-${
                            substring 0 8 self.lastModifiedDate
                          }.${self.shortRev or "dirty"}";
                      }));
                }) (hf: hp: { llvm-config = f.llvmPackages_14.llvm; }) hf hp);

            # all haskellPackages
            allHaskellPackages = let
              cases = listToAttrs (map (n: {
                name = "${n}";
                value = mkHaskellPackages
                  f.haskell.packages."${if n == "default" then
                    "${ghcVersion}"
                  else
                    "ghc${n}"}";
              }) supportedGHCs);
            in cases;

            # all packages
            allPackages = listToAttrs (map (n: {
              name = if n == "default" then n else "llvm-codegen-${n}";
              value = allHaskellPackages."${n}".llvm-codegen;
            }) supportedGHCs);

            # make dev shell
            mkDevShell = g:
              p.devshell.mkShell {
                name = "llvm-codegen-${
                    if g == "default" then "${ghcVersion}" else g
                  }-${substring 0 8 self.lastModifiedDate}.${
                    self.shortRev or "dirty"
                  }";
                packages = with f;
                  with f.allHaskellPackages."${g}"; [
                    ghcid
                    llvmPackages_13.llvm.dev
                    (ghcWithPackages (hp:
                      with hp; [
                        llvm-codegen
                        ghc
                        cabal-install
                        haskell-language-server
                        hpack
                        hsc2hs
                      ]))
                  ];
              };

            # all packages
            allDevShells = listToAttrs (map (n: {
              name = "${n}";
              value = mkDevShell n;
            }) supportedGHCs);
          in {
            haskellPackages = allHaskellPackages.default;
            inherit allHaskellPackages allDevShells allPackages;
          };

        pkgs = import nixpkgs {
          inherit system config;
          overlays = [ overlays.devshell overlays.default ];
        };

      in with pkgs.lib; rec {
        inherit overlays;
        packages = flattenTree (pkgs.recurseIntoAttrs pkgs.allPackages);
        devShells = flattenTree (pkgs.recurseIntoAttrs pkgs.allDevShells);
      });
}
