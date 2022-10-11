self: super:
let
  ghcVersion = "9.0.2";
  ghcName = "ghc${builtins.replaceStrings ["."] [""] ghcVersion}";
  ghcPatches =
    if super.stdenv.targetPlatform.isDarwin
    then [
      # Copied from https://github.com/NixOS/nixpkgs/pull/149942
      # If the GHC version is updated, we must update the patch URL too.
      # ---
      # Reverts the linking behavior of GHC to not resolve `-libc++` to `c++`.
      # Without this, we get the following error on macOS:
      #    ghc: loadArchive: Neither an archive, nor a fat archive: `/path/to/clang++'
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/input-output-hk/haskell.nix/2234bd36abb33050dc5441d35e9dfb1533278549/overlays/patches/ghc/ghc-9.0-macOS-loadArchive-fix.patch";
        sha256 = "aKz5Uka5RDS/N/ae3bUrYTPNLeVg2H//wOUJRSDVL4Q=";
      })
    ] else [ ];
in
{
  haskell = super.haskell // {
    compiler = super.haskell.compiler // {
      ${ghcName} = (super.haskell.compiler.${ghcName}).overrideAttrs (oldAttrs: {
        patches = (if oldAttrs ? patches then oldAttrs.patches else [ ]) ++ ghcPatches;
      });
    };

    packages = super.haskell.packages // {
      ${ghcName} = super.haskell.packages.${ghcName}.override {
        overrides = hself: hsuper:
          if super.stdenv.targetPlatform.isDarwin
          then
          # macOS-specific overrides:
            let
              # Ormolu v0.5.0.1 doesn't build correctly on aarch64-darwin.
              # Disabling the "fixity-th" flag seems to fix it.
              # https://github.com/tweag/ormolu/issues/927
              fixOrmolu = p: super.lib.pipe p [
                (super.haskell.lib.compose.addExtraLibrary hself.file-embed)
                (super.haskell.lib.compose.disableCabalFlag "fixity-th")
              ];
            in
            {
              # On aarch64-darwin, this creates a cycle for some reason; didn't look too much into it.
              ghcid = super.haskell.lib.overrideCabal hsuper.ghcid (drv: { enableSeparateBinOutput = false; });
              # See above.
              ormolu = hself.ormolu_0_5_0_1;
              ormolu_0_5_0_1 = fixOrmolu hsuper.ormolu_0_5_0_1;
              fourmolu = hself.fourmolu_0_8_2_0;
              fourmolu_0_8_2_0 = fixOrmolu hsuper.fourmolu_0_8_2_0;
            }
          else
          # We don't need to override anything on Linux:
            { };
      };
    };
  };

  inherit ghcVersion ghcName;
}

