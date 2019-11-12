{ withHoogle ? false
}:
let
  nixpkgs = import ./nixpkgs.nix;
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          ghc =
            super.ghc // { withPackages = if withHoogle then super.ghc.withHoogle else super.ghc ; };
          ghcWithPackages =
            self.ghc.withPackages;
          streamly-extras-monitoring =
            self.callCabal2nix "streamly-extras-monitoring" (pkgs.lib.cleanSource ./.) { };

          streamly = self.callCabal2nix "streamly" (pkgs.fetchgit {
            url = "https://github.com/composewell/streamly.git";
            sha256 = "11bl9a1j5nmfys4h519j5v8m4sa24pjdfabaaqcmpfp4pl0lgqwr";
            rev = "bea83e96103d51c9571ce560a75ab3031cb14fa6";
            fetchSubmodules = true;
          }) { };

          streamly-extras = self.callCabal2nix "streamly-extras" (pkgs.fetchgit {
            url = "https://github.com/juspay/streamly-extras";
            sha256 = "1i50iy52fgq7gwdfil06n9hagsmklia41vg33idzrx80vf44xckq";
            rev = "3b950a04655a357f2779660390094027d52cea9e";
            fetchSubmodules = true;
          }) { };

        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.streamly-extras-monitoring
