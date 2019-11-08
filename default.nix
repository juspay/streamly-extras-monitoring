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
            sha256 = "1di6d2b9rxcgf6jrg57r0hhkasbr7b181v96a177spjw23j5sxv9";
            rev = "be920a2bfa906d85ad7c41b6e6b9a7ce731ac774";
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
