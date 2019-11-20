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
            rev = "v0.7.0";
            sha256 = "10qm72l7r4drqsajqrg3i1pqdi1bscz8p3k23vpi2ahrscd9kfdz";
            fetchSubmodules = true;
          }) { };

          streamly-extras = self.callCabal2nix "streamly-extras" (pkgs.fetchgit {
            url = "https://github.com/juspay/streamly-extras";
            rev = "a1d60ce8df90a9325a41a5ab6037812c0f31c2ed";
            sha256 = "0rc18y9qz0vwvnxpzz3f3gx2hj2fwyidk3r3hkdqb7pymskdniqm";
            fetchSubmodules = true;
          }) { };

        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.streamly-extras-monitoring
