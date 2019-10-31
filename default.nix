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
            sha256 = "0qrsq2bnwchs8zjl4icazwhp76nlgfq8wbag2ip221y6h30vmrxi";
            rev = "8a4d965a93b375cb62afb66b74580c751f2ce3ff";
            fetchSubmodules = true;
          }) { };

          streamly-extras = self.callCabal2nix "streamly-extras" (pkgs.fetchgit {
            url = "https://github.com/juspay/streamly-extras";
            sha256 = "0ib0qvnkpzdzalxig4f9bnx5mkd4sri9sdrm403h6rxj18kkfr8k";
            rev = "faea671b14560726ff780136828a57b3be8cdc47";
            fetchSubmodules = true;
          }) { };

        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.streamly-extras-monitoring
