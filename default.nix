{ withHoogle ? false
}:
let
  nixpkgs = import ./nixpkgs.nix;
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          ghc = super.ghc // { withPackages = if withHoogle then super.ghc.withHoogle else super.ghc ; };

          ghcWithPackages = self.ghc.withPackages;

          streamly-extras-monitoring = self.callCabal2nix "streamly-extras-monitoring" (pkgs.lib.cleanSource ./.) { };

          streamly-extras = self.callCabal2nix "streamly-extras" (pkgs.fetchgit {
            url = "https://github.com/juspay/streamly-extras";
            rev = "560117cf20d04b1d5a48cefc1d4d046114af1fa6";
            sha256 = "14niq7n456yd9s2z69irxcdbfvnjkkgl0a071njdr8n6h0hck026";
            fetchSubmodules = true;
          }) { };

          fusion-plugin-types = self.callCabal2nix "fusion-plugin-types" (pkgs.fetchgit {
            url = "https://github.com/composewell/fusion-plugin-types.git";
            rev = "1a7e1c39b4496543b2dc95d59aafbf44041554f1";
            sha256 = "1mmph6gawi4cbsqmswawi1c951f6pq41qfqjbvnygm36d2qfv64i";
            fetchSubmodules = true;
            }) { };

          streamly = self.callCabal2nix "streamly" (pkgs.fetchgit {
            url = "https://github.com/composewell/streamly.git";
            rev = "500d187b8fb5b6b1bc424725dd179650fb41c49c";
            sha256 = "175cqn0sxg8r32f3dd0alkivkvjrwhmwm7xkmf4ki2j68smmjc3y";
            fetchSubmodules = true;
          }) { };
        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.streamly-extras-monitoring
