let nixpkgs = import ./nixpkgs.nix;
    pkgs = import nixpkgs { };

    drv = import ./default.nix { withHoogle = true; };
in drv.env.overrideAttrs (attrs: {
  buildInputs =
    with pkgs.haskellPackages;
    [ cabal-install
      cabal2nix
      ghcid
      hindent
      hlint
      stylish-haskell
    ] ++
    attrs.buildInputs;
})
