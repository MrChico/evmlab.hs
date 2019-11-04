let
  version = "release-19.03";
  # Import a specific Nixpkgs revision
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-${version}";
    # pin the current release-19.03 commit
    url = "https://github.com/nixos/nixpkgs/archive/50d5d73e22bb2830f490e26a528579facfc7f302.tar.gz";
    sha256 = "0c1inf0pc2jizkrfl3629s154r55ya5asmwnwn6g64ppz2wwzizi";
  };
  pkgs = import nixpkgs {};

  haskellPackages = pkgs.haskellPackages.extend (
    self: super: {
      ghc =
        super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages =
        self.ghc.withPackages;
    }
  );

  drv = pkgs.haskell.lib.addBuildTool (
    haskellPackages.callPackage (import ./default.nix) {}
  ) [pkgs.cabal-install pkgs.cabal2nix];

in
  drv
