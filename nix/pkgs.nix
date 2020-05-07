{ pkgsf ? import ./nixpkgs.nix {}
}:
let 
  cleanpkgs = import pkgsf {};
  validity-overlay =
    import (
      cleanpkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix"
    );

  pkgs =
    import pkgsf {
      overlays = [
        validity-overlay
        ( import ./gitignore-src.nix )  
        ( import ./overlay.nix )
      ];
      config.allowUnfree = true;
    };
in pkgs
