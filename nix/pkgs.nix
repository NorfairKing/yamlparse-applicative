{ pkgsf ? import ./nixpkgs.nix {}
}:
let
  validity-overlay =
    import (
      builtins.fetchGit (import ./validity-version.nix) + "/nix/overlay.nix"
    );
  safe-coloured-text-overlay =
    import (
      builtins.fetchGit (import ./safe-coloured-text-version.nix) + "/nix/overlay.nix"
    );
  sydtest-overlay =
    import (
      builtins.fetchGit (import ./sydtest-version.nix) + "/nix/overlay.nix"
    );

  pkgs =
    import pkgsf {
      overlays = [
        validity-overlay
        safe-coloured-text-overlay
        sydtest-overlay
        (import ./gitignore-src.nix)
        (import ./overlay.nix)
      ];
      config.allowUnfree = true;
    };
in
pkgs
