{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgsArgs = {
          inherit system overlays;
        };
        pkgs = import nixpkgs pkgsArgs;
        rust-toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        buildRustCrateForPkgs = pkgs: pkgs.buildRustCrate.override {
          rustc = rust-toolchain pkgs;
          defaultCrateOverrides = pkgs.defaultCrateOverrides // {
          };
        };
        cargoNix = import ./Cargo.nix {
          inherit pkgs buildRustCrateForPkgs;
        };
      in
      rec {
        packages = {
        };
        devShell =
          with pkgs; mkShell {
            nativeBuildInputs = [
              rust-toolchain
              gnumake
              crate2nix
              pkg-config
            ];
            buildInputs = lib.optionals stdenv.isDarwin [
              libiconv
              darwin.apple_sdk.frameworks.SystemConfiguration
              darwin.apple_sdk.frameworks.CoreFoundation
              darwin.apple_sdk.frameworks.Security
            ] ++ lib.optionals stdenv.isLinux [
              glibc
            ];
          };
        devShells.linux-shell =
          let
            linuxPkgs =
              if system == "x86_64-linux"
              then pkgs.pkgsStatic
              else (import nixpkgs {
                system = if system == "aarch64-darwin" then "x86_64-darwin" else system;
              }).pkgsCross.musl64.pkgsStatic;
          in
          with linuxPkgs; mkShell {
            nativeBuildInputs = [
              pkg-config
            ];
            buildInputs = [
            ];
          };
      }
    );
}
