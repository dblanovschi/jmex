{ pkgs ? import <nixpkgs> { overlays = (import ./nix/overlays.nix); } }:

pkgs.mkShell {
  nativeBuildInputs = [
    (pkgs.rust-bin.stable.latest.default.override {
      extensions = [ "rust-src" ];
      targets = [ "x86_64-unknown-linux-musl" ];
    })
  ];

  buildInputs = [
    pkgs.nixpkgs-fmt
    pkgs.niv

    pkgs.clang_12
    pkgs.lld_12
    pkgs.glibc
  ];

  shellHook = ''
    export CC=clang
  '';
}
