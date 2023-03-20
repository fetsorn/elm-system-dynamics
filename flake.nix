{
  description = "A system dynamics model of market competition";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    mkElmDerivation.url = "github:jeslie0/mkElmDerivation";
  };

  outputs = { self, nixpkgs, mkElmDerivation }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        overlays = [ mkElmDerivation.overlay.${system} ];
        inherit system;
      };
    in {
      packages.${system}.default = pkgs.mkElmDerivation {
        pname = "elm-system-dynamics";
        version = "0.1.0";
        src = ./.;
      };
    };
}
