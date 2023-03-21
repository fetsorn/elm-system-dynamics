{
  description = "A system dynamics model of market competition";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    mkElmDerivation.url = "github:jeslie0/mkElmDerivation";
  };

  outputs = { self, nixpkgs, mkElmDerivation }:
    let
      eachSystem = systems: f:
        let
          op = attrs: system:
            let
              ret = f system;
              op = attrs: key:
                let
                  appendSystem = key: system: ret: { ${system} = ret.${key}; };
                in attrs // {
                  ${key} = (attrs.${key} or { })
                    // (appendSystem key system ret);
                };
            in builtins.foldl' op attrs (builtins.attrNames ret);
        in builtins.foldl' op { } systems;
      defaultSystems = [ "x86_64-linux" "aarch64-darwin" ];
    in eachSystem defaultSystems (system:
      let
        system = "aarch64-darwin";
        pkgs = import nixpkgs {
          overlays = [ mkElmDerivation.overlay.${system} ];
          inherit system;
        };
      in {
        packages.default = pkgs.mkElmDerivation {
          pname = "elm-system-dynamics";
          version = "0.1.0";
          src = ./.;
        };
      });
}
