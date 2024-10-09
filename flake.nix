{
  description = "Development Environment for Coruja";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    
  }; 
  outputs = { self, nixpkgs, ... }: {
    devShells = {
      x86_64-linux.default = let
        pkgs = import nixpkgs {
          system = "x86_64-linux";
          config.allowUnfree = true;
        };
      in pkgs.mkShell {

      packages = with pkgs; [
        R
        gnumake
        rstudio
        rPackages.Cairo
        fontconfig
      ];
      shellHook = ''
        export R_LIBS_USER="$(pwd)/.R"
      '';
      };
      aarch64-darwin.default = let
        pkgs = import nixpkgs {
          system = "aarch64-darwin";
          config.allowUnfree = true;
        };
      in pkgs.mkShell {
      packages = with pkgs; [
        R
        gnumake
        rPackages.Cairo
        fontconfig
      ];
      shellHook = ''
        export R_LIBS_USER="$(pwd)/.R"
      '';
      };
    };
  };
}
