{
  description = "R7RS-small scheme implementation based on s7 scheme";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        inputs.treefmt-nix.flakeModule
        inputs.devshell.flakeModule
      ];
      perSystem =
        {
          pkgs,
          lib,
          self',
          config,
          ...
        }:
        let
          goldfishFor =
            pkgs:
            {
              static ? false,
            }:
            let
              tbox = pkgs.tbox.overrideAttrs (
                final: prev: {
                  version = "1.7.6";
                  src = pkgs.fetchFromGitHub {
                    owner = "tboox";
                    repo = "tbox";
                    rev = "v${final.version}";
                    hash = "sha256-cwpZ7F8WzT/46HrckHe0Aug2mxirCkNA68aCxg/FcsE=";
                  };
                  meta.platforms = prev.meta.platforms ++ pkgs.lib.platforms.windows;
                }
              );
              isocline = pkgs.callPackage ./pkgs/isocline.nix { };
              s7 = (pkgs.callPackage ./pkgs/s7.nix { }).override {
                inherit static;
                withGMP = false;
                withArb = false;
                withNrepl = false;
              };
            in
            pkgs.callPackage ./pkgs/goldfish.nix {
              inherit static;
              inherit isocline s7 tbox;
            };
        in
        {
          packages = {
            goldfish = goldfishFor pkgs { };
            goldfish-musl = goldfishFor pkgs.pkgsMusl { static = true; };
            default = self'.packages.goldfish;

            goldfish-mingwW64-cross = goldfishFor pkgs.pkgsCross.mingwW64 { };
            goldfish-mingwW64-cross-static = goldfishFor pkgs.pkgsCross.mingwW64 { static = true; };
          };
          overlayAttrs = {
            inherit (config.packages)
              goldfish
              goldfish-musl
              goldfish-mingwW64-cross
              goldfish-mingwW64-cross-static
              ;
          };

          apps =
            let
              getGoldfish = lib.flip lib.getExe' "goldfish";
              mkGoldfishApp = pkg: {
                type = "app";
                program = getGoldfish pkg;
                meta.description = pkg.meta.description;
              };
            in
            {
              default = self'.apps.goldfish;
              goldfish = mkGoldfishApp self'.packages.goldfish;
              goldfish-musl = mkGoldfishApp self'.packages.goldfish-musl;
              goldfish-mingwW64-cross = mkGoldfishApp self'.packages.goldfish-mingwW64-cross;
              goldfish-mingwW64-cross-static = mkGoldfishApp self'.packages.goldfish-mingwW64-cross-static;
            };

          devshells.default = {
            name = "NixConsole";
            motd = ''
              {italic}🐟 {220}Goldfish{reset} 👾
              $(type -p menu &>/dev/null && menu)
            '';
            imports = [ "${inputs.devshell}/extra/language/c.nix" ];
            language.c.compiler = pkgs.gcc;
            language.c.includes = [ pkgs.curl.dev ];
            language.c.libraries = [ ];
            packages = with pkgs; [
              xmake
              clang-tools
              pkg-config
              gnumake
              cmake
              unzip
              gdb
              perf
              valgrind
              guile
              racket-minimal
            ];
          };

          treefmt.config = {
            projectRootFile = ".git/config";
            package = pkgs.treefmt;

            programs = {
              keep-sorted.enable = true;

              # nix
              nixfmt.enable = true;
              deadnix.enable = true;
              statix.enable = true;
            };

            settings.formatter = {
              keep-sorted = {
                includes = lib.mkForce [ "*.nix" ];
              };
            };
          };
        };

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
    };
}
