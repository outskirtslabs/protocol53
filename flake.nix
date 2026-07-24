{
  description = "dev env";
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devenv.url = "https://flakehub.com/f/ramblurr/nix-devenv/*";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    clj-helpers.url = "github:outskirtslabs/clojure-nix-locker-helpers";
    clj-helpers.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    inputs@{
      self,
      devenv,
      devshell,
      clj-helpers,
      ...
    }:
    let
      package =
        pkgs:
        clj-helpers.lib.mkCljLib {
          inherit pkgs;
          name = "TODO";
          version = "0.0.TODO";
          src = ./.;
          prepAliases = [
            "dev"
            "kaocha"
          ];
          prefetchAliases = [ "dev:kaocha" ];
          checkCommand = "clojure -Srepro -M:dev:kaocha";
          gitRev = clj-helpers.lib.gitRev self;
        };
    in
    devenv.lib.mkFlake ./. {
      inherit inputs;
      withOverlays = [
        devshell.overlays.default
        devenv.overlays.default
      ];
      packages = {
        default = package;
        # regenerates ./deps-lock.json: `nix run .#locker`
        locker = pkgs: (package pkgs).locker;
      };
      devShell =
        pkgs:
        pkgs.devshell.mkShell {
          imports = [
            devenv.capsules.base
            devenv.capsules.clojure
          ];
          # https://numtide.github.io/devshell
          commands = [
            # { package = pkgs.bazqux; }
          ];
          packages = [
            (
              if self ? packages then
                self.packages.${pkgs.system}.locker
              else
                clj-helpers.packages.${pkgs.system}.deps-lock
            )
            # pkgs.foobar
          ];
        };
    };
}
