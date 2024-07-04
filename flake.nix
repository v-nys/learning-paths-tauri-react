{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        libraries = with pkgs;[
          webkitgtk
          gtk3
          cairo
          gdk-pixbuf
          glib
          dbus
          openssl_3
          librsvg
        ];

        packages = with pkgs; [
          envsubst # to enable tests which use absolute paths
          curl
          wget
          pkg-config
          dbus
          openssl_3
          glib
          gtk3
          libsoup
          webkitgtk
          librsvg
          graphviz
        ];
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = packages;

          # envsubst must be applied for any cluster definition containing an absolute path
          shellHook =
            ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath libraries}:$LD_LIBRARY_PATH
              export XDG_DATA_DIRS=${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS
              export FLAKE_DIR=$(pwd)
              envsubst ./rust-workspace/learning-paths-tauri-react/tests/technicalinfo/pre-contents.lc.yaml > ./rust-workspace/learning-paths-tauri-react/tests/technicalinfo/contents.lc.yaml
              npm install
            '';
        };
      });
}
