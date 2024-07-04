# Learning paths
This project analyzes and visualizes YAML-encoded learning paths. Learning paths tie together specific bits of material using "any" and "all" edges and are visualized in a Tauri application.

## Getting started
From the project root, run `npm run tauri dev`.
Check all checks that should be performed (unchecking is mainly useful if some checks are too expensive) and enter the file paths to the clusters involved in the learning path.
Requires graphviz to be installed on your system.

If you are running Nix with flakes, just run `nix develop` from the project root and `npm run tauri dev` should then work fine from that shell. Dependencies other than Rust itself are handled by the flake.

## Development

### Workspace and further plugins
Plugins have the core as a dependency.
Example plugins (also useful for tests) are included in the Rust workspace, but additional plugins may be written as standalone projects.
They just need to have the core as a dependency.

### Running tests
Clusters refer to plugins simply by the (absolute) path to a library file.
This will be different on different machines.
Therefore, test clusters cannot refer to plugin files directly in their YAML files.
Rather than using relative paths etc. for these clusters (which would leave absolute paths untested), the clusters' YAML file templates should be run through `envsubst`.
This happens automatically when the Nix development environment is activated.

### CI
Git hooks are versioned, in the `hooks` folder.
Symlink them to enable.
The easiest way is (from the root folder): `ln -s -f ../hooks .git/hooks` (do it this way because symlinks are resolved relatively).

## Troubleshooting
May require environment variable `WEBKIT_DISABLE_DMABUF_RENDERER` set to `1`.
