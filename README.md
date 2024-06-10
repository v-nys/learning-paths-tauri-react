# Learning paths
This project analyzes and visualizes YAML-encoded learning paths. Learning paths tie together specific bits of material using "any" and "all" edges and are visualized in a Tauri application.

## Getting started
From the project root, run `npm run tauri dev`.
Check all checks that should be performed (unchecking is mainly useful if some checks are too expensive) and enter the file paths to the clusters involved in the learning path.
Requires graphviz to be installed on your system.

If you are running Nix with flakes, just run `nix develop` and `npm run tauri dev` should work fine from that shell.

## Development
Git hooks are versioned, in the `hooks` folder. Symlink them to enable.

## Troubleshooting
May require environment variable `WEBKIT_DISABLE_DMABUF_RENDERER` set to `1`.
