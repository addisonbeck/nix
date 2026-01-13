# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Nix-based systems configuration repository managing both NixOS and macOS (nix-darwin) systems. The configuration is highly modular and uses a compositional architecture with a distinctive `/with/` directory pattern for organizing system components.

## Building and Development

### Common Commands

```bash
# Rebuild the current system (requires hostname)
nix develop .#building --command rebuild <hostname>

# Available hostnames:
# - air (macOS)
# - bw (macOS)
# - homelab (NixOS)
# - vm (NixOS)
# - linux-testing-vm (NixOS)

# Update flakes
nix develop .#building --command update

# Format code
nix develop .#formatting --command apply formatting

# Check formatting (runs in CI)
nix develop .#formatting --command check formatting

# Manage secrets
nix develop .#managing-secrets

# Build Raspberry Pi image
nix develop .#building --command build-pi-image
nix develop .#building --command decompress-pi-image
```

Or using just:
```bash
just rebuild-pre  # Update nix-secrets and stage changes
just update       # Update all flakes
just check        # Run flake check
```

### Development Shells

The flake defines multiple development shells for different tasks:
- `default` - Includes all other shells
- `building` - System build and rebuild tools
- `formatting` - Code formatting with treefmt
- `managing-secrets` - SOPS and git-crypt tooling
- `editing` - nixd language server

## Architecture

### Directory Structure Philosophy

The repository uses a `/with/` pattern to compose system configurations:

```
system/
├── <hostname>.nix           # Top-level system definitions (air.nix, bw.nix, homelab/, etc.)
├── with/
│   ├── nix.nix              # Nix configuration
│   ├── home-manager.nix     # Home Manager setup
│   ├── desktop-environment/ # Desktop environments (darwin, gnome, hyprland, sway)
│   ├── hardware/            # Hardware-specific configs (vm.nix)
│   ├── trait/               # System traits (allow-unfree, ssh-enabled, has-swapfile)
│   └── user/
│       ├── me.nix           # Main user configuration
│       └── with/
│           ├── program/     # Individual program configurations
│           ├── service/     # Services (autoclone)
│           └── development-environment/ # Dev setups (dotfiles, nix, notes, bitwarden)
└── modules/
    ├── emacs/               # Literate Emacs configuration in org-mode
    ├── secrets/             # SOPS secret definitions
    └── automated-emailing/  # Custom modules
```

### Key Architectural Patterns

1. **Compositional Modules**: System configurations are built by importing modules from `with/` directories
2. **Literate Configuration**: Emacs configuration is written in org-mode files and tangled to elisp
3. **Per-User Programs**: Each program (fish, git, emacs, etc.) has its own module in `system/with/user/with/program/`
4. **Secrets Management**: Uses SOPS-nix with age keys for secret management
5. **Theme System**: Centralized theming in `config/` directory with active theme selection

### Emacs Configuration

The Emacs setup (`system/modules/emacs/`) is unique:
- Written as literate org-mode files (core.org, gptel.org, projects.org, etc.)
- Tangled during build using `tangle-script.el`
- The `default.nix` runs Emacs in batch mode to tangle all org files into `init.el`
- Packages are managed via `emacsWithPackagesFromUsePackage`
- Custom `ec` wrapper script manages daemon and frame reuse

### Special Args System

The flake passes special arguments through the module system:
- `inputs` - Flake inputs
- `outputs` - Flake outputs
- `nixpkgs` - The nixpkgs input
- `rootPath` - Repository root path
- `conf` - Imported config from `./config/`
- `hostname` - Current system hostname

## Formatting

This repository uses treefmt for code formatting:
- **Nix**: alejandra
- **Org files**: Custom Emacs-based formatter (`format-org.el`)
- **Lua**: stylua
- **Shell**: shellcheck

CI runs formatting checks on pushes and PRs.

## Secrets

Secrets are managed using:
- **SOPS** with age encryption
- **git-crypt** for additional encrypted files
- Secrets stored in separate `nix-secrets` flake (private repository)
- Age keys generated from SSH keys: `ssh-to-age -private-key -i ~/.ssh/id_ed25519 > ~/.config/sops/age/keys.txt`

## Working with This Repository

### Adding a New Program

1. Create a module in `system/with/user/with/program/<program-name>.nix`
2. Import it in `system/with/user/me.nix`
3. Use home-manager options to configure the program

### Modifying Emacs Configuration

1. Edit the relevant org file in `system/modules/emacs/`
2. Ensure code blocks have `:tangle <filename>.el` header args
3. Rebuild - the tangling happens automatically during build
4. Check `~/.emacs.d/generated-init.el` to debug tangled output

### Cross-Platform Considerations

- Use `lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin` for macOS-specific config
- Use `lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux` for Linux-specific config
- System definitions use either nix-darwin (`darwinConfigurations`) or NixOS (`nixosConfigurations`)

### Testing Changes

For system-wide changes, test on the VM first:
```bash
nix build .#nixosConfigurations.linux-testing-vm.config.system.build.vm
./result/bin/run-nixos-vm
```
