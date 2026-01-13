# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Nix-based systems configuration repository managing both NixOS and macOS (nix-darwin) systems. The configuration is highly modular and uses a compositional architecture with a distinctive `/with/` directory pattern for organizing system components.

**Managed systems:**
- `air`, `bw` (macOS via nix-darwin)
- `homelab`, `vm`, `linux-testing-vm` (NixOS)

**Key commands:**
```bash
# Rebuild system
nix develop .#building --command rebuild <hostname>

# Update flakes
nix develop .#building --command update

# Format code
nix develop .#formatting --command apply formatting

# Using just
just rebuild-pre  # Update nix-secrets and stage changes
just update       # Update all flakes
just check        # Run flake check
```

**Development shells:**
- `building` - System build and rebuild tools
- `formatting` - Code formatting with treefmt
- `managing-secrets` - SOPS and git-crypt tooling
- `editing` - nixd language server

## Architecture & Patterns

### Directory Structure

The repository uses a `/with/` pattern to compose system configurations:

```
system/
├── <hostname>.nix           # Top-level system definitions
├── with/
│   ├── nix.nix              # Nix configuration
│   ├── home-manager.nix     # Home Manager setup
│   ├── desktop-environment/ # Desktop environments (darwin, gnome, hyprland, sway)
│   ├── hardware/            # Hardware-specific configs
│   ├── trait/               # System traits (allow-unfree, ssh-enabled, has-swapfile)
│   └── user/
│       ├── me.nix           # Main user configuration
│       └── with/
│           ├── program/     # Individual program configurations
│           ├── service/     # Services (autoclone)
│           └── development-environment/ # Dev setups
└── modules/
    ├── emacs/               # Literate Emacs configuration in org-mode
    ├── secrets/             # SOPS secret definitions
    └── automated-emailing/  # Custom modules
```

### Core Patterns

1. **Compositional Modules**: System configurations built by importing modules from `with/` directories
2. **Literate Configuration**: Emacs config written in org-mode files, tangled to elisp during build using `tangle-script.el`
3. **Per-User Programs**: Each program has its own module in `system/with/user/with/program/`
4. **Special Args**: Flake passes `inputs`, `outputs`, `nixpkgs`, `rootPath`, `conf`, and `hostname` through module system
5. **Theme System**: Centralized theming in `config/` directory with active theme selection

### Emacs Configuration

Located in `system/modules/emacs/`:
- Written as literate org-mode files (core.org, gptel.org, projects.org, etc.)
- `default.nix` runs Emacs in batch mode to tangle all org files into `init.el`
- Packages managed via `emacsWithPackagesFromUsePackage`
- Custom `ec` wrapper script manages daemon and frame reuse
- Check `~/.emacs.d/generated-init.el` to debug tangled output

## Stack Best Practices

### Adding a New Program

1. Create module in `system/with/user/with/program/<program-name>.nix`
2. Import it in `system/with/user/me.nix`
3. Use home-manager options to configure the program

### Modifying Emacs Configuration

1. Edit the relevant org file in `system/modules/emacs/`
2. Ensure code blocks have `:tangle <filename>.el` header args
3. Rebuild - tangling happens automatically during build

### Cross-Platform Development

- Use `lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin` for macOS-specific config
- Use `lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux` for Linux-specific config
- System definitions use either `darwinConfigurations` or `nixosConfigurations`

### Testing Changes

Test system-wide changes on the VM first:
```bash
nix build .#nixosConfigurations.linux-testing-vm.config.system.build.vm
./result/bin/run-nixos-vm
```

### Formatting

Uses treefmt for code formatting:
- **Nix**: alejandra
- **Org files**: Custom Emacs-based formatter (`format-org.el`)
- **Lua**: stylua
- **Shell**: shellcheck

Check formatting before committing:
```bash
nix develop .#formatting --command check formatting
```

## Anti-Patterns

- **Don't** bypass the `/with/` compositional pattern - always compose configurations through module imports
- **Don't** manage Emacs packages outside of `emacsWithPackagesFromUsePackage` - keep all package definitions in org files
- **Don't** hardcode platform-specific code without guards - always use `lib.optionalAttrs` with platform checks
- **Don't** commit unformatted code - CI runs formatting checks and will reject improperly formatted code
- **Don't** skip VM testing for system-wide changes - use `linux-testing-vm` to validate changes before deploying

## Data Models

### Flake Special Arguments

The flake passes these arguments through the module system:
- `inputs` - Flake inputs for accessing external dependencies
- `outputs` - Flake outputs for self-referencing
- `nixpkgs` - The nixpkgs input
- `rootPath` - Repository root path for relative imports
- `conf` - Imported config from `./config/` (theme system, etc.)
- `hostname` - Current system hostname

### System Configuration Structure

Each system configuration composes:
- Base Nix configuration (`with/nix.nix`)
- Home Manager setup (`with/home-manager.nix`)
- Desktop environment (optional, from `with/desktop-environment/`)
- Hardware configuration (from `with/hardware/`)
- System traits (from `with/trait/`)
- User configuration (`with/user/me.nix`)

## Configuration, Security, and Authentication

### Secrets Management

Uses dual-layer secret management:
- **SOPS** with age encryption for declarative secrets
- **git-crypt** for additional encrypted files
- Secrets stored in separate `nix-secrets` flake (private repository)

Generate age keys from SSH keys:
```bash
ssh-to-age -private-key -i ~/.ssh/id_ed25519 > ~/.config/sops/age/keys.txt
```

Access secrets shell:
```bash
nix develop .#managing-secrets
```

### Secret Definitions

Secrets defined in `system/modules/secrets/` and consumed via SOPS-nix module options throughout system configurations.
