#!/usr/bin/env bash

install_self () {
  cd "$HOME" || exit

  curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --no-confirm
  # shellcheck source=/dev/null
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
  sudo rm /etc/nix/nix.conf

  # install homebrew
  NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

  nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake github:addisonbeck/nix#air

  rm bootstrap.sh
}

install_self
