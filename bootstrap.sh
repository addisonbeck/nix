#!/usr/bin/env bash

install_self () {
  cd $HOME 

  # install nix
  curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --no-confirm
  sudo rm /etc/nix/nix.conf

  if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
  fi

  # install homebrew
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

  nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake github:addisonbeck/nix#bw

  rm bootstrap.sh
}

install_self
