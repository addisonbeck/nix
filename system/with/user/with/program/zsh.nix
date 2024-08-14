{ ... }: {
  programs.zsh.enable = true;
  programs.zsh.profileExtra = ''
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  '';
}
