{pkgs, ...}: {
  programs.tmux.enable = true;
  programs.tmux.terminal = "screen-256color";
  programs.tmux.disableConfirmationPrompt = true;
  #programs.tmux.shell = "${pkgs.fish}/bin/fish";
  programs.tmux.escapeTime = 0;
  programs.tmux.extraConfig = ''
    set -g set-clipboard on
    set -g mouse on
    set -g detach-on-destroy off
    set -g base-index 0
    set-option -sg escape-time 0
    set-option -g focus-events on

    set -gq allow-passthrough on
    set -g visual-activity off

    setw -g pane-base-index 0

    # Vi-like copy mode
    setw -g mode-keys vi

    bind -T copy-mode-vi v send -X begin-selection
    bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "pbcopy"
    bind P paste-buffer
    bind -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "pbcopy"

    # Undercurl support?
    set -as terminal-overrides ',*:Smulx=\E[4::%p1%dm'
    set -as terminal-overrides ',*:Setulc=\E[58::2::%p1%{65536}%/%d::%p1%{256}%/%{255}%&%d::%p1%{255}%&%d%;m'

    # Status Bar

    # set -g status on
    set -g status off
    set -g status-position top
    set -g status-style ""
    set -g status-right ""
    set -g status-right-length 40
    set -g status-right-style default
    set -g status-interval 15
    set -g status-justify absolute-centre
    set -g status-keys vi
    set -g mode-keys vi
    set -g status-left-length 50
    set -g status-left-style default
    # set -g window-status-format " #I: #W "
    set -g window-status-format ""
    set -g window-status-style dim,fg=white,bg=black
    # set -g window-status-current-format " #I: #W "
    set -g window-status-current-format ""
    set -g window-status-current-style bold,fg=black,bg=green
    set -g status-right-style bold,fg=blue,bg=black
    set -g status-left "#[bold,fg=blue,bg=black]#h: #S"

    # Keybindings

    unbind C-b
    set -g prefix C-Space
    bind C-Space send-prefix

    bind -T copy-mode-vi v send -X begin-selection
    bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "pbcopy"
    bind P paste-buffer
    bind -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "pbcopy"

    bind p run-shell "${
      (pkgs.writeShellScript "toggle-tmux-popup" ''
        SESSION_PREFIX="🚮"
               SESSION_NAME="scratchpad"
        SEPARATOR_CHAR="▶"

        function full_session_name () {
                 printf "''${SESSION_PREFIX} ''${SEPARATOR_CHAR} ''${SESSION_NAME}"
               }

               if [ "$(tmux display-message -p -F "#{session_name}")" = "$(full_session_name)" ];then
                tmux detach-client
               else
                 tmux popup -d '#{pane_current_path}' -xC -yC -w95% -h95% -E "tmux attach -t \"$(full_session_name)\" || tmux new -s \"$(full_session_name)\""
               fi
      '')
    }"

    bind r source-file ~/.tmux.conf

    # Disable all keymaps

    bind -T root F12  \
      set prefix None \;\
      set key-table off \;\
      set status off \;\
      # set status-left "#[fg=blue,bg=black,dim]#h: #S" \;\
      # set window-status-style "hidden" \;\
      # set window-status-current-style "" \;\
      # set window-status-format "" \;\
      # set window-status-current-format " 😴 " \;\
      if -F "#{pane_in_mode}" "send-keys -X cancel" \;\
      refresh-client -S \;\

    bind -T off F12 \
      set -u prefix \;\
      set -u key-table \;\
      set status on \;\
      # set -u status-left \;\
      # set -u window-status-style \;\
      # set -u window-status-current-style \;\
      # set -u window-status-format \;\
      # set -u window-status-current-format \;\
      refresh-client -S

    # Load remote specific config if applicable

    if-shell "test -n \"$SSH_CLIENT\"" \ "source-file ~/.tmux.remote.conf"
  '';
}
