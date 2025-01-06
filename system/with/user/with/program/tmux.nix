{pkgs, ...}: {
  home.packages = [
    pkgs.fzf
    (pkgs.writeShellScriptBin "fzf-tmux-popup" ''
      active_window=$(tmux display-message -p "#{session_name}:#{window_index} │ #{window_name}")

      window=$(tmux list-windows -a -F "#{session_last_attached}_#{window_last_flag}_#{session_name}:#{window_index} │ #{window_name}" |
              grep -v "$active_window" |
              sort -r |
              cut -d_ -f3- |
              fzf --reverse --no-info --header="Current: $active_window")
      if [ $? -eq 0 ]; then
        session=$(echo "$window" | cut -d: -f1)
        window_id=$(echo "$window" | cut -d: -f2 | cut -d' ' -f1)
        tmux switch-client -t "$session" \; select-window -t "$session:$window_id"
      fi
    '')
    (pkgs.writeShellScriptBin "tmux-toggle-popup" ''
      SESSION_PREFIX="🚮"
      SESSION_NAME="scratchpad"
      SEPARATOR_CHAR="▶"

      function full_session_name() {
          printf "''${SESSION_PREFIX} ''${SEPARATOR_CHAR} ''${SESSION_NAME}"
      }

      FULL_SESSION="$(full_session_name)"

      if [ "$(tmux display-message -p -F "#{session_name}")" = "$FULL_SESSION" ]; then
          tmux detach-client
      else
          # Create session in background if it doesn't exist, with hidden flag
          tmux has-session -t "$FULL_SESSION" 2>/dev/null || \
            tmux new-session -d -s "$FULL_SESSION" \; set-option remain-on-exit on \; set-option hidden 1

          # Ensure hidden flag is set even for existing sessions
          tmux set-option -t "$FULL_SESSION" remain-on-exit on \; set-option -t "$FULL_SESSION" hidden 1

          # Now show the popup with the hidden session
          tmux popup -d '#{pane_current_path}' -xC -yC -w95% -h95% -E \
            "tmux attach -t \"$FULL_SESSION\""
      fi
    '')

    (pkgs.writeShellScriptBin "tmux-session-popup" ''
      CURRENT_SESSION=$(tmux display-message -p "#{session_name}")
      POPUP_SUFFIX="_popup"

      function get_popup_session_name() {
        local base_session=$1
        printf "%s%s" "$base_session" "$POPUP_SUFFIX"
      }

      POPUP_SESSION=$(get_popup_session_name "$CURRENT_SESSION")

      if [[ "$CURRENT_SESSION" == *"$POPUP_SUFFIX" ]]; then
        tmux detach-client
      else
        # Create session in background if it doesn't exist, with hidden flag
        tmux has-session -t "$POPUP_SESSION" 2>/dev/null || \
          tmux new-session -d -s "$POPUP_SESSION" \; set-option remain-on-exit on \; set-option hidden 1

        # Ensure hidden flag is set even for existing sessions
        tmux set-option -t "$POPUP_SESSION" remain-on-exit on \; set-option -t "$POPUP_SESSION" hidden 1

        # Now show the popup with the hidden session
        tmux popup -d '#{pane_current_path}' -xC -yC -w95% -h95% -E \
          "tmux attach -t \"$POPUP_SESSION\""
      fi
    '')
  ];

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    disableConfirmationPrompt = true;
    escapeTime = 0;

    extraConfig = ''
      # General Settings
      set -g set-clipboard on
      set -g mouse on
      set -g detach-on-destroy off
      set -g base-index 0
      set-option -sg escape-time 0
      set-option -g focus-events on
      set -gq allow-passthrough on
      set -g visual-activity off
      setw -g pane-base-index 0

      # Terminal Features
      set -as terminal-overrides ',*:Smulx=\E[4::%p1%dm'
      set -as terminal-overrides ',*:Setulc=\E[58::2::%p1%{65536}%/%d::%p1%{256}%/%{255}%&%d::%p1%{255}%&%d%;m'
      set-option -ga terminal-overrides ',xterm-kitty:cnorm=\E[?12h\E[?25h'

      # Vi Mode Settings
      setw -g mode-keys vi
      bind -T copy-mode-vi v send -X begin-selection
      bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "pbcopy"
      bind P paste-buffer
      bind -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "pbcopy"

      # Status Bar Configuration
      set -g status off
      set -g status-position top
      set -g status-interval 15
      set -g status-justify absolute-centre
      set -g status-keys vi
      set -g status-left-length 50
      set -g status-right-length 40
      set -g status-left "#[bold,fg=blue,bg=black]#h: #S"
      set -g status-right ""
      set -g window-status-format ""
      set -g window-status-current-format ""

      # Key Bindings
      unbind C-b
      set -g prefix C-Space
      bind C-Space send-prefix
      bind-key -n C-j display-popup -E "fzf-tmux-popup"
      bind-key p run-shell "tmux-toggle-popup"
      bind-key -n C-p run-shell "tmux-session-popup"
      bind Space switch-client -l
      bind r source-file ~/.config/tmux/tmux.conf

      # Keybinding Toggle (F12)
      bind -T root F12 \
        set prefix None \;\
        set key-table off \;\
        set status off \;\
        if -F "#{pane_in_mode}" "send-keys -X cancel" \;\
        refresh-client -S

      bind -T off F12 \
        set -u prefix \;\
        set -u key-table \;\
        set status on \;\
        refresh-client -S

      # Remote Config
      if-shell "test -n \"$SSH_CLIENT\"" "source-file ~/.tmux.remote.conf"
    '';
  };
}
