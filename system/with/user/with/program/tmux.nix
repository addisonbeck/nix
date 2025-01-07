{inputs, pkgs, ...}: {

  home.packages = [
    pkgs.fzf
    inputs.tmux-popr.packages.${pkgs.system}.default
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
      SOCKET_NAME="popup-scratch"

      function full_session_name() {
          printf "''${SESSION_PREFIX} ''${SEPARATOR_CHAR} ''${SESSION_NAME}"
      }

      FULL_SESSION="$(full_session_name)"

      # Check if we're already in the popup
      if [ -n "$TMUX" ] && [[ "$TMUX" == *"$SOCKET_NAME"* ]]; then
          tmux -L "$SOCKET_NAME" detach-client
          exit 0
      fi

      # Create or attach to the popup session
      if ! tmux -L "$SOCKET_NAME" has-session -t "$FULL_SESSION" 2>/dev/null; then
          tmux -L "$SOCKET_NAME" new-session -d -s "$FULL_SESSION"
          tmux -L "$SOCKET_NAME" set-option -t "$FULL_SESSION" status off
      fi

      # Show popup with the dedicated socket
      exec tmux popup -d '#{pane_current_path}' -xC -yC -w95% -h95% -E \
        "tmux -L $SOCKET_NAME attach -t \"$FULL_SESSION\""
    '')
    (pkgs.writeShellScriptBin "tmux-session-popup" ''
      # Capture the parent session name before creating the popup
      PARENT_SESSION=$(tmux display-message -p "#{session_name}")
      POPUP_SUFFIX="_popup"
      SOCKET_PREFIX="popup-session"

      function get_popup_session_name() {
        local base_session=$1
        printf "%s%s" "$base_session" "$POPUP_SUFFIX"
      }

      function get_socket_name() {
        local base_session=($1//[^a-zA-Z0-9]/)
        printf "%s-%s" "$SOCKET_PREFIX" "$base_session"
      }

      POPUP_SESSION=$(get_popup_session_name "$PARENT_SESSION")
      SOCKET_NAME=$(get_socket_name "$PARENT_SESSION")

      # Check if we're already in the popup
      if [ -n "$TMUX" ] && [[ "$TMUX" == *"$SOCKET_NAME"* ]]; then
          tmux -L "$SOCKET_NAME" detach-client
          exit 0
      fi

      # Create or attach to the popup session
      if ! tmux -L "$SOCKET_NAME" has-session -t "$POPUP_SESSION" 2>/dev/null; then
          tmux -L "$SOCKET_NAME" new-session -d -s "$POPUP_SESSION"
          tmux -L "$SOCKET_NAME" set-option -t "$POPUP_SESSION" status off
      fi

      # Show popup with the dedicated socket
      exec tmux popup -d '#{pane_current_path}' -xC -yC -w95% -h95% -E \
        "tmux -L $SOCKET_NAME attach -t \"$POPUP_SESSION\""
    '')
    (pkgs.writeShellScriptBin "lazygit-popup" ''
      SOCKET_NAME="popup-lazygit"

      # Check if we're already in the popup
      if [ -n "$TMUX" ] && [[ "$TMUX" == *"$SOCKET_NAME"* ]]; then
          tmux -L "$SOCKET_NAME" detach-client
          exit 0
      fi

      # Show popup with lazygit
      exec tmux popup -d '#{pane_current_path}' -xC -yC -w95% -h95% -E "lazygit"
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
      bind-key -n C-p run-shell "tmux-popr"
      bind-key -n C-l run-shell "tmux-popr lazygit"
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
