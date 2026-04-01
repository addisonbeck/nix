#!/data/data/com.termux/files/usr/bin/bash
# Bootstrap Termux for Emacs-over-SSH on Onyx Boox e-ink tablet
# Usage: curl -fsSL <raw-url> | bash
set -euo pipefail

HOME_DIR="/data/data/com.termux/files/home"
BIN_DIR="$HOME_DIR/bin"

echo "==> Updating packages..."
pkg update -y && pkg upgrade -y

echo "==> Installing packages..."
pkg install -y \
  openssh \
  autossh \
  curl \
  git \
  termux-tools \
  termux-api

echo "==> Requesting storage permission..."
termux-setup-storage || true

echo "==> Creating directories..."
mkdir -p "$BIN_DIR" "$HOME_DIR/.ssh" "$HOME_DIR/.termux" "$HOME_DIR/.shortcuts"
chmod 700 "$HOME_DIR/.ssh"

echo "==> Writing SSH config..."
cat > "$HOME_DIR/.ssh/config" << 'SSHEOF'
Host bw
  User me
  ServerAliveInterval 30
  ServerAliveCountMax 5
  TCPKeepAlive yes
  ControlMaster auto
  ControlPath ~/.ssh/cm-%r@%h:%p
  ControlPersist 30m
  Compression no
  ForwardAgent yes
  RequestTTY yes
SSHEOF
chmod 600 "$HOME_DIR/.ssh/config"

echo "==> Writing Termux properties (extra-keys, display)..."
cat > "$HOME_DIR/.termux/termux.properties" << 'PROPEOF'
# Extra keys: Emacs-optimized with popup macros (swipe up = chord)
extra-keys = [[ \
  {key: ESC, popup: {macro: "CTRL g", display: "C-g"}}, \
  {key: CTRL, popup: {macro: "CTRL a", display: "C-a"}}, \
  {key: ALT, popup: {macro: "CTRL c", display: "C-c"}}, \
  {key: TAB, popup: {macro: "ALT x", display: "M-x"}}, \
  '~', \
  {key: UP, popup: PGUP}, \
  {key: '-', popup: '+'}, \
  {key: PGUP, popup: HOME} \
], [ \
  {key: '/', popup: {macro: "CTRL s", display: "C-s"}}, \
  {key: '|', popup: {macro: "CTRL x 3", display: "split-v"}}, \
  {key: BACKSLASH, popup: {macro: "CTRL x 1", display: "del-win"}}, \
  {key: ':', popup: {macro: "CTRL x CTRL s", display: "save"}}, \
  {key: LEFT, popup: HOME}, \
  {key: DOWN, popup: PGDN}, \
  {key: RIGHT, popup: END}, \
  {key: PGDN, popup: END} \
]]

# No cursor blink - causes e-ink ghosting
terminal-cursor-blink-rate = 0
bell-character = vibrate
PROPEOF

echo "==> Writing e-ink color scheme (gruvbox light)..."
cat > "$HOME_DIR/.termux/colors.properties" << 'COLOREOF'
# Gruvbox Light - high contrast for e-ink grayscale rendering
background: #fbf1c7
foreground: #3c3836
cursor: #3c3836

color0:  #fbf1c7
color8:  #928374
color1:  #9d0006
color9:  #cc241d
color2:  #79740e
color10: #98971a
color3:  #b57614
color11: #d79921
color4:  #076678
color12: #458588
color5:  #8f3f71
color13: #b16286
color6:  #427b58
color14: #689d6a
color7:  #3c3836
color15: #1d2021
COLOREOF

echo "==> Downloading JetBrains Mono Nerd Font..."
curl -fLo "$HOME_DIR/.termux/font.ttf" \
  "https://github.com/ryanoasis/nerd-fonts/raw/HEAD/patched-fonts/JetBrainsMono/Ligatures/Regular/JetBrainsMonoNerdFont-Regular.ttf" \
  2>/dev/null || echo "    Font download failed; using default font"

echo "==> Writing connection scripts..."

cat > "$BIN_DIR/bw" << 'BWEOF'
#!/data/data/com.termux/files/usr/bin/bash
termux-wake-lock 2>/dev/null || true
exec ssh -t bw 'em'
BWEOF
chmod +x "$BIN_DIR/bw"

# Auto-reconnect variant: autossh restarts the connection on drop
cat > "$BIN_DIR/bw-persist" << 'BWPEOF'
#!/data/data/com.termux/files/usr/bin/bash
termux-wake-lock 2>/dev/null || true
export AUTOSSH_POLL=30
export AUTOSSH_GATETIME=0
exec autossh -M 0 -t bw 'em'
BWPEOF
chmod +x "$BIN_DIR/bw-persist"

# Termux:Widget home screen shortcuts
ln -sf "$BIN_DIR/bw" "$HOME_DIR/.shortcuts/bw"
ln -sf "$BIN_DIR/bw-persist" "$HOME_DIR/.shortcuts/bw-persist"

echo "==> Ensuring ~/bin is in PATH..."
PROFILE="$HOME_DIR/.bashrc"
if ! grep -q 'HOME/bin' "$PROFILE" 2>/dev/null; then
  # shellcheck disable=SC2016
  echo 'export PATH="$HOME/bin:$PATH"' >> "$PROFILE"
fi

echo "==> Generating SSH key (if none exists)..."
if [ ! -f "$HOME_DIR/.ssh/id_ed25519" ]; then
  ssh-keygen -t ed25519 -f "$HOME_DIR/.ssh/id_ed25519" -N '' -C "termux@boox"
  echo ""
  echo "    Copy this public key to bw:"
  cat "$HOME_DIR/.ssh/id_ed25519.pub"
  echo ""
fi

echo "==> Reloading Termux settings..."
termux-reload-settings 2>/dev/null || true

echo ""
echo "==> Done!"
echo ""
echo "    Next steps:"
echo "    1. Add SSH key to bw (see above) if newly generated"
echo "    2. In Boox Settings > Apps > Termux:"
echo "         - Refresh mode: Speed (or Smooth on firmware V4.1+)"
echo "         - Bold Font: on"
echo "         - Full refresh every 5 pages"
echo "         - Battery: Unrestricted (prevents Doze killing SSH)"
echo "    3. Run 'bw' (standard) or 'bw-persist' (auto-reconnect)"
echo "    4. Install Termux:Widget from F-Droid for home screen shortcuts"
echo ""
