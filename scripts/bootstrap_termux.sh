#!/data/data/com.termux/files/usr/bin/bash
# Bootstrap Termux configuration
# Usage: curl -fsSL <raw-url> | bash
set -euo pipefail

TERMUX_PREFIX="/data/data/com.termux/files/usr"
HOME_DIR="/data/data/com.termux/files/home"
BIN_DIR="$HOME_DIR/bin"

echo "==> Updating packages..."
pkg update -y && pkg upgrade -y

echo "==> Installing packages..."
pkg install -y \
  openssh \
  curl \
  git \
  vim \
  python \
  termux-tools

echo "==> Requesting storage permission..."
termux-setup-storage || true

echo "==> Creating directories..."
mkdir -p "$BIN_DIR"
mkdir -p "$HOME_DIR/.ssh"
chmod 700 "$HOME_DIR/.ssh"

echo "==> Writing SSH shortcut (bw)..."
cat > "$BIN_DIR/bw" << 'SSH_SCRIPT'
#!/data/data/com.termux/files/usr/bin/bash
# Quick SSH into bw and open emacsclient in terminal
exec ssh -t me@bw 'emacsclient -t || { echo "emacsclient failed, launching emacs..."; emacs -nw; }'
SSH_SCRIPT
chmod +x "$BIN_DIR/bw"

echo "==> Ensuring ~/bin is in PATH..."
PROFILE="$HOME_DIR/.bashrc"
if ! grep -q 'export PATH="$HOME/bin:$PATH"' "$PROFILE" 2>/dev/null; then
  echo '' >> "$PROFILE"
  echo '# Local bin' >> "$PROFILE"
  echo 'export PATH="$HOME/bin:$PATH"' >> "$PROFILE"
fi

echo ""
echo "==> Done! Bootstrap complete."
echo ""
echo "    Run 'bw' to SSH into bw and open emacsclient."
echo "    Reload your shell or run: source ~/.bashrc"
echo ""
