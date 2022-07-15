#
# .profile
#

# PATH

## [Ruby](https://wiki.archlinux.org/title/ruby#Setup)
if ruby -v 2>/dev/null>/dev/null; then
    export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
    export PATH="$PATH:$GEM_HOME/bin"
fi

## Home
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.local/bin:$PATH

# VARS

## GnuPG
export GPG_TTY=$(tty)

## Editor
if vim --version 2>/dev/null>/dev/null; then
    export VISUAL=vim
    export EDITOR="$VISUAL"
fi

## libvirt
export LIBVIRT_DEFAULT_URI="qemu:///system"

## paru
export PARU_CONF=$HOME/.config/paru/paru.conf

# Greeter for xinit

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  echo
  read -p "Hi, cool man! Do you want to access Xmonad? [y/Y]: " -n 1 -r
  echo
  if [[ $REPLY =~ ^[yY]$ ]]; then
    exec xinit xmonad
  fi
fi

