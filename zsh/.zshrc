# Preferences:
setopt nomatch
unsetopt autocd beep extendedglob notify

# History:
HISTFILE="$XDG_CONFIG_HOME/zsh/.history"
HISTSIZE=1024
SAVEHIST=256
setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS

#
# Completion:
#

setopt MENU_COMPLETE

# Allow menu-style selection (cycling through options):
zstyle ":completion:*" menu select
# Allow .. directory:
zstyle ":completion:*" special-dirs true
# Case-insensitive:
zstyle ":completion:*" matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

autoload -Uz compinit && compinit

zmodload zsh/complist
bindkey -M menuselect '^M' .accept-line

#
# Aliases:
#

alias ls="ls --color=always"
alias l="ls -CFG"
alias la="ls -ACFG"
alias ll="ls -AClFG"

alias mkdir="mkdir -pv"

alias rm="rm -I --preserve-root"
alias mv="mv -i"
alias cp="cp -i"
alias ln="ln -i"

alias e="(emacs &)"
alias vi="nvim"

alias reload="echo Restarting Zsh... && source $HOME/.zshrc"
alias path="echo $PATH | tr ':' '\n'"
alias update="sudo apt-get update && sudo apt-get upgrade"

# Windows clipboard:
alias wincb="powershell.exe Get-Clipboard"

mdcd() {
  mkdir -p "${1}"
  cd "${1}"
}

#
# Prompt:
#

PROMPT="%F{#99b9f3}%~/ %F{#6495ed}$ %f"
