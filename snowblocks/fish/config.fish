# fisher
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# env variable
set -gx IGLOO_PATH_USER_CONFIG $HOME/.config
set -gx EDITOR vim
set -gx FZF_DEFAULT_OPTS "
    --color=fg:#e5e9f0,bg:#2e3440,hl:#81a1c1
    --color=fg+:#e5e9f0,bg+:#2e3440,hl+:#81a1c1
    --color=info:#eacb8a,prompt:#bf6069,pointer:#b48dac
    --color=marker:#a3be8b,spinner:#b48dac,header:#a3be8b"
set -gx RIPGREP_CONFIG_PATH $IGLOO_PATH_USER_CONFIG/ripgrep/config


# locals
set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8


# path
set PATH $HOME/Library/Python/3.7/bin $PATH
set PATH $HOME/igloo/snowblocks/bin $PATH
set PATH /usr/local/opt/llvm/bin $PATH
set PATH /usr/local/opt/qt/bin $PATH
set PATH $HOME/utils/binaries/ $PATH
set PATH $HOME/miniconda3/bin/ $PATH
set PATH $HOME/.roswell/bin $PATH
set PATH $HOME/.cargo/bin $PATH


# alias
alias vim=nvim
# direnv
eval (direnv hook fish)

# >>> conda initialize >>>
eval conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<
source ~/.byndercli/local.env
