if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# env variable
set -gx IGLOO_PATH_USER_CONFIG $HOME/.config
set -gx EDITOR vim
set -gx FZF_DEFAULT_OPTS_NORD "
    --color=fg:#e5e9f0,bg:#2e3440,hl:#81a1c1
    --color=fg+:#e5e9f0,bg+:#2e3440,hl+:#81a1c1
    --color=info:#eacb8a,prompt:#bf6069,pointer:#b48dac
    --color=marker:#a3be8b,spinner:#b48dac,header:#a3be8b"
set -gx FZF_DEFAULT_OPTS_SOLARIZED "
    --color fg:-1,bg:-1,hl:$blue,fg+:$base02,bg+:-1,hl+:$blue
    --color info:$yellow,prompt:$yellow,pointer:$base03,marker:$base03,spinner:$yellow"

set -gx FZF_DEFAULT_OPTS $FZF_DEFAULT_OPTS_NORD

set -gx RIPGREP_CONFIG_PATH $IGLOO_PATH_USER_CONFIG/ripgrep/config

# path
set PATH $HOME/.local/bin/ $PATH
set PATH $HOME/igloo/snowblocks/bin $PATH
set PATH $HOME/utils/binaries/ $PATH
set PATH $HOME/opt/bin/ $PATH
set PATH $HOME/.cargo/bin $PATH
set PATH $HOME/opt/kafka_2.13-3.6.0/bin $PATH

# alias
alias vim=nvim
alias python=python3

# local environment variables
set FILE "$HOME/.config/fish/local.fish" && test -f $FILE && source $FILE

# source user functions
source $HOME/.config/fish/userfunctions.fish

# Activate vi mode
fish_vi_key_bindings

export DISPLAY=localhost:0.0
