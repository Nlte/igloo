# fisher
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# env variable
set -gx IGLOO_PATH_USER_CONFIG $HOME/.config
set -gx EDITOR vim
#set -gx FZF_DEFAULT_OPTS "--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C\
#			--color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B"
set -gx RIPGREP_CONFIG_PATH $IGLOO_PATH_USER_CONFIG/ripgrep/config


# locals
set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8


# path
set PATH $HOME/Library/Python/3.7/bin $PATH
set PATH $HOME/igloo/snowblocks/bin $PATH
set PATH /usr/local/opt/llvm/bin $PATH
set PATH /usr/local/opt/qt/bin $PATH
set PATH $HOME/utils/binaries $PATH


# alias
alias vim=nvim
# direnv
eval (direnv hook fish)

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /usr/local/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

