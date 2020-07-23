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


# path
set PATH $HOME/Library/Python/3.7/bin $PATH
set PATH $IGLOO_PATH_USER_CONFIG/bin $PATH
set PATH /usr/local/opt/llvm/bin/ $PATH


# alias
alias vim=nvim
