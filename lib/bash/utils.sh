fzf_select_if_more_than_two() {
    if [ $# -eq 1 ]; then
        echo $1
        return 0
    fi
    ret=$(printf '%s\n' $@ | fzf --header="fzf text at the bottom")
    echo $ret
}
