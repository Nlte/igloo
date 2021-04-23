fzf_select_if_more_than_two() {
    if [ $# -eq 1 ]; then
        echo $1
        return 0
    fi
    ret=$(printf '%s\n' $@ | fzf --header="fzf text at the bottom")
    echo $ret
}

sed_example_escape() {
    escaped=$(echo $1 | sed 's/\./\\./g; s/\//\\\//g ; s/\*/\\\*/g')
    echo $escaped
}
# example:
# libdir="/opt/my/lib"
# escaped=$(sed_example_escape "${libdir}")
# find "${libdir}" -name '*.py' | sed "s/${escaped}//g"

do_something_with_find() {
    libdir=/opt/my/lib
    find "${libdir}" -name '*.py' -print0 | while read -d $'\0' file
    do
        echo "Do something with $file"
    done
}
