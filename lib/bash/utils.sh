
fzf_select_if_more_than_two() {
    if [ $# -eq 1 ]; then
        echo $1
        return 0
    fi
    ret=$(printf '%s\n' $@ | fzf --header="fzf text at the bottom")
    echo $ret
}


sed_escape() {
    # example:
    # libdir="/opt/my/lib"
    # escaped=$(sed_example_escape "${libdir}")
    # find "${libdir}" -name '*.py' | sed "s/${escaped}//g"
    escaped=$(echo $1 | sed 's/\./\\./g; s/\//\\\//g ; s/\*/\\\*/g')
    echo $escaped
}


do_something_with_find() {
    libdir=/opt/my/lib
    find "${libdir}" -name '*.py' -print0 | while read -d $'\0' file
    do
        echo "Do something with $file"
    done
}


bury_copy() {
    mkdir -p `dirname $2` && cp "$1" "$2";
}


bury_copy_src_files() {
    libdir=~/Documents/git/mylib  # must end without /
    destdir=/opt/installedlib  # must end without /
    escapeddir=$(sed_escape "${libdir}")
    find "${libdir}" -name '*.py' -print0 | sed "s/$escapeddir\///g" | while read -d $'\0' file
    do
        dest="${destdir}/${file}"
        src="${libdir}/${file}"
        bury_copy $src $dest
    done
}
