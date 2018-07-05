#!/bin/bash

EMACS=$(which emacs)

if [ $# -lt 2 ] ; then
    echo "Usage:: " `basename $0` " [ -d ]  item1  item2"
    exit 1
fi

dir="no"
if [ "$1" = "-d" ]; then
    dir="yes"
    item1="$2"
    item2="$3"
else
    if [ -d "$1" -a -d "$2" ]; then
        dir="yes"
    fi
    item1="$1"
    item2="$2"
fi

if [ "$dir" = "no" ]; then

    # Check that files do exist
    if [ ! -f "$item1" ] ; then
        printf "File %s not found.\n" "$item1"
        exit 2
    fi
    if [ ! -f "$item2" ] ; then
        printf "File %s not found.\n" "$item2"
        exit 2
    fi

    # Check whether files are identical or not
    diffdata=`diff "$item1" "$item2"`
    if [ "_" = "_$diffdata" ] ; then
        printf "%s and %s are identical.\n" "$item1" "$item2"
        exit 3
    fi

fi

diff_fn="ediff-files"
if [ "$dir" = "yes" ]; then
    diff_fn="ediff-directories"
    opt="\"\""
fi

# Run Emacs with ediff-files function
printf "Comparing files %s and %s . . .  " "$item1" "$item2"
$EMACS -eval "($diff_fn \"$item1\" \"$item2\" $opt)" && echo done.

exit 0
