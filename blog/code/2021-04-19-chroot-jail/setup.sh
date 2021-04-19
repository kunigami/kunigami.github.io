bins=( "/bin/bash" "/bin/ls" "/bin/mkdir" )

NEW_ROOT="$HOME/root"

# clear
sudo rm -rf $NEW_ROOT

for bin_file in "${bins[@]}"
do

    # copy binaries
    bin_dir=$(dirname $bin_file)
    mkdir -p $NEW_ROOT$bin_dir
    cp $bin_file $NEW_ROOT$bin_dir

    deps=$(ldd $bin_file)

    # copy dependencies from binaries
    while read -r dep; do
        dep_file=$(echo $dep | grep -o "\/[a-z0-9_\.\/\-]*")
        if [ ! -z "$dep_file" ]
        then
           echo "$NEW_ROOT$dep_dir"
           dep_dir=$(dirname $dep_file)
           mkdir -p $NEW_ROOT$dep_dir
           cp $dep_file $NEW_ROOT$dep_dir
        fi
    done <<< "$deps"

    mkdir -p $NEW_ROOT"/home"

    # add escape binary
    gcc -static escape_real.c -o $NEW_ROOT"/home/escape"
    # allow the binasry to perform chroot
    sudo setcap 'cap_sys_chroot+ep' $NEW_ROOT"/home/escape"

    sudo chown kunigami_test:kunigami_test $NEW_ROOT"/home"


done
