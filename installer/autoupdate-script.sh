#!/bin/bash

# Emacs4art autoupdate script
# Intended to be called from within emacs
# $ bash update-script.sh /path/to/emacs4art
# Written by Hlöðver Sigurðsson <hlolli@gmail.com>
# May, 2018

emacs4art_directory=$1
username=$(whoami)
tmp_dir=~/.emacs-4art-tmp
git_changed=0

if [ -z "$emacs4art_directory" ]; then
    echo "No parameter for emacs4art directory location"
    exit 0
fi

# Make sure there's write access to the emacs4art directory
# https://stackoverflow.com/a/14104522

INFO=( $(stat -L -c "%a %G %U" $DIR) )
PERM=${INFO[0]}
GROUP=${INFO[1]}
OWNER=${INFO[2]}

ACCESS=0
if (( ($PERM & 0002) != 0 )); then
    # Everyone has write access
    ACCESS=1
elif (( ($PERM & 0020) != 0 )); then
    # Some group has write access.
    # Is user in that group?
    gs=( $(groups $username) )
    for g in "${gs[@]}"; do
        if [[ $GROUP == $g ]]; then
            ACCESS=1
            break
        fi
    done
elif (( ($PERM & 0200) != 0 )); then
    # The owner has write access.
    # Does the user own the file?
    [[ $username == $OWNER ]] && ACCESS=1
fi

if [$ACCESS -eq 0]; then
    echo "No write access, not updateing emacs4art"
    exit 0
fi


# Create temporary directory for working within
rm -rf $tmp_dir
mkdir $tmp_dir


# Check for the presence of git
git --version 2>&1 >/dev/null # improvement by tripleee
GIT_IS_AVAILABLE=$?

# When git is present, see if emacs4art
# directory has a git tracker
HAS_DOT_GIT=0

if [ $GIT_IS_AVAILABLE -eq 0 ]; then
    HAS_DOT_GIT="$(git rev-parse --is-inside-work-tree 2>/dev/null)"
fi


# Borrowed from the lein downloader
HTTP_CLIENT=${HTTP_CLIENT:-"wget -O"}
if type -p curl >/dev/null 2>&1; then
    if [ "$https_proxy" != "" ]; then
        CURL_PROXY="-x $https_proxy"
    fi
    HTTP_CLIENT="curl $CURL_PROXY -f -k -L -o"
fi


function download_tarball {
    echo ""
    echo $(tput setaf 2)"--> Downloading emacs4art..."$(tput sgr0)
    echo ""
    $HTTP_CLIENT $tmp_dir/emacs4art.zip https://github.com/panaeolus/emacs4art/zipball/master

    # Unzip zipball
    unzip $tmp_dir/emacs4art.zip -d $tmp_dir/

    # Copy over the new files
    cp -rf $tmp_dir/emacs4art/* $emacs4art_directory

}


function git_clone {
    echo ""
    echo $(tput setaf 2)"--> Cloning emacs4art..."$(tput sgr0)
    echo ""
    git clone https://github.com/panaeolus/emacs4art.git $tmp_dir/emacs4art

    # Copy over the new files
    cp -rf $tmp_dir/emacs4art/* $emacs4art_directory

}

function git_forcefully_pull {
    git fetch --all
    git reset --hard origin/master
    git pull origin master
}

if [ "$HAS_DOT_GIT" ]; then
    git_forcefully_pull
elif [ $GIT_IS_AVAILABLE -eq 0 ]; then
    git_clone
else
    download_tarball
fi



