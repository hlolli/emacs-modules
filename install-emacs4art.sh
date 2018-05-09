#!/bin/bash

# Emacs4art Installer based on Sam Aaron's Emacs-Live installer
# Written by Hlöðver Sigurðsson <hlolli@gmail.com>
# May, 2018

old_config=~/original-emacs-d
tmp_dir=~/.emacs-4art-tmp
username=$(whoami)


if [[ -e $old_config ]]; then

    echo $(tput setaf 1)"emacs4art Installer Warning"$(tput sgr0)

    echo "It looks like I've already stored some of your previous Emacs configuration in: "
    echo $(tput setaf 3)$old_config$(tput sgr0)
    echo "Please move or remove it before running me again."
    echo "I don't want to clobber over valuable files."
    exit 0
fi

# Create temporary directory for working within
rm -rf $tmp_dir
mkdir $tmp_dir


# Check for the presence of git
git --version 2>&1 >/dev/null # improvement by tripleee
GIT_IS_AVAILABLE=$?


# Borrowed from the lein downloader
HTTP_CLIENT=${HTTP_CLIENT:-"wget -O"}
if type -p curl >/dev/null 2>&1; then
    if [ "$https_proxy" != "" ]; then
        CURL_PROXY="-x $https_proxy"
    fi
    HTTP_CLIENT="curl $CURL_PROXY -f -k -L -o"
fi

# Print outro and ask for user confirmation to continue
echo ""
echo ""
echo $(tput setaf 4)
echo "WARNING: This will disable (not remove!) all your personal emacs configuration"
echo "by moveing your ~/.emacs.d (if exists) to ~/original-emacs-d"
echo ""
echo "After the installation is over, you can safely move back the parts that you need"
echo "from your original-emacs-d to the newly created ~/.emacs.d"
echo $(tput sgr0)
echo ""

read -p $(tput setaf 3)"Are you sure you would like to continue? (y/N) "$(tput sgr0)

function download_tarball {
    echo ""
    echo $(tput setaf 2)"--> Downloading emacs4art..."$(tput sgr0)
    echo ""
    $HTTP_CLIENT $tmp_dir/emacs4art.zip https://github.com/panaeolus/emacs4art/zipball/master

    # Unzip zipball
    unzip $tmp_dir/live.zip -d $tmp_dir/
}

function git_clone {
    echo ""
    echo $(tput setaf 2)"--> Cloning emacs4art..."$(tput sgr0)
    echo ""
    git clone https://github.com/panaeolus/emacs4art.git $tmp_dir/emacs4art
}

if [[ $REPLY =~ ^[Yy]$ ]]; then

    # User wishes to install

    # Download emacs4art with git (or as a tarball if git isn't on the system)

    if [ $GIT_IS_AVAILABLE -eq 0 ]; then
        git_clone
    else
        download_tarball
    fi

    created_old_emacs_config_dir=false

    function create_old_dir {
        if $created_old_emacs_config_dir; then
            # do nothing
            true
        else
            echo ""
            echo $(tput setaf 1)
            echo "======================================"
            echo "     Emacs config files detected. "
            echo "======================================$(tput sgr0)"

            mkdir -p $old_config
            echo "# Your Old Emacs Config Files

This directory contains any Emacs configuration files that had existed prior
to installing emacs4art.

To see which files have been preserved:

    ls -allh $old_config

To revert back to your old Emacs configs simply:

    rm -rf ~/.emacs.d
    mv $old_config ~/.emacs.d" > $old_config/HOW_TO_RESTORE_YOUR_OLD_EMACS_CONFIG.md

            created_old_emacs_config_dir=true
        fi
    }

    if [ -e ~/.emacs.d/ ]; then
        create_old_dir
        echo $(tput setaf 1)
        echo "Found ~/.emacs.d config directory"
        echo ""
        mv ~/.emacs.d $old_config
        echo "Moved ~/.emacs.d to $old_config"
        echo "------------------------------------------"
        echo ""
        echo $(tput sgr0)
    fi

    if [ -e ~/.emacs.el ]; then
        create_old_dir
        echo $(tput setaf 1)
        echo "Found ~/.emacs.el config file."
        echo ""
        mv ~/.emacs.el $old_config/.emacs.el
        echo "Moved to $old_config/.emacs.el"
        echo "------------------------------------------"
        echo ""
        echo $(tput sgr0)
    fi

    if [ -e ~/.emacs ]; then
        create_old_dir
        echo $(tput setaf 1)
        echo "Found ~/.emacs config file."
        echo ""
        mv ~/.emacs $old_config/.emacs
        echo "Moved to $old_config/.emacs"
        echo "------------------------------------------"
        echo ""
        echo $(tput sgr0)
    fi

    mkdir ~/.emacs.d
    cp -R $tmp_dir/emacs4art ~/.emacs.d


    echo $(tput setaf 2)"--> Installation Completed"$(tput sgr0)
    echo $(tput setaf 5)
    cat $tmp_dir/outro.txt
    echo $(tput sgr0)
    echo ""
    echo "
# This loads emacs4art, make sure that this command is always at the top init.el
(load-file (expand-file-name (concat user-emacs-directory \"emacs4art/initialize.el\")))

# Here below you can put your own configuration
# for tips and tricks, go to https://github.com/panaeolus/emacs4art
" > ~/.emacs.d/init.el 
    
    rm -rf $tmp_dir

else
    echo "--> Installation aborted."
fi
