# SCRIPT TO COPY SOURCE FILES TO A REMOTE SERVER.


clear
date
echo -e "===============================\n"


# {{{ CONSTANTS 

# Change these accordingly:
remoteUser="delixir"
remoteIP="172.16.42.7"
remoteDir="/home/delixir"
project="haskoin"
remoteProjectDir="$remoteUser@$remoteIP:$remoteDir/$project"

# Note the project directory is presumed to be structured as:
#
# $project              # main directory
# | $project-frontend   # frontend directory
# | $project-backend    # backend directory
# | server.sh
# | README.md
# | etc.

# }}}


# {{{ FUNCTIONS 
copyFiles() { # Copy the backend files to the remote server.
  # {{{
  echo "Copying files..."
  rsync -a -e ssh            \
    --exclude=".*"           \
    src                      \
    $remoteProjectDir
  echo -e "Done.\n"
  # }}}
}
# }}}


echo -e "/-------------------------------- hlint --------------------------------\\"
hlint src
echo -e "\-------------------------------- hlint --------------------------------/\n"
copyFiles
