# SCRIPT TO COPY SOURCE FILES TO A REMOTE SERVER.


# clear
echo -e "\n\n\n"
date
echo -e "===============================\n"


# {{{ CONSTANTS 

# Change these accordingly:
remoteUser="delixir"
remoteIP="172.16.42.7"
# remoteIP="192.168.8.101"
remoteDir="/home/delixir"
project="haskoin"
remoteProjectDir="$remoteUser@$remoteIP:$remoteDir/$project"
# }}}


# {{{ FUNCTIONS 
copyFiles() { # Copy the backend files to the remote server.
  # {{{
  echo "Copying files..."
  rsync -a -e ssh            \
    --exclude=".*"           \
    --exclude="*.pdf"        \
    --exclude="start.sh"     \
    .                        \
    $remoteProjectDir
  echo -e "Done.\n"
  # }}}
}

runRemoteCommand() { # Run a given command on the remote machine via ssh.
  # {{{
  ssh $remoteUser@$remoteIP $1
  # }}}
}

runStackTest() { # Run the tests for the given exercise.
  # {{{
  case $1 in
    all)
      echo -e "Running all the tests:\n"
      runRemoteCommand "cd $remoteDir/$project && stack test --fast"
      ;;
    *)
      echo -e "Running tests for Chapter $1 - Exercise $2:\n"
      runRemoteCommand "cd $remoteDir/$project && stack test --fast --test-arguments='--match \"Chapter $1 - Exercise $2\"'"
      ;;
  esac
  # }}}
}

lint() {
  # {{{
  echo -e "/-------------------------------- hlint --------------------------------\\"
  hlint src
  echo -e "\-------------------------------- hlint --------------------------------/\n"
  # }}}
}

lintAndCopy() {
  # {{{
  lint
  copyFiles
  # }}}
}
# }}}



case $1 in
  test)
    # {{{
    lintAndCopy
    runStackTest $2 $3
    ;;
    # }}}
  *)
    # {{{
    lintAndCopy
    ;;
    # }}}
esac
