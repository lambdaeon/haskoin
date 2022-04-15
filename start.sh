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
  rsync -a -e ssh                  \
    --exclude=".*"                 \
    --exclude="dist*"              \
    --exclude="*.pdf"              \
    --exclude="programmingbitcoin" \
    --exclude="start.sh"           \
    .                              \
    $remoteProjectDir
  echo -e "Done.\n"
  # }}}
}

runRemoteCommand() { # Run a given command on the remote machine via ssh.
  # {{{
  ssh $remoteUser@$remoteIP $1
  # }}}
}

runTest() { # Run the tests for the given exercise.
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

runLocalTest() { # Run the tests for the given exercise locally.
  # {{{
  case $1 in
    all)
      echo -e "Running all the tests:\n"
      # stack test --fast
      cabal test --test-show-details always
      ;;
    *)
      argument="Chapter $1 - Exercise $2"
      echo -e "Running tests for $argument:\n"
      final="cabal test --test-show-details always --test-options='--match \"$argument\"'"
      # final="stack test --fast --test-arguments='--match \"$argument\"'"
      touch test.sh
      echo $final >> test.sh
      # source test.sh
      sh test.sh
      rm test.sh
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
    runTest $2 $3
    ;;
    # }}}
  localTest)
    # {{{
    lint
    runLocalTest $2 $3
    ;;
    # }}}
  noLint)
    # {{{
    copyFiles
    ;;
    # }}}
  publish)
    # {{{
    cp README.md .tempReadme.md
    echo -e "\n## Library Directory Structure\n" >> README.md
    echo '```' >> README.md
    tree src >> README.md
    echo '```' >> README.md
    git add .
    git commit -m "$2"
    git push
    mv .tempReadme.md README.md
    ;;
    # }}}
  *)
    # {{{
    lintAndCopy
    ;;
    # }}}
esac
