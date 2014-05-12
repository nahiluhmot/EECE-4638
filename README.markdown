# EECE 4638

## How to build Haskell code

    cabal clean && cabal build

## How to run output

    ./dist/build/project-name/project-name data-file

Example:

    ./dist/build/graph-coloring/graph-coloring data/color12-4.input

## Where to run

Rather than having you install all dependencies for Haskell on your computer, we set up a VPS that you can SSH into.

    Location:          eece-4638.tlunter.com
    SSH Key:           aws-key.pem (aws-key.ppk for Putty)
    Project locations: /opt/eece-4638

In each project directory, to build the source code, run:
 
    make clean build

To test the runtime, run:

    make generate
