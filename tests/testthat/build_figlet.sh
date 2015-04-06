#!/bin/bash
set -e
VERSION=2.2.5
ARCHIVE="figlet-${VERSION}.tar.gz"
URL="ftp://ftp.figlet.org/pub/figlet/program/unix/${ARCHIVE}"
BIN="figlet/figlet"

if [ ! -f $BIN ]; then
    echo "*** Downloading figlet"
    if [ ! -f $ARCHIVE ]; then
         curl -O $URL
     fi
    tar -zxf ${ARCHIVE}
    mv figlet-${VERSION} figlet
    echo "*** Building figlet"
    make -C figlet
fi
