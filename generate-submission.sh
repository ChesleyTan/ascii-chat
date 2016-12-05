#!/bin/bash

FILES="*"
EXCLUDE=".git/* src/TODO.txt submission.zip sync.sh"

git log --stat > gitlog.txt
cd src
make clean
cd ..
rm -f submission.zip
zip -r submission.zip $FILES -x $EXCLUDE

