#!/bin/sh

cd data
for i in *.csv; do
  cp -p ~/.local/state/stacker/$i .
done

# EOF
