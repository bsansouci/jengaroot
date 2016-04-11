#!/bin/bash

here=$(cd $(dirname $0); pwd)
cd ${here}

# get tarballs (unless already have them)
tmp=/tmp/jengaroot-required-package-tarballs
mkdir -p $tmp
cat package.list | (cd $tmp;
while read x; do
    if [ ! -f $tmp/$(basename $x) ]; then wget $x; fi
done)

# unpack tarballs into [packages], which already contains the necessary [jbuild] files
find $tmp -type f | (cd packages; while read x; do tar -xzf $x; done)

# reorganize ppx_tools, split code into subdirs
if [[ "$OSTYPE" == "linux-gnu" ]]; then
  (cd packages/ppx_tools-ppx_tools_0.99.3
  mv genlifter.ml ppx_metaquot.ml syntax
  mv ast_convenience.ml{i,} ast_mapper_class.ml{i,} lib
  find syntax/*.ml | xargs sed -i 's/ Ast_convenience/ Ppx_tools.Ast_convenience/'
  )
elif [[ "$OSTYPE" == "darwin"* ]]; then
  (cd packages/ppx_tools-ppx_tools_0.99.3
  mv genlifter.ml ppx_metaquot.ml syntax
  mv ast_convenience.ml{i,} ast_mapper_class.ml{i,} lib
  find syntax/*.ml | xargs sed -i '' 's| Ast_convenience| Ppx_tools.Ast_convenience|'
  )
fi
# commit everything to hg
hg init; hg ci -Aqm init
