#!/bin/bash

MD_DIR="/Users/dev/docs/notes"

fswatch -0 "$MD_DIR" -e ".*" -i "\\.md$" |
    xargs -0 -n1 -I{} ~/config/scripts/test.sh "{}"

# TODO: this creates pdf files in the same folder as the md files. We want to move them into another oflder though
# fixme: saving in typora now leads to two compilations...
