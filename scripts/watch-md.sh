#!/bin/bash

MD_DIR="/Users/dev/docs/notes"

fswatch -0 "$MD_DIR" -e ".*" -i "\\.md$" |
    xargs -0 -n1 -I{} md2pdf "{}" \
        \
        --config-file ~/config/dotfiles/.mdconfig.json # --basedir "~/docs/pdf-notes" \
# md2pdf $MD_DIR/**/*.md --basedir "~/docs/pdf-notes" -w
