#!/bin/bash

# depends on youtube-dl
# video url has to be in $1

[[ $# -ne 1 ]] && echo "please include playlist url as parameter" && exit 1

subFormat="srt"
# subLanguage="en,zh"
subLanguage="en,zt"

youtube-dl "$1" \
    --playlist-item 1-2 `# for not downloading all files`\
    -o '~/Movies/media/viki/%(title)s.%(ext)s' \
    --write-sub \
        --sub-format "$subFormat" \
        --sub-langs "$subLanguage"
