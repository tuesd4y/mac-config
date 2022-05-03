#!/bin/bash

echo "Converting $1 to pdf"
# TODO find correct folder instead abc.pdf
cat "$1" | md2pdf >~/abc.pdf
