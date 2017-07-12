#!/bin/zsh

grep -c "." ~/Dropbox/todo.txt/todo.txt | awk '{print "TD: "$1}'
