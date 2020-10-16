#!/bin/sh

set -e

cd docs

git add --all *

git commit -m "Update the book" || true

git pull

git push -u origin main
