#!/bin/bash

# When I use
# npx shadow-cljs release app
# node dragging does not work for some reason.
npx shadow-cljs compile app

cp -r public/* release/
rm release/index.html
git add release/**
git commit -m "new release"
git push
