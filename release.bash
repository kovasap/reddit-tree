#!/bin/bash

# When I use
# npx shadow-cljs release app
# node dragging does not work for some reason.
npx shadow-cljs compile app

cp -r public/* release/
mkdir -p release/reddit-tree
mv release/js release/reddit-tree/js
mv release/css release/reddit-tree/css
sed -i 's/\/css\/site.css/\/reddit-tree\/css\/site.css/g' release/reddit-tree.html
sed -i 's/\/js\/app.js/\/reddit-tree\/js\/app.js/g' release/reddit-tree.html
rm release/index.html
git add release/**
git commit -m "new release"
git push
