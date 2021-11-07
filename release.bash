#!/bin/bash

npx shadow-cljs release app
cp -r public/* release/
