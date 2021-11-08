# Reddit Comment Tree Visualization

See live version at https://kovasap.github.io/reddit-tree.html.

## TODOs

* Fix release build dragging errors https://github.com/thheller/shadow-cljs/issues/955.
* Make edges disappear when node is invisible to make the tree look like it's
  growing as the time slider is used.
* Try on larger data sets - add fail safes for when dataset crashes code.
* Add histogram or text describing how comment post timing (relative to OP)
  effects its chances to get many upvotes/responses.
* Add histogram showing node scores. Would be super cool if there was an
  animated transition where the nodes in the graph moved to stack up on each
  other in the histogram.
* Play with different force parameters for different nodes
  (https://github.com/d3/d3-force).  For example, the OP node could repel other
  nodes more than normal.

## Development Commands

### Development mode
```
npm install
npx shadow-cljs watch app
```
start a ClojureScript REPL
```
npx shadow-cljs browser-repl
```
### Building for production

```
./release.bash
```

This will put all the final artifacts in the "release" directory.
