# Reddit Comment Tree Visualization

## TODOs

* Add comment text on hover (blocked on
  https://github.com/gadfly361/rid3/issues/10#issuecomment-950186633).
* Add link to comment for each node (blocked on
  https://github.com/gadfly361/rid3/issues/10#issuecomment-950186633).
* Make sure dragging works (blocked on
  https://github.com/gadfly361/rid3/issues/10#issuecomment-950186633).
* Try on larger data sets - add fail safes for when dataset crashes code.
* Add histogram or text describing how comment post timing (relative to OP)
  effects its chances to get many upvotes/responses. 
* Add histogram showing node scores. Would be super cool if there was an
  animated transition where the nodes in the graph moved to stack up on each
  other in the histogram.
* Come up with better title
* Maybe add a bar that lets you scroll through time and watch the tree get built
  up.
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
npx shadow-cljs release app
```
