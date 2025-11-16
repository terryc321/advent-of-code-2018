
" sandblocks code editor "

Metacello new
  baseline: 'Sandblocks';
  repository: 'github://hpi-swa/Sandblocks:master/packages';
  load: #tutorial.

SBEditor openExample.

" Extend the default browser with a block display mode:
  (if you pass `true` it will be the default mode) "
CodeHolder addSandblocksDefault: false.

" Note: By default squeak absorbs ctrl+up/down for scrolling.
        You can turn this off with this line:  "
HandMorph synthesizeMouseWheelEvents: false.

"really cool editor "



