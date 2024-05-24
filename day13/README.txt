
General idea is use scheme programming language to produce an output file "output"
use common lisp to make sdl interactive video window reads "output" and
displays to user , can forward and backward ```the computation ```
as we have the full history of the cart movements at any time step

Keys
n  back one time step 
m  forward one time step

N  back ten time steps
M  forward ten time steps

-  zoom out
=  zoom in
arrow keys move grid on screen




Consistently getting wrong result.

may be a timing issue

when cart hits a corner piece , possibly should determine what direction train should face
then 






visualiser shows sometimes where logic errors occur in programming

at time step 7599 two trains are coming together , but never crash , as though they pass through
each other


decode of track

Q 1 . how to visualise / visualize the track on screen

simple 2 d grid

think that be easy meh ...

bar.scm creates the output file
bar.scm -> output

viz.lisp reads an output file and recreates the graphical display visualise what the trains did
the trains may do silly things , as we shall see
it is not up to viz to detect or sort out problems with output
simply viz shows what the output is telling us


output file format

(begin-track)
...
(track 1 1 #f)            ... no track here
(track 1 1 slash)         ... / corner piece 
(track 2 3 backslash) 	  ... \  corner piece
(track 3 5 horz)      	  ... -  horizontal pipe
(track 3 6 vert)          ...  |  vertical pipe
(track 3 4 cross)         ... +  cross section 
...
(end-track)

;; followed by train simulation data .....

(tick N)                 ... nth tick of simulation
((train-no 7) (x 110) (y 109) (direction down) (internal right))
((train-no 5) (x 56) (y 40) (direction left) (internal right))
...
(tick N+1)
...
...

;; .......


