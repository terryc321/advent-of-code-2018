# this is my first python program
import tkinter as tk

# point as a  tuple (x,y )
bricks = {}
sands = {}
water = {}
todos = []


minX = False
maxX = False
minY = False
maxY = False

# python = is assignment
#recognise either x= or y =
def x_or_y(str):
    if (str[0] == 'x'):
        return True
    elif (str[0] =='y'):
        return False
    else:
        raise ValueError('neither x= or y=')

# ord chr 
def oneVal(str , i):
    n = 0
    a = 0 
    lens = len(str)
    if (i < lens and (str[i] >= '0' and str[i] <= '9')):
        a = ord(str[i]) - ord('0')
        n = a
        i = i + 1
        while (i < lens and (str[i] >= '0' and str[i] <= '9')):
            a = ord(str[i]) - ord('0')
            n = (n * 10) + a
            i = i + 1
        return [n , i]
    else:
        return False



def threeVals(str):
    i = 1
    nums = []
    lens = len(str)
    while i < lens:
        r = oneVal(str,i)
        if r == False:
            pass
        else:
            nums = nums + [r[0]]
            i = r[1]
        i = i + 1 
    if len(nums) == 3 :
        return nums
    else:
        raise ValueError("expected three values from threeVals")



def get_content(): 
    global minX , maxX , minY ,maxY
    # read input
    lines = {}
    content = ""
    with open('input.txt', 'r') as file:
        with open('output.dat', 'w') as file2:        
        #with open('example.txt' , 'r') as file:
            content = file.read()
            lines = content.splitlines()
            for line in lines:
                r = threeVals(line)
                if (line[0] == 'x'):
                    x = r[0]
                    y1 = r[1]
                    y2 = r[2]
                    file2.write(f"(x {x} {y1} {y2})\n")
                    # default values 
                    if (minX == False):
                        minX = x 
                    if (maxX == False):
                        maxX = x
                    if (minY == False):
                        minY = y1 
                    if (maxY == False):
                        maxY = y2                
                    for y in range(y1,y2 + 1):
                        bricks[(x,y)] = True
                        # maximums
                        if (x < minX) :
                            minX = x 
                        if (y < minY) :
                            minY = y 
                        if (x > maxX) :
                            maxX = x 
                        if (y > maxY) :
                            maxY = y 

                elif (line[0] == 'y'):
                    y = r[0]
                    x1 = r[1]
                    x2 = r[2]
                    file2.write(f"(y {y} {x1} {x2})\n")
                    # default values 
                    if (minX == False):
                        minX = x1 
                    if (maxX == False):
                        maxX = x2
                    if (minY == False):
                        minY = y 
                    if (maxY == False):
                        maxY = y                    
                    for x in range(x1,x2 + 1):
                        bricks[(x,y)] = True
                        # maximums 
                        if (x < minX) :
                            minX = x 
                        if (y < minY) :
                            minY = y 
                        if (x > maxX) :
                            maxX = x 
                        if (y > maxY) :
                            maxY = y 

                else:
                    ValueError("expected either x= or y=")
                
            #print(lines)
        # parse it into x=A...y=A .. B
        # parse it into y=A ..x=A .. B

def isBrick(x,y):
    if ((x,y) in bricks):
        return True 
    return False

def isWater(x,y):
    if ((x,y) in water):
        return True 
    return False


def isSand(x,y):
    if ((x,y) in sands):
        return True 
    return False

# say come out sprinkler moving down - increasing Y 
def unwind(x,y):
    global minX , maxX , minY ,maxY    
    if y > maxY :
        print("y exceeding maximum Y ")
        return 
    if x < (minX - 3):
        print("x exceeding min X sub 3 -- possible error ")
        return 
    if x > (maxX + 3):
        print("x exceeding max X plus 3 -- possible error")
        return 
    if isBrick(x,y):
        return 

    water[(x,y)] = True 
    l = solve_left_or_right(x-1,y,0-1)
    r = solve_left_or_right(x+1,y,0+1)
    # determine what to do 
    print("l and r =>" , [l,r])
    if (l == 1 and r == 1): 
        unwind(x,y-1)
    else :
        print("unwind - unhandled case l ,r " , [l,r])
        return 0
    



# say come out sprinkler moving down - increasing Y 
# sprinkler at 500 , 0 

# waterfall == solve 
def solve(x,y):
    global minX , maxX , minY ,maxY    
    if y > maxY :
        print("y exceeding maximum Y ")
        return 
    if x < (minX - 3):
        print("x exceeding min X sub 3 -- possible error ")
        return 
    if x > (maxX + 3):
        print("x exceeding max X plus 3 -- possible error")
        return 
    if isBrick(x,y):
        return 1

    water[(x,y)] = True 
    if isBrick(x,y+1):        
        l = solve_left_or_right(x-1,y , 0-1)
        r = solve_left_or_right(x+1,y , 0+1)
        # determine what to do 
        print("l and r =>" , [l,r])
        if (l == 1 and r == 1): 
            unwind(x,y-1)
        else :
            print("unhandled case l ,r " , [l,r])
            return 0
    else:
        solve(x,y+1)


# move left if hit brick - stop
# move left if brick below and not at left and not at left below
#        E o
#        E X
# water fall condition
#
def solve_left_or_right(x,y,dir):
    global minX , maxX , minY ,maxY , todos    
    if x < (minX - 3):
        print("x exceeding min X sub 3 -- possible error ")
        raise ValueError('x exceeding maxX') 
    if x > (maxX + 3):
        print("x exceeding max X plus 3 -- possible error")
        raise ValueError('x exceeding maxX')
    if isBrick(x,y):
        return 1
    # brick below me - 
    if isBrick(x,y+1):
        water[(x,y)] = True 
        return solve_left_or_right(x+dir,y,dir)
    if isWater(x,y):
        # already water here - keep going?
        return solve_left_or_right(x+dir,y,dir)

    if isWater(x,y+1):
        water[(x,y)] = True 
        return solve_left_or_right(x+dir,y,dir)
    # no brick and no water so must it be a new waterfall?
    todos = todos + [(x,y)]

    

class AnimationWindow:
    global minX , minY , maxX , maxY , bricks
    def __init__(self, root):
        self.minX = minX
        self.maxX = maxX
        self.minY = minY 
        self.maxY = maxY
        self.scale = 10 
        self.offsetX = 0
        self.offsetY = 0
        self.root = root
        self.root.title("Animated Rectangle")
        
        # Create Canvas
        self.canvas = tk.Canvas(root, width=1000, height=700, bg='white')
        self.canvas.pack(fill="both", expand=True)  # Allows the canvas to resize with the window
        self.canvas.bind("<Configure>", self.on_resize)
        self.canvas.pack()
        
        # Create a red rectangle
        self.rect = self.canvas.create_rectangle(50, 50, 100, 100, fill='red')
        self.rect = self.canvas.create_rectangle(150, 50, 100, 100, fill='blue')

        # Animation state
        self.is_running = False
        self.x_velocity = 2
        self.y_velocity = 2
        self.delay = 20  # milliseconds between frames
                
        # Bind keyboard events
        self.root.bind('=', self.view_zoom_out) 
        self.root.bind('-', self.view_zoom_in) 
        self.root.bind('<Prior>', self.view_page_up) # page up
        self.root.bind('<Next>', self.view_page_down) #page down
        self.root.bind('<Right>', self.view_right)
        self.root.bind('<Left>', self.view_left)
        self.root.bind('<Right>', self.view_right)
        self.root.bind('<Down>', self.view_down)
        self.root.bind('<Up>', self.view_up)
        self.root.bind('<space>', self.toggle_animation)
        self.root.bind('<Escape>', self.quit_app)
        self.root.bind('<n>', self.prev)
        self.root.bind('<m>', self.next)
        
    def view_zoom_in(self,event):
        print("zooming in")
        self.scale = self.scale - 1 
        if (self.scale < 1):
            self.scale = 1
        self.my_redraw()

    def view_zoom_out(self,event):
        print("zooming out")
        self.scale = self.scale + 1 
        self.my_redraw()


    def view_left(self,event):
        #print("view moved left\n")
        self.offsetX = self.offsetX + 1 
        self.my_redraw()

    def view_right(self,event):
        #print("view moved right\n")
        self.offsetX = self.offsetX - 1 
        self.my_redraw()

    def view_up(self,event):
        #print("view moved up\n")
        self.offsetY = self.offsetY + 1 
        self.my_redraw()

    def view_down(self,event):
        #print("view moved down\n")
        self.offsetY = self.offsetY - 1             
        self.my_redraw()

    def view_page_up(self,event):
        #print("view moved up\n")
        self.offsetY = self.offsetY + 30
        self.my_redraw()

    def view_page_down(self,event):
        #print("view moved down\n")
        self.offsetY = self.offsetY - 30             
        self.my_redraw()

    def equiv_x(self , x):
        x = x - self.minX + self.offsetX
        x = x * self.scale
        return x

    def equiv_y(self , y):
        y = y - self.minY + self.offsetY
        y = y * self.scale
        return y

    def my_redraw(self):
        # Clear the canvas
        self.canvas.delete("all")
        # Redraw using the new dimensions from the event
        for key in bricks.keys():
            (x,y) = key 
            x = self.equiv_x(x)
            y = self.equiv_y(y)
            self.canvas.create_rectangle(x, y, x + self.scale, y + self.scale, fill='red')
        # draw sprinkler
        (x,y) = (self.equiv_x(500),self.equiv_y(0))
        self.canvas.create_rectangle(x, y, x + self.scale, y + self.scale, fill='blue')

        for key in water.keys():
            (x,y) = key 
            x = self.equiv_x(x)
            y = self.equiv_y(y)
            self.canvas.create_rectangle(x, y, x + self.scale, y + self.scale, fill='blue')
        # Create a red rectangle
        #self.rect = self.canvas.create_rectangle(50, 50, 100, 100, fill='red')
        #self.rect = self.canvas.create_rectangle(150, 50, 100, 100, fill='blue')

        # Initial draw
        #self.canvas.create_text(200, event.height-20, text="Press SPACE to start/stop, Escape to quit")
        print('todos size =>' , len(todos))
        print('water size =>' , len(water))
        
    def on_resize(self , event):
        self.my_redraw()




    def next(self , event):
        print ("next event!??\n")

    def prev(self , event):
        global todos
        #print ("prev event!??\n")
        if todos == []:
            return 
        (x,y) = todos.pop()
        solve(x,y)
        self.my_redraw()

        



    def toggle_animation(self, event):
        if self.is_running:
            self.is_running = False
            self.canvas.itemconfig(self.rect, fill='red')
        else:
            self.is_running = True
            self.canvas.itemconfig(self.rect, fill='green')
            self.animate()

    def animate(self):
        if not self.is_running:
            return
        
        # Update position
        self.canvas.move(self.rect, self.x_velocity, self.y_velocity)
        
        # Get current coordinates
        coords = self.canvas.coords(self.rect)
        
        # Bounce off walls
        if coords[0] <= 0 or coords[2] >= 400:
            self.x_velocity = -self.x_velocity
        if coords[1] <= 0 or coords[3] >= 300:
            self.y_velocity = -self.y_velocity
            
        # Schedule next frame
        self.root.after(self.delay, self.animate)

    def quit_app(self, event):
        self.root.destroy()

if __name__ == "__main__":
    # call to get the content from input file
    get_content()
    print("maximums x axis =>" , [minX ,maxX] , " y axis =>" ,[ minY , maxY])
    solve(500,1)


    root = tk.Tk()
    app = AnimationWindow(root)
    root.mainloop()   

