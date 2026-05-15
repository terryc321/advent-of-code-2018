# this is my first python program
import tkinter as tk

class AnimationWindow:
    def __init__(self, root):
        self.root = root
        self.root.title("Animated Rectangle")
        
        # Create Canvas
        self.canvas = tk.Canvas(root, width=1000, height=700, bg='white')
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
        self.root.bind('<space>', self.toggle_animation)
        self.root.bind('<q>', self.quit_app)
        
        # Initial draw
        self.canvas.create_text(200, 280, text="Press SPACE to start/stop, Q to quit")

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
    root = tk.Tk()
    app = AnimationWindow(root)
    root.mainloop()   