import turtle

# insructions: an L-system string
# langle: left angle, angle: right angle, distance: duh.
# start: either a string or a tuple of (width, height) for the starting position
# file: name of file to save eps of image
# width, height: width and height of the screen drawing area.
def draw(instructions, langle, rangle, distance, start, width=960, height=810, file='turtle.eps'):

    screen = turtle.getscreen()
    # Controls animation speed.
    screen.tracer(1000,0)

    # Increases drawing canvas size without increasing window size
    # The effect is that scrollbars are automatically added to the window
    # From turtle docs on screensize()
    turtle.screensize(width, height)

    turtle.hideturtle()

    turtle.penup()

    if start in ['centre', 'center']:
        pass
    elif start in ['bottom_centre','bottom_center']:
        turtle.goto(0,-turtle.window_height()/2)
    elif start in ['center_left','centre_left']:
        turtle.goto(-turtle.window_width()/2,0)
    elif start in ['bottom_left']:
        turtle.goto(-turtle.window_width()/2,-turtle.window_height()/2)
    elif start in ['bottom_right']:
        turtle.goto(turtle.window_width()/2,-turtle.window_height()/2)
    elif start in ['top_left']:
        turtle.goto(-turtle.window_width()/2,turtle.window_height()/2)
    elif isinstance(start, tuple):
        (w, h) = start
        turtle.goto(w,h)

    turtle.pendown()

    # Tilts the turtle by angle.
    # Not really useful.
    #turtle.left(langle)
    
    stack = []

    for cmd in instructions:
        if cmd in ['f','g']:
            turtle.forward(distance)
        if cmd in ['h']:
            turtle.penup()
            turtle.forward(distance)
            turtle.pendown()    
        elif cmd == 'b':
            turtle.backward(distance)
        elif cmd == '+':
            turtle.right(rangle)
        elif cmd == '-':
            turtle.left(rangle)
        elif cmd in ['x','y']:
            pass
        elif cmd=='[':
            stack.append((turtle.position(), turtle.heading()))
        elif cmd==']':
            position, heading = stack.pop()
            turtle.penup()
            turtle.setposition(position)
            turtle.setheading(heading)
            turtle.pendown()    


    turtle.update()
    screen.getcanvas().postscript(file=file,width=width,height=height)

    turtle.exitonclick()
