from turtle import *
import turtle

# insructions: an L-system string
# langle: left angle, angle: right angle, distance: duh.
def draw(instructions, langle, rangle, distance, start):

    turtle.penup()

    if start in ['centre', 'center']:
        pass
    elif start in ['bottom_centre','bottom_center']:
        turtle.goto(0,-turtle.window_height()/2)
    elif start in ['bottom_left']:
        turtle.goto(-turtle.window_width()/2,-turtle.window_height()/2)
    elif start in ['bottom_right']:
        turtle.goto(turtle.window_width()/2,-turtle.window_height()/2)

    turtle.pendown()

    turtle.hideturtle()
    turtle.left(langle)
    turtle.tracer(1e3,0)
    
    wn = turtle.Screen()
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
    turtle.exitonclick()
