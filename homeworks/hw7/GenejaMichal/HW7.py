import random
from turtle import *


def draw_wood():
	color('brown')
	pendown()
	begin_fill()
	fd(20)
	for i in range(5):
		lt(90)
		fd(40)
	end_fill()
	penup()


def set_up():
	rt(90)
	fd(250)
	lt(90)


def draw_tree():
	color("green")
	pendown()
	begin_fill()
	rt(90)
	for i in range(3):
		fd(131 - 50 * i)
		lt(120)
		fd(240 - 70 * i)
		rt(120)
	rt(120)
	for i in range(3):
		fd(100 + 70 * i)
		lt(120)
		fd(31 + 50 * i)
		rt(120)
	end_fill()
	penup()


def draw_ornaments():
	title("Press to put a ball on a Christmas Tree")
	ht()
	screen.onclick(draw_ball)


def draw_ball(x, y):
	screen.onclick(None)
	c = random.choice(["blue", "red", "yellow", "white"])
	goto(x - 10, y)
	color(c)
	pendown()
	begin_fill()
	circle(10)
	end_fill()
	penup()
	screen.onclick(draw_ball)


if __name__ == '__main__':
	screen = Screen()
	penup()
	shape("turtle")
	set_up()
	draw_wood()
	draw_tree()
	draw_ornaments()
	done()
