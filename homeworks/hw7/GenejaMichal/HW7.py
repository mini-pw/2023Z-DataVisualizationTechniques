from turtle import *


def draw_wood():
	color('brown')
	pendown()
	begin_fill()
	fd(25)
	for i in range(5):
		lt(90)
		fd(50)
	end_fill()
	penup()


def set_up():
	rt(90)
	fd(500)
	lt(90)


def draw_tree():
	color("green")
	pendown()
	begin_fill()
	rt(90)
	for i in range(3):
		fd(262-100*i)
		lt(120)
		fd(480-140*i)
		rt(120)
	rt(120)
	for i in range(3):
		fd(200+140*i)
		lt(120)
		fd(62+100*i)
		rt(120)
	end_fill()


if __name__ == '__main__':
	penup()
	set_up()
	draw_wood()
	draw_tree()
	done()
