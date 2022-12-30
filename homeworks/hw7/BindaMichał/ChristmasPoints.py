import random
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import matplotlib.animation as animation
import time


def is_point_in_triangle(x, y, triangle_vertices):
  A = triangle_vertices[0]
  B = triangle_vertices[1]
  C = triangle_vertices[2]
  area1 = abs(x * (A[1] - B[1]) + A[0] * (B[1] - y) + B[0] * (y - A[1])) / 2
  area2 = abs(x * (B[1] - C[1]) + B[0] * (C[1] - y) + C[0] * (y - B[1])) / 2
  area3 = abs(x * (C[1] - A[1]) + C[0] * (A[1] - y) + A[0] * (y - C[1])) / 2
  triangle_area = abs(A[0] * (B[1] - C[1]) + B[0] * (C[1] - A[1]) + C[0] * (A[1] - B[1])) / 2
  return area1 + area2 + area3 == triangle_area


triangle_vertices = [(1,0), (2,0), (1.5,20)]


def create_xyz(incl_yellow):
    random.seed(42)
    coordinates = []

    #create brown points
    for i in range(1000):
      y = random.uniform(-2, 0)
      x = random.uniform(1.4,1.6)
      z = "brown"
      coordinates.append((x, y, z))
    #create random green points
    for i in range(10000):
      x = random.uniform(1, 2)
      y = random.uniform(0, 20)
      if is_point_in_triangle(x, y, triangle_vertices):
        z = "forestgreen"
        coordinates.append((x, y, z))
    #create random yellow points
    for i in range(400):
      x = random.uniform(1, 2)
      y = random.uniform(0, 20)
      if is_point_in_triangle(x, y, triangle_vertices):
        z = "yellow" if incl_yellow else "forestgreen"
        coordinates.append((x, y, z))
    #create random blue points
    for i in range(4000):
      y = random.uniform(0, 20)
      x = (y/2)%2
      if is_point_in_triangle(x, y, triangle_vertices):
        z = "purple"
        coordinates.append((x, y, z))

    x_coords = [x for x, y, z in coordinates]
    y_coords = [y for x, y, z in coordinates]
    z_color = [z for x, y, z in coordinates]

    return x_coords, y_coords, z_color


flag = True
fig = plt.figure()

while True:
    xs, ys, zs = create_xyz(flag)
    plt.clf()
    # Create point graph
    plt.scatter(xs, ys, c = zs)
    plt.scatter(1.5,20, c = "gold",s = 600)
    plt.draw()
    plt.pause(0.1)

    flag = not flag



