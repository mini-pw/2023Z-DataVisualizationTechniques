import random
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import matplotlib.animation as animation

def is_point_in_triangle(x, y, triangle_vertices):
  # Calculate the areas of the three triangles formed by the point and the vertices of the triangle
  A = triangle_vertices[0]
  B = triangle_vertices[1]
  C = triangle_vertices[2]
  area1 = abs(x * (A[1] - B[1]) + A[0] * (B[1] - y) + B[0] * (y - A[1])) / 2
  area2 = abs(x * (B[1] - C[1]) + B[0] * (C[1] - y) + C[0] * (y - B[1])) / 2
  area3 = abs(x * (C[1] - A[1]) + C[0] * (A[1] - y) + A[0] * (y - C[1])) / 2
  # Calculate the area of the triangle
  triangle_area = abs(A[0] * (B[1] - C[1]) + B[0] * (C[1] - A[1]) + C[0] * (A[1] - B[1])) / 2
  # Check if the areas of the three triangles are equal to the area of the original triangle
  return area1 + area2 + area3 == triangle_area

# Set the vertices of the triangle
triangle_vertices = [(1,0), (2,0), (1.5,20)]

# Generate 100 random coordinates
coordinates = []
#create tandom green points
for i in range(10000):
  x = random.uniform(1, 2)
  y = random.uniform(0, 20)
  if is_point_in_triangle(x, y, triangle_vertices):
    z = "green"
    coordinates.append((x, y, z))
#create random yellow points
for i in range(400):
  x = random.uniform(1, 2)
  y = random.uniform(0, 20)
  if is_point_in_triangle(x, y, triangle_vertices):
    z = "yellow"
    coordinates.append((x, y, z))
#create blue serpentyna
for i in range(4000):
  y = random.uniform(0, 20)
  x = (y/2)%2
  if is_point_in_triangle(x, y, triangle_vertices):
    z = "purple"
    coordinates.append((x, y, z))
#add star
x = 1.5
y = 20
z = "yellow"
coordinates.append((x,y,z))
print(coordinates)

x_coords = [x for x, y, z in coordinates]
y_coords = [y for x, y, z in coordinates]
z_color = [z for x, y, z in coordinates]


# Create the point graph
fig = plt.scatter(x_coords, y_coords, c = z_color)
plt.scatter(1.5,20, c = "yellow",s = 600)
plt.show()
