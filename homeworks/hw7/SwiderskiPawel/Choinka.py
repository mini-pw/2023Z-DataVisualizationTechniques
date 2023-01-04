import matplotlib.pyplot as plt
import random
import math

def trisample(A, B, C):
    """
    Given three vertices A, B, C,
    sample point uniformly in the triangle
    """
    r1 = random.random()
    r2 = random.random()

    s1 = math.sqrt(r1)

    x = A[0] * (1.0 - s1) + B[0] * (1.0 - r2) * s1 + C[0] * r2 * s1
    y = A[1] * (1.0 - s1) + B[1] * (1.0 - r2) * s1 + C[1] * r2 * s1

    return (x, y)

# The base of the tree will be a triangle
base_size = 1.5

# The base of the tree will be a triangle
points = [
    [-base_size, 0],
    [base_size, 0],
    [0, base_size+1],
]

# Plot the base of the tree
plt.fill(*zip(*points), color='green')

# The trunk of the tree will be a rectangle
trunk_width = 0.5
trunk_height = 0.2

plt.fill(
    [-trunk_width / 2, trunk_width / 2, trunk_width / 2, -trunk_width / 2],
    [-trunk_height, -trunk_height, 0, 0],
    color='brown',
)

# Add some ball decorations to the tree
random.seed(312345)
num_decorations = 40
for i in range(num_decorations):
    x,y = trisample([-base_size, 0],
    [base_size, 0],
    [0, base_size+1])
    size = 10
    plt.plot(
        x, y, marker='o', markersize=size, markeredgecolor='black', markerfacecolor='red', alpha=1
    )

plt.axis('equal')
plt.show()