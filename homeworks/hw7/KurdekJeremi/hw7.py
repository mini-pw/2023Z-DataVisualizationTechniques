import matplotlib.pyplot as plt
from random import randint

plt.figure(figsize=(6, 8), facecolor='lightgreen')

plt.plot([45, 45], [-15, -5], 'brown', linewidth=40)
plt.plot([45],[92],'gold', marker="*", markersize=50)

def draw_sierpinski(points, depth, colors):
    x1, y1, x2, y2, x3, y3 = points
    if depth > 0:
        draw_sierpinski([x1, y1, (x1 + x2) / 2, (y1 + y2) / 2, (x1 + x3) / 2, (y1 + y3) / 2], depth - 1, colors)
        draw_sierpinski([(x1 + x2) / 2, (y1 + y2) / 2, x2, y2, (x2 + x3) / 2, (y2 + y3) / 2], depth - 1, colors)
        draw_sierpinski([(x1 + x3) / 2, (y1 + y3) / 2, (x2 + x3) / 2, (y2 + y3) / 2, x3, y3], depth - 1, colors)
    else:
        color = colors[randint(0, len(colors)-1)]
        plt.fill([x1, x2, x3, x1], [y1, y2, y3, y1], color)

draw_sierpinski([0, 0, 90, 0, 45, 90], 3, colors=['darkgreen'])
draw_sierpinski([0, 0, 90, 0, 45, 90], 7, colors=['white', 'darkgreen','darkgreen','darkgreen'])

plt.axis('off')
plt.savefig('hw7.png')
plt.show()
