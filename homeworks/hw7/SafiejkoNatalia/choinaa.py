import math
import random
import matplotlib.pyplot as plt
import matplotlib as mpl
import matplotlib.pyplot as plt


fig = plt.figure()
ax = fig.add_subplot(111, projection="3d")


#ŚNIEŻYNKI
X=[random.randint(-400,400) for i in range(200)]
Z1=[random.randint(100,400) for i in range(200)]
X1= random.sample(X, k=100)
Y1= random.sample(X, k=100)
Z1= random.sample(Z1, k=100)
ax.scatter(X1, Y1, Z1, c="white", marker="*", s=5)

k = 250
# choinka
Z = [i + 100 for i in range(k)]
X = [math.sin(i / 5) * (k - i) for i in range(k)]
Y = [math.cos(i / 5) * (k - i) for i in range(k)]
ax.scatter(X, Y, Z, c="green", marker="d")

# lancuch
Z1 = [x + 10 for x in Z]
l = len(Z1) - 25
Z1 = Z1[:l]
X = [math.sin(i / 6) * (k - i) for i in range(l)]
Y = [math.cos(i / 6) * (k - i) for i in range(l)]
ax.scatter(X, Y, Z1, c="silver", marker="*")
plt.xlim(-400, 400)
plt.ylim(-400, 400)

# bombki
max = 10
Z = [i + 100 for i in range(1, k - 10, max)]
X = [math.cos(i / 5 + 2) * (k - i + 20) for i in range(1, k - 10, max)]
Y = [math.sin(i / 5 + 2) * (k - i + 20) for i in range(1, k - 10, max)]
ax.scatter(X, Y, Z, c="white", marker="o")
X = [math.cos(i / 5 + 3) * (k - i + 20) for i in range(1, k - 10, max)]
Y = [math.sin(i / 5 + 3) * (k - i + 20) for i in range(1, k - 10, max)]
ax.scatter(X, Y, Z, c="purple", marker="o")
# gwiazda
plt.xlim(-400, 400)
plt.ylim(-400, 400)
ax.scatter(5, 5, k + 110, c="silver", marker="*", s=500)

ax.w_xaxis.set_pane_color((0, 0, 0.7, 0.7))
ax.w_yaxis.set_pane_color((0, 0, 0.7, 0.7))
ax.w_zaxis.set_pane_color((0, 0, 0, 0.9))
ax.text(-200, 50, 400, "Feliz navidad!", color='black', fontweight='bold', fontsize=14, family='cursive')


ax.grid(False)



plt.show()
