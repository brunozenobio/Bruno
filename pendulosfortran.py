from numpy import sin, cos
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.integrate as integrate
import matplotlib.animation as animation
data=pd.read_csv('datosprk.dat',header=None, sep = ",",names = [ "t", "x", "y"])
print(data)
t=data['t']
x=data['x']
y=data['y']
x=np.array(x)
y=np.array(y)
print(x)
dt = 0.001
t = np.arange(0, 100, dt)
fig = plt.figure()
ax = fig.add_subplot(111, autoscale_on=False, xlim=(-2, 2), ylim=(-2, 2))
ax.set_aspect('equal')
ax.grid()

line, = ax.plot([], [], 'o-', lw=2)
time_template = 'time = %.1fs'
time_text = ax.text(0.05, 0.9, '', transform=ax.transAxes)

def init():
    line.set_data([], [])
    time_text.set_text('')
    return line, time_text


def animate(i):
    thisx = [0,x[i]]
    thisy = [0,y[i]]

    line.set_data([0,x[i]], [0,y[i]])
    time_text.set_text(time_template % (i*dt))
    return line, time_text


ani = animation.FuncAnimation(fig, animate, range(1, len(y)),
                              interval=dt*1000, blit=True, init_func=init)
plt.show()