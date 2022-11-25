import numpy as np
import matplotlib.pyplot as plt
import scipy.integrate as integrate
import matplotlib.animation as animation
g=-9.81
v0=25
y0=0
x0=0
phi1=45
phi=np.radians(phi1)
dt = 0.05
t = np.arange(0, 20, dt)
def x(g,v0,x0,t,phi):
    x=x0+v0*np.cos(phi)*t
    return x
def y(g,v0,y0,phi):
    y=y0+v0*np.sin(phi)*t+g*t**2/2
    return y
x1=x(g,v0,x0,t,phi)
y1=y(g,v0,y0,phi)
fig = plt.figure()
ax = fig.add_subplot(111, autoscale_on=False, xlim=(0, 50), ylim=(0, 50))
ax.set_aspect('equal')
ax.grid()

line, = ax.plot([], [], 'o', lw=1)
time_template = 'time = %.1fs'
time_text = ax.text(0.05, 0.9, '', transform=ax.transAxes)


def init():
    line.set_data([], [])
    time_text.set_text('')
    return line, time_text


def animate(i):
    thisx = [0, x1[i]]
    thisy=[0,y1[i]]

    line.set_data(thisx, thisy)
    time_text.set_text(time_template % (i*dt))
    return line, time_text


ani = animation.FuncAnimation(fig, animate, range(1, len(y1)),
                              interval=dt*1000, blit=True, init_func=init)

plt.show()