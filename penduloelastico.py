from numpy import sin, cos
import numpy as np
import matplotlib.pyplot as plt
import scipy.integrate as integrate
import matplotlib.animation as animation

g = 9.8  # acceleration due to gravity, in m/s^2
l = 1.0  # length of pendulum 1 in m
L2 = 1.0  # length of pendulum 2 in m
m= 1.0  # mass of pendulum 1 in kg
M2 = 1.0  # mass of pendulum 2 in kg
k=22
gamma=10.3
w0=0.0
F=g*l
def derivs(state, t):

    dydx = np.zeros_like(state)
    dydx[0] = state[1]
    dydx[1] = -1/(l+state[2])*(2*state[1]*state[3]+g*sin(state[0]))
    dydx[2]=state[3]
    dydx[3]=(l+state[2])*state[1]**2+g*cos(state[0])-k/m*state[2]
    return dydx

# create a time array from 0..100 sampled at 0.05 second steps
dt = 0.05
t = np.arange(0, 20, dt)

# th1 and th2 are the initial angles (degrees)
# w10 and w20 are the initial angular velocities (degrees per second)
th1=90
z1=m*g/l
v1=0
v2=0
# initial state
state = np.radians([th1,v1,z1, v2])

# integrate your ODE using scipy.integrate.
y = integrate.odeint(derivs, state, t)

x1 =np.sin(y[:, 0])+y[3,3]*sin(y[:, 0])
y1=-np.cos(y[:, 0])+y[3,3]*cos(y[:, 0])
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
    thisx = [0,x1[i]]
    thisy = [0,y1[i]]

    line.set_data(thisx, thisy)
    time_text.set_text(time_template % (i*dt))
    return line, time_text


ani = animation.FuncAnimation(fig, animate, range(1, len(x1)),
                              interval=dt*1000, blit=True, init_func=init)
plt.show()