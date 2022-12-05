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
k=10
gamma=1.3/m
w0=0.0
F=g*l
gamma=0.5
def derivs(state, t):

    dydx = np.zeros_like(state)
    dydx[0] = state[1]
    dydx[1] = -l*(m*g+k*l)*sin(state[0])/(m*l**2)-k*state[0]-gamma*state[1]

    return dydx

# create a time array from 0..100 sampled at 0.05 second steps
dt = 0.05
t = np.arange(0, 20, dt)

# th1 and th2 are the initial angles (degrees)
# w10 and w20 are the initial angular velocities (degrees per second)
th=np.radians(90)
# initial state
state = ([th, 0])

# integrate your ODE using scipy.integrate.
y = integrate.odeint(derivs, state, t)

x1 =l*cos((y[:, 0]))
y2=-l*sin((y[:, 0]))+l
fig = plt.figure()
ax = fig.add_subplot(111, autoscale_on=False, xlim=(-2, 2), ylim=(-2, 2))
ax.set_aspect('equal')
ax.grid()

line, = ax.plot([], [], 'o--', lw=2)
line1, = ax.plot([], [], '-', lw=2)
line2, = ax.plot([], [], '-', lw=2)
time_template = 'time = %.1fs'
time_text = ax.text(0.05, 0.9, '', transform=ax.transAxes)


def init():
    line.set_data([], [])
    line1.set_data([], [])
    line2.set_data([], [])
    time_text.set_text('')
    return line,line1, time_text


def animate(i):
    thisx = [0, x1[i]]
    thisy = [0,y2[i]]
    line.set_data(thisx, thisy)
    line1.set_data(0, [0,4/3*l])
    line2.set_data([0,x1[i]],[4/3*l,y2[i]])
    time_text.set_text(time_template % (i*dt))
    return line, time_text,line1,line2


ani = animation.FuncAnimation(fig, animate, range(1, len(y)),
                              interval=dt*1000, blit=True, init_func=init)
plt.show()