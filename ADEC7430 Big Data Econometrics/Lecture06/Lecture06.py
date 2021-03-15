
#%% 
import os
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import datetime as DT 

#%%
projFld = "/Users/apple/Desktop/ADEC7430 Big Data Econometrics/Lecture06"
outFld = os.path.join(projFld, "Output")
#%%

# initialize the figure
fig = plt.figure()

# define a "fancy" function
def f(x,y, xoffset = 0, yoffset = 0):
    # return(np.exp(-(x^2)-(y^2)))
    # why the extra parentheses? try np.exp(-10^2-10^2)...
    # return(np.exp(-np.power(x+xoffset,2)-0.3*np.power(y+yoffset,2)))
    # return(np.exp(-(x + xoffset)**2 - (y + yoffset)**2))
    # return(np.sin(x + xoffset + y + yoffset)))
    return(np.sin(2*np.pi * (x + xoffset + y + yoffset)))

# try a few values to confirm it meets what you know about the formula
#%%    
# define the grid to support the 3D plot on
xgrid = np.linspace(-4,4,50)
ygrid = np.linspace(-4,4,50).reshape(-1,1)
xgrid 

# break - talk about reshape 
xx = np.array([0,1,2,3,4,5])
xx.reshape(2,3)
xx.reshape(6,1)
xx.reshape(7,1)  # error that cannot reshape of size 6 to (7,1)
xx.reshape(-1,1)   # -1 means "no idea, ask Python to figure out", and 1 means 1"1 column"
# end break about reshape 

plt.imshow(f(xgrid,ygrid))
# Note a few things: 
# the "coordinates" show the indexes, not the actual values, of X and Y 
# if the size of the plot if fixed and small 
# For Spyder, go to Tools -> Preferences -> iPython Cosoles -> Graphics -> Graphics Backend - move to "antomotic reshape" 
# then restart the iPython kernel
# after rerunning the steps from the top to here, you should notice a separate window iwht the plot 
# index position of numbers lie on the axies 

#%% 2d figure 
# now iterate over a range of moving centers, plot and stack
moves = 100
centers = [[-1 + np.cos(2*np.pi*i), np.sin(2*np.pi*i)] for i in np.linspace(0,1,moves)]

imgs = []   # this is am empty list created, ready to stock images 
for i in range(moves):
    imgs.append([
            plt.imshow(
                    f(xgrid,ygrid, xoffset = centers[i][0], yoffset = centers[i][1]) 
                    , animated = True)
            ]
    )
# add to imgs list, imshow - plt.imshow is a way to show the colorful image 
ani = animation.ArtistAnimation(fig, imgs, interval = 50, blit = True, repeat_delay=1000)
plt.show()
ani.save(os.path.join(outFld, '2d_ani.mp4'))
#%% 3d rotating figure
# see various plots for matplotlib at https://matplotlib.org/mpl_toolkits/mplot3d/tutorial.html#surface-plots

from mpl_toolkits.mplot3d import Axes3D
fig = plt.figure()
ax = Axes3D(fig) # idea: modify/rotate axes to rotate the plot

ax.plot_surface(xgrid, ygrid, f(xgrid,ygrid)) # initial plot
# now rotate axes
ax.view_init(elev = 30, azim = 10)
fig

ax.view_init(elev = -30, azim = 10)
fig
# getting a differnet ones rather than normal figure 

#%% now let's play - a few stages into a moview 
zz = f(xgrid, ygrid)

def init():
    ax.plot_surface(xgrid, ygrid, zz)
    return fig, 
# IT'S CORRECT return two objects, one is explicit and one is inexplicit 

# define a series of elevations and azimuth points, moving gradually from one to the next
# let's also keep the elevation between -45 and 45 degrees
nframes = 360
elevazim = [[0,0]]
for i in range(nframes):
    tdelta = np.random.uniform(-3,3)
    tmp_elevazim = elevazim[-1:][0] # last element in that list
    if abs(tmp_elevazim[0] + tdelta) < 45:
        tmp_elevazim[0] += tdelta
    tmp_elevazim[1] += np.random.uniform(0,1)
    elevazim.append(tmp_elevazim.copy())

def animate(i):
    ax.view_init(elev=elevazim[i][0], azim = elevazim[i][1])
    return fig,

t0 = DT.datetime.now()
anim = animation.FuncAnimation(fig, animate, init_func=init, 
                               frames = 360, interval = 40, blit=True)
# the anim is a function to write annimation, which writes on "fig", using "nframes", 
# using "animate" function, "interval of 20 secs; 24 Hzs is cinema frame" 
t1 = DT.datetime.now()
print ("stacking the images took" + str(t1-t0))
anim.save(os.path.join(outFld,'3d_surface_rotate.mp4'), fps=24)
t2 = DT.datetime.now()
print ("blabla" + str(t2-t1))
# MovieWritter ffmpeg unavailable. Trying to use pillow instead. 

""" R codes 
# - install imagemagick 
# install.packages("animation")
# library (animation)

# anipoints <- 100 

# Output <- "/Users/apple/Desktop/ADEC7430 Big Data Econometrics"
saveGIF({
  for (i in l:anipoints){
    newtheta = newtheta + xmove[i]
    newtheta
  }
})
    """
