from mpl_toolkits import mplot3d
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np
import h5py


nfiles = 2000
filenames = []

for i in range(nfiles):
	filenames.append('./../output_files/wave_2d_'+'%04d'%(i)+'.h5')

plt.ion()
plt.figure()

for i, filename in enumerate(filenames):
        #print(filename)
        try:
	        f = h5py.File(filename, 'r')
	        x = f['x_grid_points'][:]
	        y = f['y_grid_points'][:]
	        output = f['solution(unp1)'][:]
	        time = f['time(t)'][:]
	        f.close()
	        ax = plt.axes(projection='3d')
	        ax.contourf(x, y, np.transpose(output), levels = np.linspace(-0.02, 0.02, 100), cmap='viridis')
	        ax.set_title("time: %4.2f"%(time))
	        ax.set_xlabel('x')
	        ax.set_ylabel('y')
	        ax.set_zlabel('unp1') 
	        ax.set_zlim(-0.02, 0.02)	
	        plt.pause(0.01)
	        plt.clf()
        except:
	        break 

plt.pause(1.0)
plt.close()

