import os
import time
import matplotlib.pyplot as plt
import numpy as np
import pickle
import pystan

plt.style.use('ggplot')

filename = "translated"

sm = pickle.load(open(filename + '.pkl', 'rb'))

schools_dat = {'y' : [28,  8, -3,  7, -1,  1, 18, 12],
                'sigma' : [15, 10, 16, 11,  9, 11, 10, 18]}

stime = time.time()
fit = sm.sampling(data=schools_dat, iter=10000, chains=6, n_jobs=1) #, diagnostic_file=(filename + ".txt"))	
sample_time = time.time() - stime

with open('stats.txt', 'w') as f:
	f.write("Translated sample time: " + (str(sample_time)) + "\n")
	
toplot = fit.extract(pars=["tau","theta"])
x = toplot["theta"][:,0]
y = np.log(toplot["tau"])

#plt.scatter([-1000], [-1000])

fig = plt.scatter(x, y, alpha=0.6, s=0.5)
plt.xlabel('theta[0]')
plt.ylabel('log(tau)')
plt.ylim(-10, 3)
plt.xlim(-10, 25)

plt.savefig(filename + ".pdf")
