import os
import time
import matplotlib.pyplot as plt
import pickle
import pystan

plt.style.use('ggplot')

filename = "translated"

sm = pickle.load(open(filename + '.pkl', 'rb'))

stime = time.time()
fit = sm.sampling(data={}, iter=100000, chains=6, n_jobs=1) #, diagnostic_file=(filename + ".txt"))	
sample_time = time.time() - stime

with open('stats.txt', 'w') as f:
	f.write("Translated sample time: " + (str(sample_time)) + "\n")
	
toplot = fit.extract(pars=["x","y"])
x = toplot["x"]
y = toplot["y"]

#plt.scatter([-1000], [-1000])

fig = plt.scatter(x, y, alpha=0.6, s=0.5)
plt.xlabel('x')
plt.ylabel('y')
plt.ylim(-10, 15)
plt.xlim(-100, 100)

plt.savefig(filename + ".pdf")
