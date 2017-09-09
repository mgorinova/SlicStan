import os
import time
import matplotlib.pyplot as plt
import pickle
import pystan

plt.style.use('ggplot')

sm = pickle.load(open('neals_naive.pkl', 'rb'))

fit = sm.sampling(data={}, iter=4000, chains=6, n_jobs=1)

toplot = fit.extract(pars=["x","y"])
x = toplot["x"]
y = toplot["y"]

#plt.scatter([-10000],[-10000])
#plt.scatter([-20000],[-20000])
#plt.scatter([-20000],[-20000])
#plt.scatter([-20000],[-20000])
#plt.scatter([-20000],[-20000])

fig = plt.scatter(x, y, alpha=0.6, s=0.5)
plt.xlabel('x')
plt.ylabel('y')
plt.ylim(-10, 15)
plt.xlim(-100, 100)

plt.savefig("neals_naive.pdf", bbox_inches='tight')
