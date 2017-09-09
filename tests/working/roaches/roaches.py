from pystan import StanModel
import pickle
import time


stime = time.time()
orig_sm = StanModel(file='roaches.stan')
orig_comp_time = time.time() - stime
print (orig_comp_time)

with open('roaches', 'wb') as f:
    pickle.dump(orig_sm, f)


stime = time.time()
orig_sm = StanModel(file='roaches_overdisp.stan')
orig_comp_time = time.time() - stime
print (orig_comp_time)

with open('roaches_overdisp.pkl', 'wb') as f:
    pickle.dump(orig_sm, f)
	
	
	
#stime = time.time()
#trans_sm = StanModel(file='translated.stan')
#trans_comp_time = time.time() - stime
#print (trans_comp_time)

#with open('translated.pkl', 'wb') as f:
#    pickle.dump(trans_sm, f)
	
#with open('stats.txt', 'w') as f:
#	f.write("Original model compilation time: " + (str(orig_comp_time)) + "\n")
#	f.write("Translated model compilation time: " + (str(trans_comp_time)) + "\n")
	
