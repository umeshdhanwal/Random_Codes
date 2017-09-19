# -*- coding: utf-8 -*-
"""
Created on Tue Sep 19 16:32:30 2017

@author: 1537259
"""

import random
import matplotlib.pyplot as plt

n = 1000000    # number of coin tosses in each trial
t = 1000000    # number of trials

prob=[0.2,0.3,0.9,0.4,0.3,0.1]


trials = [sum([random.sample(prob,1) for i in range(n)]) for j in range(t)]

len(trials)

freq = [trials.count(k) for k in range(n+1)]  

print(trials)
print(freq)

plt.bar(range(n+1), freq, align='center')
plt.show()