# -*- coding: utf-8 -*-
"""
Created on Wed Nov 24 18:37:37 2021

@author: RPM6364
"""


import pandas as pd

df = pd.DataFrame({'var1': [1,2,3,4,5]})
df  

x = 33
'x', x  

# También funciona con gráficos (no soporta los gráficos de cualquier libreria)

import matplotlib.pyplot as plt
import numpy as np

arr = np.random.normal(1, 1, size = 250)
fig, ax = plt.subplots()
ax.hist(arr, bins = 30)

fig 

import streamlit as st
from plotnine import *


g = ggplot(df, aes(x = 'var1', y = 'var1')) + geom_line()

st.pyplot(ggplot.draw(g))

