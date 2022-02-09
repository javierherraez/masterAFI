# -*- coding: utf-8 -*-
"""
Created on Sat Jan 22 18:41:31 2022

@author: RPM6364
"""


import streamlit as st
import pandas as pd
import numpy as np
from plotnine import *



def set_home():
  
    df = pd.DataFrame({
    'animal': ['gato','perro', 'caracol', 'serpiente'],
    'edad':['3 años', '5 meses', '5 días', '1 año'],
    'característica': ['mamífero', 'mamífero', 'molusco', 'reptil']
    })

    ### Varias columnas con mismo ancho

    col1, col2, col3 = st.columns(3)

    col1.write(df)
    col2.write(df)
    col3.write(df)

def set_data():
    
    st.header('Checkbox input')

    if st.checkbox('Mostrar datos'):
        df = pd.DataFrame(
           np.random.randn(20, 3),
           columns=['col1', 'col2', 'col3'])

        st.dataframe(df)
     

def set_analisis():

    st.header('Selectbox input')

    df = pd.DataFrame({
        'animal': ['gato','perro', 'caracol', 'serpiente'],
        'edad':['3 años', '5 meses', '5 días', '1 año'],
        'característica': ['mamífero', 'mamífero', 'molusco', 'reptil']
        })

    seleccion = st.selectbox(
        'Elige el animal que quieres conocer',
         df['animal'])

    st.write('Has elegido: ', df[df['animal'] == seleccion])



def set_visualizacion():

    df = pd.DataFrame({
    'animal': ['gato','perro', 'caracol', 'serpiente'],
    'edad (años)':['3', '5', '5', '1'],
    'característica': ['mamífero', 'mamífero', 'molusco', 'reptil']
    })

    g = ggplot(df, aes(x = 'animal', y = 'edad (años)')) + geom_col()

    st.pyplot(ggplot.draw(g))