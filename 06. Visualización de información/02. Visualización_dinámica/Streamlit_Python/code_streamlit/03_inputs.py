# -*- coding: utf-8 -*-
"""
Created on Tue Nov 30 07:21:52 2021

@author: RPM6364
"""


import streamlit as st  
import numpy as np
import pandas as pd


st.header('Slider input')

x = st.slider('Elige un número')
st.write('Has elegido el ' +  str(x))
st.write('Has elegido el ', x)

#######################################

st.header('Text input')
st.text_input("Escribe tu nombre", key="nombre")

# Podemos acceder al valor del texto con el siguente comando:
if st.session_state.nombre:
    st.write('Hola ' + st.session_state.nombre + ', ¿Cómo estás?')

##############################################
st.header('Button input')

if st.button('Pulse para continuar'):
    st.write('¡Bienvenido al siguiente paso!')
else:
    st.write('')

##############################################
st.header('Checkbox input')

if st.checkbox('Mostrar datos'):
    df = pd.DataFrame(
       np.random.randn(20, 3),
       columns=['col1', 'col2', 'col3'])

    df
 
##############################################

st.header('Selectbox input')

df = pd.DataFrame({
    'animal': ['gato','perro', 'caracol', 'serpiente'],
    'edad':['3 años', '5 meses', '5 días', '1 año'],
    'característica': ['mamífero', 'mamífero', 'molusco', 'reptil']
    })

seleccion = st.selectbox(
    'Elige el animal que quieres conocer',
     df['animal'])

'Has elegido: ', df[df['animal'] == seleccion]

##############################################

st.header('Radiobutton input')


pais = st.radio(
     "Elige qué país prefieres",
('España', 'Italia', 'Alemania'))
# df['animal'])

if pais == 'España':
    st.write('¡Te gusta España!')
else:
    st.write("Te gusta más " + pais + " que España")