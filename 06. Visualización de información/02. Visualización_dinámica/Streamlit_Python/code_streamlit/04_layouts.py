# -*- coding: utf-8 -*-
"""
Created on Tue Nov 30 18:11:15 2021

@author: RPM6364
"""


import streamlit as st  
import pandas as pd

#########################
## Menu lateral
#########################

sidebar_prueba2 = st.sidebar.header("Bienvenido a nuestra web")

sidebar_prueba1 = st.sidebar.selectbox("Elige tu género",
    ("Hombre", "Mujer", "Prefiero no decirlo")
)


#########################
## Organización en columnas
#########################

df = pd.DataFrame({
    'animal': ['gato','perro', 'caracol', 'serpiente'],
    'edad':['3 años', '5 meses', '5 días', '1 año'],
    'característica': ['mamífero', 'mamífero', 'molusco', 'reptil']
    })

#### Varias columnas con mismo ancho

col1, col2, col3 = st.columns(3)

col1.write(df)
col2.write(df)
col3.write(df)

##########################
### Varias columnas con ancho proporcional a lo indicado

col1, col2, col3 = st.columns([3, 1, 2])

col1.write(df)
col2.write(df)
col3.write(df)

#########################

## Más de un objeto por columna

with col1:
  st.header("Df1")
  col1.write(df)  

with col2:
  st.header("Df2")
  col2.write(df)  

with col3:
  st.header("Df3")
  col3.write(df)  
  
#########################
## Contenedor que se puede extender o contraer.
#########################


with st.expander("Ver comentario datos"):
    st.write("""Los datos anteriores recogen la edad de distintos animales, así como su clasificación según sus características animales.""")


  
#########################
## Contenedor con muchos elementos
## Este container permite añadir muchos elementos al mismo sitio de la app, sin ir en orden. 
#########################

container = st.container() 
container.write("Esto es el container")

st.write("Esto no")

container.write("Esto es el container de nuevo")


#########################
## Contenedor con un único elemento
#########################

container = st.empty() 
container.write("Esto es el container 2")

st.write("Esto no 2")

container.write("Esto es el container de nuevo 2")

#########################
## Esto puede servir, por ejemplo, para ir sobreescribiendo un dato: 

import time

with st.empty():
    for seconds in range(60):
        st.write(f"⏳ Han pasado {seconds} segundos")
        time.sleep(1)
    st.write("✔️ 1 minute over!")


