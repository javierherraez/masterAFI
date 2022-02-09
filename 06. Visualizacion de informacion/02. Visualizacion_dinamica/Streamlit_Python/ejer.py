from os import getcwd, sep
from statistics import mean
import streamlit as st  
import pandas as pd
from plotnine import *

st.title("Salarios a nivel nacional")

#### Varias columnas con mismo ancho

col1, col2, col3 = st.columns(3)

ees = pd.read_csv("ees2014.csv", sep = ";")
ees = ees.rename({'var_tipocontrato': 'Tipo de contrato', 'var_grupo_cnae': 'CNAE', 'var_antiguedad':'Antigüedad'}, axis=1)

with col1:
    seleccion = st.selectbox(
        "Variable en el eje X",
        ("Tipo de contrato", "CNAE", "Antigüedad"))


with col3:
    pais = st.radio(
        "Separar por sexo",
    ('Sí', 'No'))


g = ggplot(ees) + geom_col(aes(seleccion, "salario_bruto_anual_medio")) + theme_minimal()

st.pyplot(ggplot.draw(g))



