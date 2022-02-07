# -*- coding: utf-8 -*-
"""
Created on Tue Nov 30 20:59:15 2021

@author: RPM6364
"""

#  HACER LA MISMA QUE EN SHINY

import streamlit as st
import pandas as pd
from plotnine import  *

ees = pd.read_csv('ees2014.csv', sep =  ';', encoding = 'latin1')

ees['salario_factor'] = ees.salario_bruto_anual_medio*ees.factor_elev

df_cnae  =  ees.groupby(['var_grupo_cnae', 'var_sexo'], as_index = False)[['salario_factor', 'factor_elev']].sum()
df_cnae['salario'] = df_cnae.salario_factor/df_cnae.factor_elev
df_cnae = df_cnae[['var_grupo_cnae', 'var_sexo', 'salario']]
df_cnae.columns = ['variable', 'var_sexo', 'salario']


df_contrato  =  ees.groupby(['var_tipocontrato', 'var_sexo'], as_index = False)[['salario_factor', 'factor_elev']].sum()
df_contrato['salario'] = df_contrato.salario_factor/df_contrato.factor_elev
df_contrato = df_contrato[['var_tipocontrato', 'var_sexo', 'salario']]
df_contrato.columns = ['variable', 'var_sexo', 'salario']



df_antiguedad  =  ees.groupby(['var_antiguedad', 'var_sexo'], as_index = False)[['salario_factor', 'factor_elev']].sum()
df_antiguedad['salario'] = df_antiguedad.salario_factor/df_antiguedad.factor_elev
df_antiguedad = df_antiguedad[['var_antiguedad', 'var_sexo', 'salario']]
df_antiguedad.columns = ['variable', 'var_sexo', 'salario']


st.title('Salarios 2014')

st.subheader('Salarios a nivel nacional')

col1, col2, col3 = st.columns(3)

with col1:
    ejeX = st.selectbox(
        'Variable en el eje X',
        ['CNAE', 'Tipo contrato', 'Antigüedad'])

tema = theme(panel_grid_major_y = element_line(linetype='dashed'), panel_grid_major_x = element_blank(), panel_grid_minor_y = element_blank())


if ejeX == 'CNAE':
    df = df_cnae.copy()
elif ejeX == 'Tipo contrato':
    df = df_contrato.copy()
elif ejeX == 'Antigüedad':
    df = df_antiguedad.copy()
    tema = theme(panel_grid_major_y = element_line(linetype='dashed'), panel_grid_major_x = element_blank(), panel_grid_minor_y = element_blank(), axis_text_x=element_text(size = 7))

with col3:
    sexo = st.radio(
         'Separar por sexo',('No', 'Sí'))

if sexo == 'No':
    p = ggplot(df, aes("variable", "salario")) + geom_col(fill = 'lightblue') + labs(x = ejeX, y = 'Salario') + theme_bw() + tema
    st.pyplot(ggplot.draw(p))
    
else:
    p = ggplot(df, aes("variable", "salario", fill = "var_sexo")) + geom_bar(stat = 'identity',position = 'dodge') + scale_fill_manual(values = ['lightblue', 'lightgreen']) + labs(x = ejeX, y = 'Salario', fill = 'Sexo') + theme_bw() + tema  
    st.pyplot(ggplot.draw(p))
