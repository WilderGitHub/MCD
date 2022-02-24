import streamlit as st
import pandas as pd
x = st.slider('Nivel de confianza',3,10)
st.write(x, 'cuadradin', x * x)

#@st.cache
#def cargar():
#    tienda = pd.read_csv("store_data.csv")
#    return tienda
tienda = st.cache(pd.read_csv)("store_data.csv", header=None)
st.dataframe(tienda.head(x))