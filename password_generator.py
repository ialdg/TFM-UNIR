"""
# -*- coding: utf-8 -*-
Created on Tue Jul 28 04:18:31 2020
from turtle import *
import os
os. getcwd()
os.chdir('D:\\98_uso_python\\2_pruebas')
import pandas as pd
import numpy as np
import matplotlib.pyplot
import turtle
@author: ivanlorenzo
"""
import string as st
import sys
from random import choices


def main():
    """Función principal del programa"""
    #cadena = st.punctuation + st.ascii_lowercase + st.digits + st.ascii_uppercase + "¡?"
    # alternativa:
    cadena = '\\6noqc3eC_/.(Yzñ}v<;ZN-|{fbPw[8Ñl¡O+?iuAh!~:&5r"127LH9G^4=kQ]MW`xjyUmE¿*Fpt0VS,X$gBdaR)>%#KITDJs@\''
    n = int(sys.argv[1])
    s = "".join(choices(cadena, k=n))
    print("La contraseña de %d caracteres es %s" % (n, s))


main()
