# -*- coding: utf-8 -*-
"""
Created on Wed Nov 11 16:19:21 2020

@author: charlescolgan
"""

#Ecoli_Data.xlsx

import pandas as pd

import numpy as np

import seaborn as sns

def read():
    'read in file and create seperate columns for Year and Month'
    
    global file #make data frame global
    
    check = False#for validation
    
    while check == False:
        'loop for validation'
        try:   
            file = pd.read_excel(input("Please enter file name: "))
            
            file["Year"] = pd.DatetimeIndex(file["Date Collected"]).year
            
            file["Month"] = pd.DatetimeIndex(file["Date Collected"]).month
            
            check = True#pass validation
            
        except FileNotFoundError:
            print("file name not found.")
            print()
            
        except UnicodeDecodeError:
            print("file is not in the correct format. File should have extension .xlsx")
            print()
    
def means():
    'compute averages'
    
    
    
def plots():
    'create plots'
    
    sns.barplot(x = "Year", y = "E.coli", data = file)#Display E.coli per year as bar graph
    
    sns.barplot(x = "Month", y = "E.coli", data = file)#Display E.coli per month as bar graph
    
    
    
def main():
    read()
    print()
    #means()
    print()
    plots()
    print()
    
main()