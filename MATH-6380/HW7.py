# -*- coding: utf-8 -*-
"""
Created on Thu Nov  5 10:57:02 2020

@author: charlescolgan
"""

#titanic_traning.csv

import pandas as pd
'Used for storage and manipualtion of data'

def read():
    'read in File'
    
    global file
    
    check = False#validation check
    
    while check == False:
        try:
            file_name = input("Please input file name: ")#Get file name from user
            print()
    
            file = pd.read_csv(file_name)#read in file
            
            check = True#complete validation
            
        except FileNotFoundError:
            print("File not found")
            print()
            
        except UnicodeDecodeError:
            print("file is not in the correct format. File should have extension .csv")
            print()
    
def sever():
    'Compute severity of missing/inconsistent values in file and display in table'
    
    feats = file.copy()

    feats = feats.drop(columns = ["ID"])#Remove unnescary columns
    
    vals = {"pclass":file["pclass"].unique(), 
            "sex":["male", "female"], 
            "age":file["age"].unique(),
            "sibsp":file["sibsp"].unique(), 
            "parch":file["parch"].unique(), 
            "fare":file["fare"].unique(),
            "embarked":["C", "S", "Q"], 
            "survived":file["survived"].unique()}#Dictionary of unique an proper values for each feature
    
    MV = feats.isna().sum()#number of missing values in each feature
    
    MVp = round(100*feats.isna().sum()/len(file),2)#% of missing values in each feature
    
    IV = len(file)-feats.isin(vals).sum()#number of inconsitent values in each feature
    
    IVp = round(100*(len(file)-feats.isin(vals).sum())/len(file),2)#% of missing values in each feature
    
    Sev_Tab = pd.concat([MV, MVp, IV, IVp],axis = 1)#Create table
    
    Sev_Tab.columns = ["Missing Values (MV)", "% of MV (MV/p)", "Inconsistent Values)", "% of IV (IV/p)"]
        #rename columns
    
    issues = feats[file.isna().any(axis = 1) | ~file.isin(vals).any(axis = 1)]
        #display rows contating missing or inconssitent data
    
    print(Sev_Tab)
    print()
    print(issues)
    
def main():
    read()
    print()
    sever()
    print()
    
    
main()
