# -*- coding: utf-8 -*-
"""
Created on Tue Oct 27 15:51:05 2020

@author: charlescolgan
"""

#titanic_training.xlsx

#titanic_test.xlsx

import pandas as pd
'used for data storage and manipulation'

def read():
    'Read in training and test data from .xlsx file'
    
    check_train = False #validate training
    
    check_test = False #validate training
    
    
    names = ["Id", "pclass", "gender", "age", "sibsp", "parch", "fare", "embarked", "survived"]
    #Column names
    
    while check_train == False:  
        try:
           
            global file_train 
           
            file_train = pd.read_excel(input("Please enter file name for training data: "), names = names)            
            #Form data frame for training data
        
            check_train = True #Show valid
            
        except FileNotFoundError:
            print("file not found, please enter file name for training data: ")
            print() 
            
        except OSError:
            print("file not found, please enter file name for test data: ")
            print()
            
    while  check_test == False:
        try:  
            global file_test
            
            file_test = pd.read_excel(input("Please enter file name for test data: "), names = names[0:8])
        
            check_test = True #Show valid
            
        except FileNotFoundError:
            print("file not found, please enter file name for test data: ")
            print()
            
        except OSError:
            print("file not found, please enter file name for test data: ")
            print()
            
    
def OneR():
    'Implement OneR classification'
    
    checks = ["gender", "pclass", "sibsp", "parch", "embarked"] #Features to check with OneR
    
    file_test["survived"] = pd.Series(dtype = "int") #Location for classing
    
    global result
    
    results = pd.DataFrame(index = range(392), columns = checks)
    
    for element in checks:
        for i in file_train[element].unique():
            
            if len(file_train[(file_train[element] == i) & (file_train["survived"] == 1)]) > len(
                    file_train[(file_train[element] == i) & (file_train["survived"] == 0)]):
                
                file_test.loc[file_train[element] == i, "survived"] = 1
                                     
            else:
                file_test.loc[file_train[element] == i, "survived"] = 0
                
        results[element] = file_test["survived"]
        
def save():
    'Save results of OneR'
    
def success():
    'determine success rate of OneR'
                    
def main():
    read()
    print()
    OneR()
    print()
    #save()
    print()
    #success()

main()
        



        
