# -*- coding: utf-8 -*-
"""
Created on Tue Oct 20 14:09:39 2020

@author: charlescolgan
"""

#insurance.txt

import numpy as np
"Used for loading, saving, and perfoming calculations on data"

import pandas as pd
"Used for better indexing"

def main():   
    check = False#Validation flag
    
    while check == False:
        
        try:
            file = input("Please input file name: ")#Takes user input of file
            
            print()
            
            names = ["age", "sex", "bmi", "children", "smoker", "region", "expense"]
            #Data names
    
            formats = [np.float, "S100", np.float, np.float ,"S100", "S100", np.float]
            #Data formats
            
            raw = np.loadtxt(file, dtype={"names":names, "formats":formats}, skiprows = 1)
            #Load txt file
            
            raw = pd.DataFrame(data = raw, columns = names)
            #Convert array to data frame
            
            args=[raw["age"], 
                  raw["bmi"], 
                  raw.loc[raw["sex"] == b'female' ,"bmi"],
                  raw.loc[raw["sex"] == b'male' ,"bmi"], 
                  raw.loc[raw["smoker"] == b'yes' ,"bmi"], 
                  raw.loc[raw["smoker"] == b'no' ,"bmi"], 
                  raw.loc[raw["region"] == b'northwest' ,"bmi"], 
                  raw.loc[raw["region"] == b'southwest' ,"bmi"], 
                  raw.loc[raw["region"] == b'northeast' ,"bmi"], 
                  raw.loc[raw["region"] == b'southeast' ,"bmi"], 
                  raw.loc[raw["children"] > 2 ,"bmi"]]
           #Arguments for filtering data 
            
            stats=[[], [], []]#Empty list for pre-allocation
            
            for element in args:
                "Compute mean, stdev, and median of args"
                stats[0].append(element.mean())
                
                stats[1].append(element.std())
                
                stats[2].append(element.median())
                        
            ops = ["mean", "stdev", "median"]
            
            fin = pd.DataFrame(columns = ops)#Pre-allocate data frame for args
            
            fin["mean"] = pd.Series(stats[0])#Populate means column with means
            
            fin["stdev"] = pd.Series(stats[1])#Populate stdevs column with stdevs
            
            fin["median"] = pd.Series(stats[2])#Populate medians column with medians
            
            fin = fin.to_numpy()#convert to numpy array to save
            
            np.savetxt("insuracne_stats.txt", fin, header = "Mean, Stdv, Median")# save to txt file
            
            check = True#approve validation
            
        except OSError:
            print("file not found, please enter file name: ")
            print()
              
main()

