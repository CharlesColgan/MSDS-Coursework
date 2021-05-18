# -*- coding: utf-8 -*-
"""
Created on Tue Sep 29 12:27:40 2020

@author: charlescolgan
"""

import pandas as pd
'used to create table for display'

def main():
    txt_check = False #Tag for validation

    while txt_check == False:
         
            try:
                file = open(input("Please input a file name: "),"r") #Open file input by user
                print()
                txt_check = True #Signal proper file used
                
                wins = {} #Pre-allocation for dictionary
    
                for line in file:
                    strings = line.splitlines()
                    for string in strings:
                        if string not in wins:
                            wins[string] = 1
                        else:
                            wins[string] += 1
                'This creates a list with each entry being a line from the input file.'
                'Then, each string is checked and the correpsonding value in the dictionarry is incremented as'
                'needed'
                
                visu = wins.copy() #Create copy of wins  
                 
                for key in visu:
                    visu[key] = "*"*visu[key]
                'this replaces the value of each key with a corresponding number of asterikss'
                            
                wins_sort = sorted(wins.items(), key=lambda x: x[1], reverse=True) 
                visu_sort = sorted(visu.items(), key=lambda x: x[1], reverse=True)
                #Sorting dictionaries to be used in tables
                 
                win_names = ["Team", "number of wins"] #Column names
                
                win_tab = pd.DataFrame(wins_sort,columns = win_names) #Table of wins numerical
                vis_tab = pd.DataFrame(visu_sort,columns = win_names) #Table of wins visual
                
                print(win_tab)
                print()                                 
                print(vis_tab)
            
                file.close()
            except FileNotFoundError:
                    print("Please enter a valid file name")#Alert user to error
                    print()
   
main()

