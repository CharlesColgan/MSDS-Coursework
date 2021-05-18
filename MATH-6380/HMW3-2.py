# -*- coding: utf-8 -*-
"""
Created on Wed Sep 23 17:00:00 2020

@author: charlescolgan
"""
from tabulate import tabulate
'Use tabulate to form tables'

class Error(Exception):
    'Base class for other exceptions'
class TypeError(Error):
    'Rasie when input is a not a .txt file'

def tbl():
    'Function for creating table from a supplied text file of proper length'
    
    valid = False #Flag for validation
    
    while valid == False:
        readable = input("File to be tabled: ")# User input of file name
        
        try: 
            if readable.endswith(".txt"):
                file = open(readable,"r")
                steps = file.readlines()
                ' read file as a list'
            
                P=[]
                mean = []
                mim=[]
                mxm=[]
                Rows=[]
                'Pre-allocation for lists'
                          
                Headers=["Month","Average","Minimum","Maximum"]
                Months=["Jan","Feb","Mar","Apr","May","Jun","Jly","Aug","Sep","Oct","Nov","Dec"]
                'Descripstive info for table'
                
                for element in steps:
                    P.append(element.strip())
                    'Remove line break elemets and form new list'
    
                for element in P:
                    if isinstance(element, int):
                        return(P)
                    else:
                        P=list(map(int, P))
                'Change any strings in P to integers'
                                     
                num_lines = 0
                for line in P:
                    num_lines += 1
                    
                'Count number of lines in P'
                    
                if num_lines > 365:
                    print("Warning: file continas more than 365 data point.")
                    print("Only the first 365 data points will be used")
                    print()
                'Warns user if not all data points are used'
                    
                Part=[P[0:31], P[31:59], P[59:90], P[90:121], P[121:151], P[151:181],\
                        P[181:212], P[212:243], P[243:273], P[273:304], P[304:334], P[334:365]]
                'Partition list by months'
                    
                for i in range(0,12):
                    mean.append(round(sum(Part[i])/len(Part[i]),2))
                    mim.append(min(Part[i]))
                    mxm.append(max(Part[i]))
                    Rows.append([Months[i],mean[i],mim[i],mxm[i]])
                'Calculate average, minimum, and maximum for each sub list in Part and create'
                'rows for the table'
                               
                print(tabulate(Rows,Headers))#print table
                valid = True
                file.close()  
            else:
                valid = False
                raise TypeError # remind user of proper file extension
                file.close()
            break
 
        except TypeError:
            print("File was not of type txt")
            print()
        except ValueError:
            print("File was not of the appropriate length")
            print()
        except ZeroDivisionError:
            print("File has less than 365 data points")
            print()
tbl()








