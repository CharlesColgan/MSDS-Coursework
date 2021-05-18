# -*- coding: utf-8 -*-
"""
Created on Tue Sep 15 18:11:34 2020

@author: charlescolgan
"""

def validate_input():
    'Checks whether user input is an integer'
    val=False
    while val == False:
        inp=input("Enter a numerical score between 0.0 and 100.0: ") # User input
        try:
            numb=int(inp) # Check if input is an integer between 0 and 100
            if numb < 0 or numb > 100:
                raise ValueError
            else:
                val=True # Flag to show succesful validation
                return(numb) # acceptable user input value is returned
        except ValueError:
            print("please enter a valid score") # user reminded of proper input type
def calculate_grade():
    'Displays grade value based on given grade scheme'
    if numb>=90:
        print(numb,":Your grade is A")
    elif numb >= 80:
        print(numb,":Your grade is B")
    elif numb >= 70:
        print(numb,":Your grade is C")
    elif numb >= 60:
        print(numb,":Your grade is D")
    elif numb < 60:
        print(numb,":Your grade is F")
numb=validate_input() #Checks to see if user input an inteeger
numb=calculate_grade() # Calculates and displays users grade