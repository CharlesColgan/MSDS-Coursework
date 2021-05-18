# -*- coding: utf-8 -*-
"""
Created on Sun Sep 20 11:06:37 2020

@author: charlescolgan
"""

class Error(Exception):
    'Base class for other exceptions'
class NegativeError(Error):
    'Rasie when input is a negative number'
def kinetic_energy():
    'Takes user input of vaules for mass and velocity.'
    'Raises erros if the values supplied are non-flaot or negative'
    check_mass=False 
    check_vel=False
    neg_check=False
    'Flags for validation'  
    while check_mass == False or check_vel == False or neg_check == False:
        mass=input("Enter a value for mass in kilograms: ") # user input for mass
        velocity=input("Enter a value for velocity in meters per second: ") # user input for velocity
        'User input'
        print()
        try:
            numb_mass=float(mass) # read in input as float
            numb_vel=float(velocity) # read in input as float
            if numb_mass < 0 or numb_vel < 0: # Check if either input value is negative
                'Negative number check'
                raise NegativeError
            else:
                check_mass=True # Flag to show succesful validation
                check_vel=True  # Flag to show succesful validation
                neg_check=True  # Flag to show succesful validation
                KE=0.5*numb_mass*numb_vel**2 # Provided formula for Kinetic Enrgy
                print(KE,"meters per second squared")
            break
        except ValueError:
            print("Please enter only integer values")# user reminded of proper input type
            print()
        except NegativeError:
            print("Please enter only positive integer values")# user reminded of proper input type
            print()
            
KE=kinetic_energy() # Calculates and displays kinetic enregy from values supplied by user