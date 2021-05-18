# -*- coding: utf-8 -*-

"""
Created on Saturday Sep 19 9:48:35 2020
@author: Gal Egozi
"""


def kinetic_energy(m: float, v: float) -> float:
    """
    Accepts the mass and velocity as arguments and outputs the kinetic energy.
    :param m: A non-negative number indicating the mass.
    :param v: A non-negative number indicating the velocity.
    :return: The kinetic energy (computed as 1/2 m*v^2).
    """
    if m < 0 or v < 0:  # A warning is given because physics is weird, especially in quantum mechanics. Trust us.
        print('Warning: either the mass or the velocity entered are negative. Computation will continue, but it may '
              'lead to unexpected results.')  # The actual warning that gets displayed.
    return (m * v ** 2) / 2  # Return the value, which may be negative, but the user would be aware of this.


mass = input('Mass value: ')  # prompt the user for a mass value.
try:  # because user input can be bad.
    mass = float(mass)  # because the input is read as a string, it needs to be converted to float.
except ValueError:  # Cannot do the conversion because input string entered is not valid float.
    print('Warning: the entered mass is invalid. Using 0 for the mass.')  # Let the user know what the program will
    # do with their bad input, replace their input with the number zero.
    mass = 0  # Set the mass to zero because bad input was given.
vel = input('Velocity value: ')  # Prompt the user for a velocity value.
try:  # Did the user enter a float? User input can be bad.
    vel = float(vel)  # Attempt to convert the user inputted velocity to a float (user input is read in as a string).
except ValueError:  # Cannot convert the user input to a float, so bad user input.
    print('Warning: the entered velocity is invalid. Using 0 for the velocity.')  # Letting the user know how the
    # program will handle their bad input.
    vel = 0  # set the velocity to zero as a default value for bad input.
print('Energy is', kinetic_energy(mass, vel))  # Generate the energy value and print it.
