def get_input() -> (float, float):
    """
    Takes in user input and returns a validated value for height and weight. Requires users to input a positive
    height and a non-negative weight.
    :return: A height and weight value, if they are floating point values. The height must be positive, but the weight
    must be non-negative.
    """
    height: float = float(input('Enter a height value in inches: '))  # Reading in the height. If height is not a
    # number, it will be handled by the float casting.
    weight: float = float(input('Enter a weight value in pounds: '))  # Reading in the weight. The float casting
    # handles inputs that are not numbers.
    if height <= 0:  # Height must be positive because negative height is not reasonable and a zero height causes
        # division by zero.
        if height == 0:  # A height of zero causes a division by zero.
            raise ValueError('The height cannot be zero.')  # Pass the error to the user that there will be a
            # division by zero.
        else:  # Because bmi uses the square of the height, it may be okay to assume that a positive height was
            # intended. Therefore, it is reasonable to warn the user and replace height with its absolute value. This
            # is a reasonable assumption because when trying to compute BMI with negative heights, the square of the
            # height means that the absolute value of height, not the raw value of height, matters. Therefore,
            # a person with weight w and height -h technically has the same BMI as a person with weight w and height
            # h. Although it is not physically possible to have a person with negative height, a more subtle solution
            # is to handle a negative height by replacing it with its positive counterpart (absolute value of the
            # height) is chosen.
            height = -1 * height
            print(f'Warning: the height {-1 * height:d} was used. The height of {height:d} will be used instead.')
            # Warning the user of the action that will be taken.
    if weight < 0:  # Weight should not be negative.
        raise ValueError('Weight must be non-negative.')  # Pass the error to the user.
    return height, weight  # A valid height and weight, so return them to the user.


def calculate_BMI(height: float, weight: float) -> float:
    """
    Calculates the BMI from a valid height and weight. Bad input to this function may result in errors or unexpected
    results.
    :param height: A validated positive floating point number is recommended, but a non-zero floating point
    number will be accepted.
    :param weight: A validated non-negative floating point number.
    :return: The BMI. If either of the inputs are not floats or if the height is zero, an error may occur. Results may
    be unexpected if weight or height is negative.
    """
    return weight * 703 / (height ** 2)  # Using the formula from the instructions.


def calculate_weight_category(bmi: float) -> str:
    """
    Takes a floating point BMI and determines whether a person is underweight, optimal, or overweight.
    :param bmi: The float parameter indicating the BMI.
    :return: A string either 'underweight', 'optimal', or 'overweight'.
    """
    if bmi <= 0:  # bmi should not be negative.
        print('Warning: bmi should not be negative.')  # Let the user know this and leave them to deal with the
        # negative BMI.
        return 'underweight'  # Returning underweight for negative BMI.
    elif bmi < 18.5:  # A non-negative, but underweight BMI.
        return 'underweight'  # Let the user know that the BMI represents an underweight person.
    elif bmi <= 25:  # A BMI that is at least 18.5 and at most 25 is normal (optimal).
        return 'optimal'  # Send this information to the user.
    else:  # BMI bigger than 25.
        return 'overweight'  # Return the status of overweight to the user.


h, w = get_input()  # Accept the input for height and weight.
bmi = calculate_BMI(h, w)  # Calculate the bmi for the given height and weight.
cat = calculate_weight_category(bmi)  # Determine the weight category for the BMI.
print('For a height of %d and a weight of %d, the BMI is %d, which indicates that their weight category is %s' % (
h, w, bmi, cat))  # Notify the user of all this information.
