balance = 320000
annualInterestRate = 0.2

# function that calculates the balance at year end given a monthly payment
def yearEndBalance(monthlyPayment):
    '''
    Calculates year end balance given a monthly payment
    as an argument. monthlyPayment can be int or float '''
    myBalance = balance
    for m in range(12):
        interest = (myBalance - monthlyPayment) * annualInterestRate / 12.0
        myBalance = myBalance + interest - monthlyPayment
    return myBalance 

guess = 10

while yearEndBalance(guess) >= 0:
    guess += 10
    
print("Lowest payment: " + str(guess))
