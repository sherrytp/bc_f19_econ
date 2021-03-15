balance = 270472
annualInterestRate = 0.21
# test case 1
# balance = 320000
# annualInterestRate = 0.2
# Lowest Payment: 29157.09
# test case 2
# balance = 999999
# annualInterestRate = 0.18
# Lowest Payment: 90325.03

epsilon = 0.01
lower = balance / 12
upper = balance * ((1 + annualInterestRate / 12.0) ** 12) / 12.0
ans = (lower + upper) / 2.0

def yearEndBalance(monthlyPayment):
    '''
    Calculates year end balance given a monthly payment
    as an argument. monthlyPayment can be int or float '''
    myBalance = balance
    for m in range(12):
        interest = (myBalance - monthlyPayment) * annualInterestRate / 12.0
        myBalance = myBalance + interest - monthlyPayment
    return myBalance

while abs(yearEndBalance(ans)) >= epsilon:
    # print("lower = " + str(lower) + " upper = " + str(upper) + " ans = " + str(ans))
    if yearEndBalance(ans) < 0:
        upper = ans
    else:
        lower = ans
    ans = (lower + upper) / 2.0
    
print ("Lowest Payment: " + str(round(ans, 2)))

