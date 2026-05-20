import math

a = 5 # Variable assignment
b1 = 4 ** 2 # Exponent
c = 5 * 10 # Multiplication
d = 12 / 3 # Division
e = 3 + 4 # Addition
f = 7 - 2 # Subtraction
g = math.sqrt(9) # Square Root
h = 6 / 2 * (1 + 2) ** 2 # PEMDAS Example
l = [1,2,3,4] # List


print(b1)
print(c)
print(d)
print(e)
print(f)
print(g)
print(h)
print(len(l))

# For loop
for i in range(4):
    print(i)

# While loop
j = 4
while j > 0:
    j -= 1
    print(j)

# Return false if number is 0 or negative, true if the number is not 0 and greater than 0
def try_out(number):
    if number == 0 or number < 0:
        return False
    elif number != 0 and number > 0:
        return True

#Return true if the number is between 3 and 2, false otherwise
def try_out1(number):
    if number <= 3 and number >= 2:
        return True
    else:
        return False
    
print(try_out(0))
print(try_out(-1))
print(try_out(1))
print(try_out1(2))
print(try_out1(4))