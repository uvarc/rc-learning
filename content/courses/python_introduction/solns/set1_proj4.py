n_str = input("Please enter integer number N > 0: ")
try:
    N = int(n_str)
    if N >0:
        b_sum = 0
        for number in range(1,N+1):
            b_sum = b_sum+number
        print (f"Sum (brute force): {b_sum}, sum (Gaussian method): {N*(N+1)//2}.")
    else:
        print ("Please enter an integer number greater than 0.")
except:
    print (f"The entered value {n_str} cannot be converted to an integer number")
