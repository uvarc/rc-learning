unit=input("Enter 1 for Imperial or 2 for metric units:")

if unit=="1":
    weight=float(input("Enter your weight in pounds:"))
    height=float(input("Enter your height in inches:"))
    BMI=weight*703.1/height**2
elif unit=="2":
    weight=float(input("Enter your weight in kg:"))
    height=float(input("Enter your height in m:"))
    BMI=weight/height**2
else:
    print("Invalid unit request")
    BMI=None

print("Your BMI is ",BMI)
