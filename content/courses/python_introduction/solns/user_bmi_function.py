def BMI(unit,weight,height):
    if unit=="1":
        BMI=weight*703.1/height**2
    elif unit=="2":
        BMI=weight/height**2
    else:
        print("Invalid unit request")
        BMI=None
    return BMI

unit=input("Enter 1 for Imperial or 2 for metric units:")
weight=float(input("Enter your weight in pounds or kg:"))
height=float(input("Enter your height in inches or m:"))
print(f"Your BMI is {BMI(unit,weight,height):.2f}")
