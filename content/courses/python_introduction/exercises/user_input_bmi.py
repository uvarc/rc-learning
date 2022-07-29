unit=int(float(input("Enter 1 for Imperial or 2 for metric units:")))

if unit==1:
    weight=float(input("Enter your weight in pounds:"))
    height=float(input("Enter your height in inches:"))
    BMI=weight*703.1/height**2
elif unit==2:
    weight=float(input("Enter your weight in kg:"))
    height=float(input("Enter your height in m:"))
    BMI=weight/height**2
else:
    print("Invalid unit request")
    BMI=None

if         BMI < 18.5 : category="Underweight"
if 18.5 <= BMI < 25.0 : category="Normal"
if 25.0 <= BMI < 30.0 : category="Overweight"
if 30.0 <= BMI < 35.0 : category="Obese Class I"
if 35.0 <= BMI < 40.0 : category="Obese Class II"
if 40.0 <= BMI < 45.0 : category="Obese Class III"
if         BMI>= 45.0 : category="Obese Class IV"

print("Your BMI is {:.2f} and you are {}".format(BMI,category))
