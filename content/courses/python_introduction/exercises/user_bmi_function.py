def BMI(unit,weight,height):
    if unit=="1":
        BMI=weight*703.1/height**2
    elif unit=="2":
        BMI=weight/height**2
    else:
        print("Invalid unit request")
        BMI=None
    return BMI

def BMI_table(bmi_val):
   if         bmi_val < 18.5 : return 0
   if 18.5 <= bmi_val < 25.0 : return 1
   if 25.0 <= bmi_val < 30.0 : return 2
   if 30.0 <= bmi_val < 35.0 : return 3
   if 35.0 <= bmi_val < 40.0 : return 4
   if 40.0 <= bmi_val < 45.0 : return 5
   if         bmi_val > 45.0 : return 6


category=["Underweight","Normal","Overweight",
          "Obese Class I","Obese Class II","Obese Class III","Obese Class IV"]

unit=input("Enter 1 for Imperial or 2 for metric units:")
weight=float(input("Enter your weight in pounds or kg:"))
height=float(input("Enter your height in inches or m:"))
bmi_value=BMI(unit,weight,height)
BMI_index=BMI_table(bmi_value)

print(f"Your BMI is {bmi_value:.2f}")
print("You are {}".format(category[BMI_index]))
