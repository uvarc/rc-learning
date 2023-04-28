weight=155.0
height=74.0

#metric
#BMI=weight/height**2

#imperial
BMI=weight*703.1/height**2

if         BMI < 18.5 : category="Underweight"
if 18.5 <= BMI < 25.0 : category="Normal"
if 25.0 <= BMI < 30.0 : category="Overweight"
if 30.0 <= BMI < 35.0 : category="Obese Class I"
if 35.0 <= BMI < 40.0 : category="Obese Class II"
if 40.0 <= BMI < 45.0 : category="Obese Class III"
if         BMI>= 45.0 : category="Obese Class IV"

print("BMI:",BMI," which is ",category)
