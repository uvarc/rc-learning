c_temps = []
f_temps = []
for t in range(0,70,10):
    c_temps.append(t)
    f = t * 1.8 + 32
    f_temps.append(f)
for index in range(len(c_temps)):
    print (f"Celsius: {c_temps[index]:.1f}, Fahrenheit: {f_temps[index]:.1f}")
