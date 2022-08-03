c_temps = [0, 10, 20, 30, 40, 50]

print (f"Number of items: {len(c_temps)}.")

print (f"Index of '30': {c_temps.index(30)}.")

c_temps.append(60)

for t in c_temps:
    f = t*1.8 + 32
    print (f"Fahrenheit: {f:.2f}")
