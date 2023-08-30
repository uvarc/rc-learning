c_temps = [0, 10, 20, 30, 40, 50]

print("Number of items:",len(c_temps))

print("Index of '30':",c_temps.index(30))

c_temps.append(60)

for c in c_temps:
    f = c*1.8 + 32
    print (c,f)
