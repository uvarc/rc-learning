c_temps = []
f_temps = []
for t in range(-40,105,5):
    c_temps.append(t)
    f = t * 1.8 + 32
    f_temps.append(f)

filtered = []
for index in range(len(c_temps)):
    print (c_temps[index], f_temps[index])
    if c_temps[index]<0 and f_temps[index]>0:
        filtered.append(f_temps[index])

print (f"Filtered: {filtered}")
