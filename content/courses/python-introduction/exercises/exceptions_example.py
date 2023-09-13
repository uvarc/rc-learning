x=[]
y=[]
with open("numbers.csv") as fin:
    for line in fin:
        data=line.rstrip("\r\n").split(",")
        try:
            x.append(float(data[0]))
            y.append(float(data[1]))
        except:
            print("Can't convert data in line:",line)

print(x[3],y[3])

