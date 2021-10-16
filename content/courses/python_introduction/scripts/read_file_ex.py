fin=open("points.csv")
x=[]
y=[]
fin.readline() #skip header
for line in fin:
    data=line.rstrip("\r\n").split(",")
    x.append(float(data[0]))
    y.append(float(data[1]))

print(x[3],y[3])

