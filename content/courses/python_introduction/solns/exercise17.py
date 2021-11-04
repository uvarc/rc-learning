fin=open("points.csv")
lines=[line.strip("\r\n") for line in fin.readlines()]
del lines[0]

x=[]
y=[]
for line in lines:
    data=list(map(float,line.split(',')))
    x.append(float(data[0]))
    y.append(float(data[1]))

print(x[3],y[3])

