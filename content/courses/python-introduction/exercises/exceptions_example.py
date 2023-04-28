x=[]
y=[]
with open("numbers.csv"):
    fin=open("numbers.csv")
    fin.readline() #skip header
    for line in fin:
        data=line.rstrip("\r\n").split(",")
        try:
            x.append(float(data[0]))
            y.append(float(data[1]))
        except:
            print("Error in line:",line)

print(x[3],y[3])

