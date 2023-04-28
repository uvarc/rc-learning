fout=open("data.txt","w")

for n in range(1,21):
    fout.write(str(n)+","+str(n**2)+","+str(n**3)+"\n")

# Must close to reopen from top
fout.close()

fin=open("data.txt","r")

n=[]
n2=[]
n3=[]

for line in fin:
    data=line.strip('\r\n').split(",")
    n.append(float(data[0]))
    n2.append(float(data[1]))
    n3.append(float(data[2]))

a=1.0; b=2.4; c=5.8; d=0.7

for i in range(len(n)):
    print(f"{n[i]:.0f},{a+b*n[i]+c*n2[i]+d*n3[i]:.2f}")
