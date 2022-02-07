"""
   Dictionary practice
   Author:  K. Holcomb
"""

fin=open("us-state-capitals.csv")

states={}
for line in fin:
    state_cap=line.strip("\r\n").split(",")
    if state_cap[0] not in states:
        states[state_cap[0]]=state_cap[1]

fin.close()

for statelist in ["Arkansas","Virginia","Wyoming"]:
    if statelist in states:
        print("The capital of {:} is {:}".format(statelist,states[statelist]))

caps_in_M=[]
for state in states:
    if states[state].startswith("A"):
        caps_in_A.append(states[state])

#Just for fun, let's alphabetize them.
caps_in_A.sort()

caps_string=";".join(caps_in_A)

#Checking
print(caps_string)

fout=open("capitals-with-a.txt","w")
fout.write(caps_string+"\n")
    
