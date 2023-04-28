capitals={"Alabama":"Montgomery"}
capitals["Alaska"]="Juneau"
capitals["Arizona"]="Little Rock"
print(list(capitals.keys()))
print("Virginia" in capitals)
print("Arkansas" in capitals)
newstate="Connecticut"
newcapital="Hartford"
if newstate not in capitals:
    capitals[newstate]=newcapital
for key in capitals:
    print("The capital of",key,"is",capitals[key])
