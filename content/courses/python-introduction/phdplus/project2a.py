with open("rome_rain.dat") as f:
    f.readline()  #Throw away header
    data=f.readlines()

rainfall={}
for line in data:
    data_list=line.split(";")
    if data_list[0] not in rainfall:
        rainfall[data_list[0]]=float(data_list[1])

max_precip=max(rainfall.values())
min_precip=min(rainfall.values())

yearly_total=0.
for key in rainfall:
    if rainfall[key]==max_precip:
        print("The maximum precipitation is in ",key)
    if rainfall[key]==min_precip:
        print("The minimum precipitation is in ",key)
    yearly_total+=rainfall[key]

print("The annual total precipitation is ",yearly_total)
