with open("world_population.csv") as f:
    f.readline()  #Throw away header
    data=f.readlines()

countries=[]
capitals=[]
population=[]

for line in data:
    data_list=line.split(",")
    countries.append(data_list[2])
    capitals.append(data_list[3])
    population.append(data_list[5])

world_pop={}
for i,country in enumerate(countries):
    if country not in world_pop:
        world_pop[country]=int(population[i])

print(world_pop["Norway"])

high_pop=[]
for country in world_pop:
    if world_pop[country]>100000000:
        high_pop.append(country)

print(f'{"Country":^13} {"2022 Population":^16}')

for i in range(len(high_pop)):
    print(f"{high_pop[i]:<13} {world_pop[high_pop[i]]:>13}")

capitals_with_b=[]
for i in range(len(countries)):
    if capitals[i].startswith("B"):
        capitals_with_b.append([countries[i].upper(),capitals[i]])

fout=open("capitals_with_B.csv","w")
fout.write("Country,Capital\n")

for i in range(len(capitals_with_b)):
    fout.write(capitals_with_b[i][0]+","+capitals_with_b[i][1]+"\n")









