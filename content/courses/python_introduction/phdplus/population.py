#!/usr/bin/env python
# coding: utf-8

with open("world_population.csv") as f:
    f.readline()  #Throw away header
    data=f.readlines()

countries=[]
population=[]

for line in data:
    data_list=line.split(",")
    countries.append(data_list[2])
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

fout=open("high_pop_countries.csv","w")

print(f'{"Country":^13} {"Population":^16}')
for i in range(len(high_pop)):
    country=high_pop[i]
    pop=world_pop[high_pop[i]]
    print(f"{country:<13} {pop:>13}")
    fout.write(country+","+str(pop)+"\n")
