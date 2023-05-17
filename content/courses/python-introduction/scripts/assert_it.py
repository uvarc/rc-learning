def avg(values):
    assert len(values)!=0,"Length of input to avg cannot be 0"
    return sum(values)/len(values)

values_1=[11.,9.,3.,8.,7.]
print(avg(values_1))

values_2=[]
print(avg(values_2))
