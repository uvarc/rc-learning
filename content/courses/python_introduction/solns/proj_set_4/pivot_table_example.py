import pandas as pd


df1 = pd.DataFrame({'ID #': [1,2,3,4,5,6,7,8,9,10],
                    'Name': ['John','Dave','Mary','Sarah','Mohammed','Rohan','Prisha','Vijay','Ananya','Raj'],
                    'Country': ['USA','USA','USA','UK','India','India','UK','India','UK','India']})

df2 = pd.DataFrame({'ID #': [1,2,3,4,5,6,7,8,9,10],
                    'Salary': [50000, 60000, 65000, 53000, 59000, 74000, 86000, 41000, 94000, 66000],
                    'Age': [24, 46, 51, 29, 33, 38, 70, 46, 49, 35]})


#merge
result = pd.merge(df1, df2, on="ID #")

#see counts of people by country
#print(result['Country'].value_counts())

#pivot table reshape
result2 = pd.pivot_table(result, index=['Country', 'Name', 'Age', 'Salary'])
print(result2)
