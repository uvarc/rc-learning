#import pandas
import pandas as pd

#create new dataframe
basketball_coaches = pd.DataFrame({"Name": ['Tony Bennett', 'Roy Williams', 'Mike Krzyzewski', 'Tom Izzo', 'Jim Boeheim'],
                          "School": ['Virginia', 'North Carolina', 'Duke', 'Michigan State', 'Syracuse'],
                          "Email": ['tbennett@virginia.edu', 'rwilliams@unc.edu', 'coachk@duke.edu','tizzo@msu.edu', 'jboeheim@syracuse.edu'],
                          "Career Wins": [346, 871, 1132, 606, 944],
                          "National Championships": [1, 3, 5, 2, 1]})


print(basketball_coaches)


#rename columns with a variable name
name = basketball_coaches['Name']
school = basketball_coaches['School']
email = basketball_coaches['Email']
career_wins = basketball_coaches['Career Wins']
championships = basketball_coaches['National Championships']

#make new subset of data
wins_per_championship = career_wins/championships

#or if you want it to be a part of the dataframe
basketball_coaches['Wins Per Championship'] = basketball_coaches['Career Wins'] / basketball_coaches['National Championships']



#print a list of all the columns
print(basketball_coaches.columns.tolist())



#split a subset of the dataframe based on some condition. Several examples listed here
uva_coach = basketball_coaches.loc[basketball_coaches['School'] == 'Virginia']

multiple_championships = basketball_coaches.loc[basketball_coaches['National Championships'] > 1]

first_three_coaches = basketball_coaches.iloc[:3]

