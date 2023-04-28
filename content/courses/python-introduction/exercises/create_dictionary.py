cities=["Boston","Brooklyn","New York","Philadelphia","Toronto",
        "San Francisco","Los Angeles","Los Angeles","Phoenix",
        "Sacramento","Chicago","Cleveland","Detroit","Indiana",
        "Milwaukee","Dallas","Houston","Memphis","New Orleans",
        "San Antonio","Atlanta","Charlotte","Miami","Orlando",
        "Washington","Denver","Minnesota","Oklahoma City","Portland"
        "Salt Lake City"]
mascots=["Celtics","Nets","Knicks","76ers","Raptors","Golden State Warriors",
         "Clippers","Lakers","Suns","Kings","Bulls","Cavaliers","Pistons",
         "Pacers","Bucks","Mavericks","Rockets","Grizzlies","Hornets","Spurs",
         "Hawks","Bobcats","Heat","Magic","Wizards","Nuggets","Timberwolves",
         "Thunder","Trail Blazers","Jazz"]

teams={}

for i,city in enumerate(cities):
    if city not in teams:
        teams[city]=[mascots[i]]
    else:
        teams[city].append(mascots[i])

print("The NBA team in Chicago is the","".join(teams["Chicago"]))
