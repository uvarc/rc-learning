inds=["Jim Dandy","Betty Boop","Minnie Moocher","Joe Friday","Teddy Salad"]
grades={"Test1":[85.4,91.7,73.2,82.3,98.5],
        "Test2":[88.1,89.8,75.9,84.0,96.3],
        "Test3":[83.7,92.4,70.1,88.2,96.8],
        "Test4":[84.1,87.2,69.3,81.7,93.9]}

grade_record=pd.DataFrame(grades,columns=["Test1","Test2","Test3","Test4"],index=inds)

grade_record.sort_values(by='Jim Dandy',axis=1)

