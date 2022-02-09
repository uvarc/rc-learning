class Point:
    def __init__(self,x,y,z):
        self.x=x
        self.y=y
        self.z=z

    def __add__(self,p2):
        return Point(self.x+p2.x,self.y+p2.y,self.z+p2.z)

    def __str__(self):
        return "("+str(self.x)+","+str(self.y)+","+str(self.z)+")"

p1=Point(1,2,3)
p2=Point(7,8,9)

p3=p1+p2
print(p3)
