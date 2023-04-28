class Point:
    def __init__(self,x,y,z):
        self.x=x
        self.y=y
        self.z=z

    def __add__(self,p2):
        return Point(self.x+p2.x,self.y+p2.y,self.z+p2.z)

    def __sub__(self,p2):
        return Point(self.x-p2.x,self.y-p2.y,self.z-p2.z)

p1=Point(1,2,3)
p2=Point(7,8,9)

p3=p1+p2
print(p3.x,p3.y,p3.z)

p4=p1-p2
print(p4.x,p4.y,p4.z)
