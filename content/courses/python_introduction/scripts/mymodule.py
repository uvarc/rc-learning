class MyClass:
    """This is my class"""
    i=12345
    #init is surrounded by double underscores
    def __init__(self,x,y):
        self.x=x
        self.y=y

    def reset(self,x,y):
        self.x=x
        self.y=y

    def addit(self,z):
        return MyClass.i+self.y-z

