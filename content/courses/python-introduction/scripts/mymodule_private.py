class MyClass:
    """This is my class"""
    _i=12345
    #init is surrounded by double underscores
    def __init__(self,x,y):
        self.x=x
        self.y=y

    def reset(self,x,y):
        self.x=x
        self.y=y

    def __redo(self):  #double underscore
        self.x=10.
        self.y=20.

    def addit(self,z):
        return MyClass._i+self.y-z

