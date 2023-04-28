class PrivateStuff:
      def __init__(self,x):
          self.__u=11.
          self.x=x
      def set_u(self,u):
          self.__u=u
      def get_u(self):
          return self.__u
secret=PrivateStuff(9.)
u1=secret.get_u()
print(u1)
secret.set_u(12.)
u2=secret.get_u()
print(u2)
