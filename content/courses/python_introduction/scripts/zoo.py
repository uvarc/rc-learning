class Animal:
    def __init__(self, name, species):
        self.name=name
        self.species=species
        
    def speak(self):
        raise NotImplementedError("Subclasses must implement this")
  
class Feline(Animal):
    def speak(self):
        return "Roar"

class Canine(Animal):
    def speak(self):
        return "Howl"

class Bird(Animal):
    def speak(self):
        return "Squawk"
  
zoo=[]
zoo.append(Feline("Raja","lion"))
zoo.append(Canine("Sasha","wolf"))
zoo.append(Bird("Polly","parrot"))

for critter in zoo:
    print(critter.name+" says "+critter.speak())
