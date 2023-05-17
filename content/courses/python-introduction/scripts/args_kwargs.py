def varargs(var1,var2,*args,**kwargs):
    print(f"Positional argument 1 {var1}")
    print(f"Positional argument 2 {var2}")

    print(f"Args are a {type(args)}")
    for arg in args:
        print(f"Variable  argument {arg}")

    print(f"Kwargs are a {type(kwargs)}")
    for kwarg in kwargs:
        print(f"Keyword:Variable {kwargs[kwarg],kwarg}")

varargs(1, 2, 3, 4, 5, 6, 7, a=8, b=9, c=10)



