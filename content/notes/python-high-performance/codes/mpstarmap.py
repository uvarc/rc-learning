from multiprocessing import Pool

def f(x,y,z):
   return x**y+z

if __name__ == '__main__':
   pool = Pool(processes=4)

   x=list(range(1,11))
   y=[2,1,3,4,2,3,1,0,3,2]
   z=list(range(50,61))
   fargs=zip(x,y,z)

   result = pool.starmap(f, fargs)

   # Print result
   print(result)
   pool.close()
   pool.join()
