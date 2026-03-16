from multiprocessing import Pool

def f(x):
  return x*x

def f2(x,y):
    return x**y

if __name__ == '__main__':
   nprocs=4
   pool = Pool(processes=nprocs)
   result = pool.map(f, range(1,10))
   # Do other stuff

   # Now check for result
   print(result)

   result=pool.starmap(f2,[(x,3) for x in range(1,11)])
   print(result)
   
   results=[]
   for x in range(1,11):
       results.append(pool.apply_async(f2,(x,3)))
   allresults=[result.get() for result in results]
   print(allresults)

   pool.close()
   pool.join()
