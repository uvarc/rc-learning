from multiprocessing import Process

def f(name):
    print('hello from '+name)

if __name__ == '__main__':
    ncpus=4
    for i in range(ncpus):
        p=Process(target=f,args=(str(i),))
        p.start()

