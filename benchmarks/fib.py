import time

def fib(n):
    if n <= 1:
        return n
    else:
        return fib(n-1) + fib(n-2)

start = time.time()
print(fib(35))
print(time.time() - start)
