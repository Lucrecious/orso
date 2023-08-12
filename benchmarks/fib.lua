local clock = os.clock

function fib(n)
    if n <= 1 then
        return n
    else
        return fib(n - 1) + fib(n - 2)
    end
end

local start = clock()
print(fib(35))
print(clock() - start)
