function factorial(n)
    if (n == 1) then
        return 1
    else
        return (n * factorial((n - 1)))
    end
end

x = factorial(165)
y = factorial(16)
t = (y == 20922789888000)
__show_env()
