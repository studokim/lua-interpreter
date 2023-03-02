function f()
    function g()
        return 5
    end
    return g
end

x = f()
x()
__show_env()
