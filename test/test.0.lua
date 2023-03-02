function twice(x)
    y = (x * 2)
    return y
end

function square(a)
    return (a * a)
end

a = 5
title_1 = "square area is"
area = square(a)
title_2 = "and its perimeter is"
perimeter = twice(twice(a))
__show_env()
