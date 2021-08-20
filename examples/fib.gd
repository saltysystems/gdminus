func _ready():
    var start = time()
    print(fib(25))
    var end = time()
    print("Time: " + str(end - start) + "ms")



func fib(n):
    if n < 2:
        return 1
    else:
        return fib(n-1) + fib(n-2)

func time():
    return OS.get_ticks_msec()
