from sympy import linsolve, Matrix, sympify
from sympy.abc import a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p

variables = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]
machines = []

with open("input.txt", "r") as file:
    for line in file:
        rest = line.split("] ")[1]
        joltage = rest.split("{")[1].split("}")[0]
        buttons = rest.split(" {")[0].split(" ")
        joltage = list(map(lambda x: int(x), joltage.split(",")))
        buttons = list(map(lambda x: list(map(lambda x: int(x), x.replace("(", "").replace(")", "").split(","))), buttons))
        machines.append((buttons, joltage))

results = []
deg3 = []

for (buttons, joltage) in machines:
    varsize = len(buttons)
    arrlen = len(joltage)
    vars = variables[0 : varsize]
    mt = []
    for button in buttons:
        arr = [0] * arrlen
        for bu in button:
            arr[bu] = 1
        mt.append(arr)
    MT = Matrix(mt)
    M = MT.T
    result = linsolve((M, joltage), vars)

    syms = result.free_symbols
    if len(syms) == 0:
        results.append(sum(list(result)[0]))
    else:
        maxJoltage = max(joltage)
        if len(syms) == 1:
            minval = 10000000
            for x in range(0, maxJoltage + 1):
                res = result.subs(list(syms)[0], x)
                val = sum(list(res)[0])
                if val < minval and all(z >= 0 and z % 1 == 0 for z in list(res)[0]):
                    minval = val
            print("Løsning:", minval)
            results.append(minval)
        elif len(syms) == 2:
            minval = 10000000
            symlist = list(syms)
            for x in range(0, maxJoltage + 1):
                for y in range (0, maxJoltage + 1):
                    res = result.subs([(symlist[0], x), (symlist[1], y)])
                    val = sum(list(res)[0])
                    if val < minval and all(z >= 0 and z % 1 == 0 for z in list(res)[0]):
                        minval = val
            print("Løsning:", minval)
            results.append(minval)
        elif len(syms) == 3:
            deg3.append((result, joltage))

print("Results without 3 free variables", sum(results))

# replace with your own deg 3 equations
# print(deg3)
eq3vars = [
    (
        {(21 - 2*l, -k - l + 28, k + l - m - 14, -k - 2*l + 38, 21 - k, k + l - m - 15, 8, -l - m + 23, 2*l + m - 15, 2*l + m - 2, k, l, m)}, 
        [29, 77, 54, 44, 63, 72, 44, 37, 84, 49],
        {k: 28, l: 23,  m: 23}
    ), 
    (
        {(l - m + 15, -k - l + 17, 2*l - m + 19, l + 6, 27 - 2*l, k + 4*l - 27, -3*l + m + 8, l + 2, -2*l + m + 1, -k + 2*l - m + 25, k, l, m)}, 
        [70, 85, 46, 29, 50, 35, 103, 69, 25, 22],
        {k: 17, l: 17, m: 103}
    ), 
    (
        {(-2*k + l + 2*m - 10, k + m + 99, -k + m + 6, -k - 2*l - 3*m + 65, k - l - m + 23, k - 2*l - 2*m + 59, l + 3*m - 24, -k + l - m + 3, k - 2*l - 3*m + 61, -k + 4*l + 4*m - 85, k, l, m)}, 
        [167, 207, 67, 89, 174, 32, 150, 167, 166, 169],
        {k: 65, l: 33, m: 22}
    ), 
    (
        {(-k - 4*l - m + 120, l + 1, k/2 + l + m - 40, -2*k - 2*l - m + 111, -k/2 - l - m + 51, 3*k/2 + 2*l + m - 76, k/2 + l - m + 6, -k/2 + m - 5, -3*l - m + 82, l - 2, k, l, m)}, 
        [75, 76, 84, 94, 78, 111, 115, 77, 46, 79],
        {k: 56, l: 28, m: 82}
    ), 
    (
        {(f + g - 12, -g + 2*h - 15, g - h + 21, -f - 2*h + 189, -f - g - h + 42, f, g, h)}, 
        [204, 36, 42, 30, 195, 195],
        {f: 42, g: 42, h: 42}
    ), 
    (
        {(-g + h - 6, i - 3, g - h - i + 17, -g + h - 1, 32 - h, 9, g, h, i)}, 
        [39, 52, 11, 40, 41, 8, 35],
        {g: 41, h: 32, i: 41}

    ), 
    (
        {(l + m, -j - l - m + 30, -2*l - 3*m + 21, l + 2*m - 1, l + m + 8, -2*l - 3*m + 25, l + 3*m - 2, -j + l + 27, -2*l - 2*m + 14, j, l + 7, l, m)}, 
        [70, 49, 40, 51, 45, 46, 81, 42, 49, 67],
        { l: 25, m: 25, j: 81}
    )
]

eq3res = []

for (eq, joltage, symToLimit) in eq3vars:
    result = sympify(eq)
    symlist = list(result.free_symbols)
    minval = 10000000
    for x in range(0, symToLimit[symlist[0]]):
        for y in range (0, symToLimit[symlist[1]]):
            for z in range (0, symToLimit[symlist[2]]):
                res = result.subs([(symlist[0], x), (symlist[1], y), (symlist[2], z)])
                val = sum(list(res)[0])
                if val < minval and all(w >= 0 and w % 1 == 0 for w in list(res)[0]):
                    minval = val
                    minRes = res
    print("Løsning:", minval)
    eq3res.append(minval)


print("Results for equtions with three variables:", sum(eq3res))
print("task 2:", sum(results) + sum(eq3res))