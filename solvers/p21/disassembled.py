import sys

a, c, d, e, f = 0, 0, 0, 0, 0

# 0
f = 123


# 1 - 4
while f != 72:
    f = f & 456

# 5
f = 0

# 6-7
e = f | 65536
f = 13284195
i = 0
while True:
    i += 1

    if i % 1000000 == 0:
        print(i)

    # 8-12
    d = e & 255
    f = (((f + d) & 15777215) * 65899) & 15777215

    # 13-16
    if 256 > e:
        # 28-30
        break
        if a == f:
            break
        else:
            # goto 6
            e = f | 65536
            f = 13284195
            continue

    # 17
    d = 0

    e = e // 256

    # while True:
    #     # 18-25
    #     if (d + 1) * 256 > e:
    #         break
    #     else:
    #         d += 1

    # 26-27
    # e = d

print(i, a,c,d,e,f)
