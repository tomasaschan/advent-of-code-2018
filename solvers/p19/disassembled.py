import sys

part_b = sys.argv[1:] and sys.argv[1] == "b"

a, b, c, e, f = 0, 0, 0, 0, 0

# init sequence
# 1: goto 17

# 17-20
f = 2 * 2 * 11 * 19 # 836

# 21-23
c = 5 * 22 + 21 # 131

# 24
f = f + c # 967

# 25-35
if part_b:
    c = (27 * 28 + 29) * 30 * 14 * 32
    f = f + c

print("Before starting:",a,b,c,e,f)


# MAIN LOOP

# 2-15
b = 1
while b <= f:
    # 3-11

    if f % b == 0:
        a += b

    # e = 1
    # while c <= f:
    #     c = b * e
    #     if c == f:
    #         a = b + a
    #     else:
    #         e += 1

    b += 1
print(a)
