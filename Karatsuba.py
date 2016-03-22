from math import *

def karatsuba(x,y):
    B = 10
    
    # Recursion
    if x < 10 or y < 10:
        return x*y    
    

    m = max(int(log10(x)+1), int(log10(y)+1))
    
    if m % 2 != 0:
        m -= 1
    m_2 = int(m/2)
    
    a, b = divmod(x, B**m_2)
    c, d = divmod(y, B**m_2)
    
    ac = karatsuba(a,c)
    bd = karatsuba(b,d)
    ad_bc = karatsuba((a+b),(c+d)) - ac - bd
    
    return ((ac*(10**m)) + bd + ((ad_bc)*(10**m_2)))
