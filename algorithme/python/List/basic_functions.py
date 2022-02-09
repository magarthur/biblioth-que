def appartient(e,L):
    for i in range(len(L)):
        if e==L[i]:
            return true
    return false


  
  
  def max1(L):
    m = L[0]
    for i in range(1,len(L)):
        if m < L[i]:
            m=L[i]
    return m
  
  
  def max2(L):
    if L[0]>L[1]:
        m,m1 = L[0],L[1]
    else:
        m,m1 = L[1],L[0]
    for i in range (2,len(L)):
        if L[i]>= m:
            m,m1 = L[i],m
        elif L[i]<m & L[i]>m1:
            m1 = L[i]
    return m,m1
