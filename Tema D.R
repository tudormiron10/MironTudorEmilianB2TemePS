#I
MC_M = function(x, k) {
  i = 0
  while (i < k) {
    numar = sample(x, 1)
    cnt = 0
    j = 1
    while (j <= length(x)) {
      if (x[j] == numar) {
        cnt = cnt + 1
      }
      j = j + 1
    }
    if (cnt >= length(x) / 2 + 1) {
      return (numar)
    }
    i = i + 1
  }
  return ("x nu are element")
}
x = sample(1:2, 100, replace = TRUE)
MC_M_element(x, 24)

#II
element_A = function(i, A) {
  z = sample(A, 1)
  Alt = A[A < z]
  Agt = A[A > z]
  if (length(Alt) > i) {
    return (element_A(i, Alt))
  }
  else {
    if (length(A) > i + length(Agt)) {
      return (z);
    }
    else {
      return (element_A(i - length(A) + length(Agt), Agt))
    }
  }
}
element_A(54, x)
sort(x)[55]
element_A(55, x)
sort(x)[56]

#III
Mediana = function(S, a) {
  if (a < 0) {
    a = -a
  }
  m = floor(a * log(length(S)))
  SS = sample(S, m)
  sorted_SS = sort(SS)
  index = ceiling(m / 2)
  return (sorted_SS[index])
}

x = runif(4473, 0, 100)
median(x)
Mediana(x, 532)