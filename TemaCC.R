#C1

# Fct pt generare permutari aleatoare
generare_permutare_aleatoare = function(n) 
{
  valori_aleatoare = runif(n)
  indice_sortat = order(valori_aleatoare)
  return(indice_sortat) # permutarea ordonata dupa indice
}

# Fct pt compararea lexicografica a doua siruri de biti
compara_lexicografic = function(w1, w2)
{
  L = min(length(w1), length(w2))
  
  for (l in 1:L) 
  {
    if (w1[l] < w2[l]) 
    {
      return(-1)  # w1 este < lexicografic
    } else if (w1[l] > w2[l])
    {
      return(1)   # w1 este > lexicografic
    }
  }
  
  if (length(w1) < length(w2)) 
  {
    return(-1)  # w1 este < lexicografic
  } else if (length(w1) > length(w2))
  {
    return(1)   # w1 este > lexicografic
  }
  
  return(0)     # w1 si w2 sunt = lexicografic
}

# Ex pt generare de perm
n = 10
permutare_aleatoare = generare_permutare_aleatoare(n)
print("Permutarea aleatoare generata:")
print(permutare_aleatoare)

# Ex pt compararea lexicografica 
w1 = c(1, 0, 1, 1, 0)
w2 = c(1, 1, 0, 0, 1)
rezultat_comparare = compara_lexicografic(w1, w2)
if (rezultat_comparare == -1)
{
  print("w1 este lexicografic < decat w2.")
} else if (rezultat_comparare == 1) 
{
  print("w1 este lexicografic > decat w2.")
} else
{
  print("w1 si w2 sunt = lexicografic.")
}

# c) Functie pentru QuickSort randomizat pentru ordonarea stricta a unei liste de cuvinte (0/1) de aceeasi lungime k
quicksort_randomizat_cuvinte = function(W)
{
  n = length(W)
  if (n <= 1) {
    return(W)
  } else {
    pivot_index = sample(1:n, 1)  # aleg un pivot aleator
    pivot = W[pivot_index]
    
    cuvinte_mici = cuvinte_egale = cuvinte_mari = vector("list", length = n)
    idx_mici = idx_egale = idx_mari = 1
    
    for (cuvant in W) 
    {
      comparatie = compara_lexicografic(cuvant, pivot)
      if (comparatie == -1) {
        cuvinte_mici[[idx_mici]] = cuvant
        idx_mici = idx_mici + 1
      } else if (comparatie == 0) {
        cuvinte_egale[[idx_egale]] = cuvant
        idx_egale = idx_egale + 1
      } else {
        cuvinte_mari[[idx_mari]] = cuvant
        idx_mari = idx_mari + 1
      }
    }
    
    cuvinte_mici = unlist(cuvinte_mici[1:(idx_mici - 1)])
    cuvinte_egale = unlist(cuvinte_egale[1:(idx_egale - 1)])
    cuvinte_mari = unlist(cuvinte_mari[1:(idx_mari - 1)])
    
    return(c(quicksort_randomizat_cuvinte(cuvinte_mici), cuvinte_egale, quicksort_randomizat_cuvinte(cuvinte_mari)))
  }
}

# Exemplu de utilizare:
n = 10
k = 5
cuvinte = matrix(sample(0:1, n * k, replace = TRUE), ncol = k)
cuvinte_sortate = quicksort_randomizat_cuvinte(cuvinte)
print("Lista de cuvinte sortata utilizand QuickSort randomizat:")
print(cuvinte_sortate)

#d) Fct pt generare permutari aleatoare folosind quicksort aleator
generare_permutare_aleatoare_quicksort = function(n, k) {
  cuvinte = replicate(n, sample(0:1, k, replace = TRUE))
  return(quicksort_randomizat_cuvinte(cuvinte))
}

# Exemplu de utilizare:
n = 10
k = 5
permutare_aleatoare = generare_permutare_aleatoare_quicksort(n, k)
print("Permutarea aleatoare generata folosind QuickSort aleator:")
print(permutare_aleatoare)

#C2

# Definirea nodurilor (V) si a muchiilor (E) pentru un graf bipartit
V = c(1, 2, 3, 4, 5, 6)  # Exemplu de noduri
E = list(c(1, 2), c(1, 5), c(1, 6), c(2, 4), c(2, 6))  # Exemplu de muchii

# a) Algoritmul aleator pentru determinarea unei taieturi de cardinal maxim intr-un graf bipartit
taietura_maxima_bipartita = function(V, E) {
  n = length(V) / 2
  A = sample(V, n)  # aleg n noduri aleator din V
  B = setdiff(V, A)  # restul nodurilor sunt din B
  
  taietura_maxima = sum(sapply(E, function(muchie) {
    (muchie[1] %in% A && muchie[2] %in% B) || (muchie[1] %in% B && muchie[2] %in% A)
  }))  # numar muchiile dintre A si B
  
  return(taietura_maxima)
}

# b) Modalitate de a creste sansele de a gasi o taietura de cardinal maxim

# rularea algoritmului de mai multe ori si selectarea celei mai bune solutii
nr_rulari = 100  # nr rulari
taieturi_maxime = replicate(nr_rulari, taietura_maxima_bipartita(V, E))
taietura_maxima = max(taieturi_maxime)

# exemplu pentru a)
print("Cardinalul taieturii de cardinal maxim este:")
print(taietura_maxima)
