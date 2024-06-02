#A1

calcul_probabil = function(lambda, p, n, k, m)
{
  if (lambda <= 0) {
    stop("Lambda trebuie sa fie pozitiva pentru distributia Poisson.")
  }
  
  valori = k:m
  
  # distributie Poisson
  poisson_prob = dpois(valori, lambda)
  # distributie geometrica
  geometric_prob = dgeom(valori, p)
  # distributie binomiala
  binomial_prob = dbinom(valori, n, p)
  
  # Lista cu probabilitati pentru cele 3 distributii
  lista_probabil = list(
    Poisson = poisson_prob,
    Geometric = geometric_prob,
    Binomial = binomial_prob
  )
  
  return(lista_probabil)
}

# Functie pentru reprezentare grafica

grafic = function(probabili, k, m) 
{
  valori = k:m
  
  plot(valori, probabili$Poisson, type = "b", col = "blue", 
       xlab = "Valori", ylab = "Probabilitate", 
       main = "Funcțiile de Masă de Probabilitate",
       ylim = c(0, max(c(probabili$Poisson, probabili$Geometric, probabili$Binomial))))
  
  # geometric
  points(valori, probabili$Geometric, type = "b", col = "green")
  
  # binomial
  points(valori, probabili$Binomial, type = "b", col = "red")
  
}

# functie pentru determinarea lui k0 (first)

gaseste_k_first = function(lambda)
{
  prag = 1 - 1e-6
  probabil_cumulativa = 0
  k_first = 0
  
  # gasirea celui mai mic k cu probabilitatea ceruta
  
  while (probabil_cumulativa <= prag) {
    probabil_cumulativa = ppois(k_first, lambda)
    k_first = k_first + 1
  }
  
  return(k_first - 1)  
}

# Exemplu 

lambda = 3.0  # lambda pentru Poisson
p = 0.2  # probabilitatea pentru binomiala și geometrica
n = 10  # numarul de teste
k = 0  # [k,m] - interval valori
m = 10 # pana unde se iau valori

probabil = calcul_probabil(lambda, p, n, k, m)

print(probabil$Poisson)
print(probabil$Geometric)
print(probabil$Binomial)

grafic(probabil, k, m)

k_first = gaseste_k_first(lambda)
print(k_first)

#A2

citeste_date = function(nume_fisier) 
{
  date = read.csv(nume_fisier)  
  
  # frecvente absolute si relative 
  frecvente_P = table(date$P)
  frecvente_S = table(date$S)
  
  # media
  media_P = mean(date$P)
  media_S = mean(date$S)
  
  rezultate = list(
    frecvente_P = frecvente_P,
    frecvente_S = frecvente_S,
    media_P = media_P,
    media_S = media_S
  )
  
  return(rezultate)
}

# Functie pentru eliminarea valorilor aberante folosind IQR

elimina_aberante = function(esantion) {
  Q1 = quantile(esantion, 0.25)
  Q3 = quantile(esantion, 0.75)
  IQR_val = Q3 - Q1
  
  lim_inf = Q1 - 1.5 * IQR_val
  lim_sup = Q3 + 1.5 * IQR_val
  
  esantion_bun = esantion[esantion >= lim_inf & esantion <= lim_sup]
  
  return(esantion_bun)
}

rezultate = citeste_date("note.csv")
P_curat = elimina_aberante(rezultate$frecvente_P)
S_curat = elimina_aberante(rezultate$frecvente_S)

print("Frecvente pentru esantionul P:")
print(P_curat)
print("Frecvente pentru esantionul S:")
print(S_curat)
print("Media pentru esantionul P:")
print(rezultate$media_P)
print("Media pentru esantionul S:")
print(rezultate$media_S)
