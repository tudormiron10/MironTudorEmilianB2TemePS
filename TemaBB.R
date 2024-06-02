#B1
TC = function(x1, x2, x3, R, r)
{
  return((x3^2 + (sqrt(x1^2 + x2^2) - R)^2) < r^2)
}

# Functie pentru estimarea volumului torului folosind Monte Carlo
monte_carlo_volum = function(R, r, nr) 
{
  count = 0
  for (i in 1:nr) {
    x1 = runif(1, -R-r, R+r)
    x2 = runif(1, -R-r, R+r)
    x3 = runif(1, -r, r)
    if (TC(x1, x2, x3, R, r)) {
      count = count + 1
    }
  }
  volum_estimat = count / nr * (2 * R * pi * r^2)
  return(volum_estimat)
}

R = 10
r = 3

# Nr esantioane pentru estimare
nr = c(10000, 20000, 50000)

# Estimari volum si erori
for (n in nr) 
{
  volum_estimat = monte_carlo_volum(R, r, n)
  volum_exact = 2 * pi * R * r^2
  eroare_relativa = abs(volum_estimat - volum_exact) / volum_exact * 100
  cat("Pentru", n, "esantioane:\n")
  cat("Estimarea volumului:", volum_estimat, "\n")
  cat("Volumul exact:", volum_exact, "\n")
  cat("Eroare relativa:", eroare_relativa, "%\n\n")
}

#B2

triunghi = function(x, y)
{
  return(y >= 0 && y <= 2 * x && y <= 6 - 3 * x)
}

# Functie pentru estimarea ariei folosind Monte Carlo
monte_carlo_arie = function(nr) 
{
  count = 0
  for (i in 1:nr)
  {
    x = runif(1, 0, 2)
    y = runif(1, 0, 6)
    if (triunghi(x, y)) 
    {
      count = count + 1
    }
  }
  aria_drp = 2 * 6  # arie dreptunghi [0, 2] x [0, 6]
  aria_estimata = (count / nr) * aria_drp
  return(aria_estimata)
}

# nr esantioane
nr = 20000

aria_estimata = monte_carlo_arie(nr)
print("Estimarea ariei triunghiului:")
print(aria_estimata)

#B3

#a)
f_a = function(x)
{
  return((2 * x - 1) / (x^2 - x - 6))
}

# Functie pentru estimarea integralei folosind Monte Carlo
monte_carlo_integrala = function(f, a, b, nr) 
{
  x = runif(nr, a, b)
  fx = f(x)
  integrala_estimata = (b - a) * mean(fx)
  return(integrala_estimata)
}

# Exemplu apel pentru a)
a1 = -1
b1 = 1
nr = 20000
valoare_exacta_a = log(3) - log(2)

# Estimare integrala pentru a)
integrala_estimata_a = monte_carlo_integrala(f_a, a1, b1, nr)
print("Estimarea integralei a):")
print(integrala_estimata_a)
print("Valoarea exacta a):")
print(valoare_exacta_a)
print("Eroare relativa a):")
print(abs(integrala_estimata_a - valoare_exacta_a) / valoare_exacta_a * 100)

# b)
f_b = function(x)
{
  return((x + 4) / (x - 3)^(1/3))
}

# Exemplu apel pentru b)
a2 = 3
b2 = 11
valoare_exacta_b = 61.2

# Estimare integrala b)

integrala_estimata_b = monte_carlo_integrala(f_b, a2, b2, nr)
print("Estimarea integralei b):")
print(integrala_estimata_b)
print("Valoarea exacta b):")
print(valoare_exacta_b)
print("Eroare relativa b):")
print(abs(integrala_estimata_b - valoare_exacta_b) / valoare_exacta_b * 100)

# c)
f_c = function(x)
{
  return(x * exp(-x^2))
}

# Estimare integrala Monte Carlo pe un interval care contine infinit
monte_carlo_integrala_infinita = function(f, nr) 
{
  x = rexp(nr) # pentru a gestiona infinitul
  fx = f(x) / dexp(x) # ajustare pentru densitatea exponentiala
  integrala_estimata = mean(fx)
  return(integrala_estimata)
}

# Exemplu apel
valoare_exacta_c = 1 / 2

integrala_estimata_c = monte_carlo_integrala_infinita(f_c, nr)
print("Estimarea integralei (c):")
print(integrala_estimata_c)
print("Valoarea exacta (c):")
print(valoare_exacta_c)
print("Eroare relativa (c):")
print(abs(integrala_estimata_c - valoare_exacta_c) / valoare_exacta_c * 100)

#B4

# Parametrii
n = 1000
p = 0.25
q = 0.01
utilizatori_initiali = 10000
target_utilizatori = 15000
nr = 10000
total_ani = 40
luni = 10
confidence_level = 0.99
eroare = 0.01

#a) Estimare nr mediu ani pentru a ajunge la 15000 de utilizatori
estimare_ani = function() 
{
  utilizatori = utilizatori_initiali
  ani = 0
  while (utilizatori < target_utilizatori) 
  {
    utilizatori = sum(rbinom(utilizatori, 1, 1 - q)) + rbinom(1, n, p)
    ani = ani + 1
  }
  return(ani)
}

mean_ani = mean(replicate(nr, estimare_ani()))
print("Numarul mediu de ani pentru a ajunge la 15000 de utilizatori este:")
print(mean_ani)

#b) Estimare probabilitate ca dupa 40 de ani si 10 luni sa existe cel putin 15000 de utilizatori
estimare_utilizatori_dupa_ani = function(total_ani, luni) 
{
  utilizatori = utilizatori_initiali
  for (i in 1:(total_ani * 12 + luni)) 
  {
    utilizatori = sum(rbinom(utilizatori, 1, 1 - q)) + rbinom(1, n, p)
  }
  return(utilizatori >= target_utilizatori)
}

estimare_probabilitate = mean(replicate(nr, estimare_utilizatori_dupa_ani(total_ani, luni)))
print("Probabilitatea ca dupa 40 de ani si 10 luni sa existe cel putin 15000 de utilizatori este:")
print(estimare_probabilitate)

#c) Estimare probabilitate cu o eroare de cel mult +/-0.01 cu probabilitatea 0.99

z = qnorm((1 + confidence_level) / 2)
nr_alfa = ceiling((z / (2 * eroare))^2)

estimare_probabilitate_alfa = mean(replicate(nr_alfa, estimare_utilizatori_dupa_ani(total_ani, luni)))
print("Probabilitatea estimata cu o eroare de +/-0.01 si probabilitatea de 0.99 este:")
print(estimare_probabilitate_alfa)