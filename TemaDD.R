#D1

data = read.csv("probabilitati.csv")

# medie si intervale de incredere
punctaje = data$probabilitati
n = length(punctaje)
mean_punctaje = mean(punctaje)
sd_punctaje = sd(punctaje)
sigma2 = 92.16
sigma = sqrt(sigma2)

# interval de incredere de 95%
eroare_95 = qnorm(0.975) * (sigma / sqrt(n))
capat_stanga_95 = mean_punctaje - eroare_95
capat_dreapta_95 = mean_punctaje + eroare_95

# interval de incredere de 99%
eroare_99 = qnorm(0.995) * (sigma / sqrt(n))
capat_stanga_99 = mean_punctaje - eroare_99
capat_dreapta_99 = mean_punctaje + eroare_99

print("Interval de incredere de 95% pentru punctajul mediu:")
print(capat_stanga_95)
print("la")
print(capat_dreapta_95)
print("Interval de incredere de 99% pentru punctajul mediu:")
print(capat_stanga_99)
print("la")
print(capat_dreapta_99)

#D2

data = read.csv("statistica.csv")

# medie si derivarea standard a esantionului
punctaje = data$statistica 
n = length(punctaje)
mean_punctaje = mean(punctaje)
sd_punctaje = sd(punctaje)

# interval de incredere de 95%
eroare_95 = qnorm(0.975) * (sd_punctaje / sqrt(n))
capat_stanga_95 = mean_punctaje - eroare_95
capat_dreapta_95 = mean_punctaje + eroare_95

# interval de incredere de 99%
eroare_99 = qnorm(0.995) * (sd_punctaje / sqrt(n))
capat_stanga_99 = mean_punctaje - eroare_99
capat_dreapta_99 = mean_punctaje + eroare_99

print("Interval de incredere de 95% pentru punctajul mediu:")
print(capat_stanga_95)
print("la")
print(capat_dreapta_95)

print("Interval de incredere de 99% pentru punctajul mediu:")
print(capat_stanga_99) 
print("la")
print(capat_dreapta_99)

#D3

n = 100  # marime esantion
numar_studenti_inapti = 14 
p_observat = numar_studenti_inapti / n  
p_0 = 0.15  # proportia dorita conform ipotezei nule

# testul z
z = (p_observat - p_0) / sqrt((p_0 * (1 - p_0)) / n)

# Valori critice pentru nivelurile de semnificatie
alpha_1 = 0.01
alpha_5 = 0.05
z_critic_1 = qnorm(1 - alpha_1 / 2)
z_critic_5 = qnorm(1 - alpha_5 / 2)

p_value = 2 * (1 - pnorm(abs(z)))

print("Statistica z calculata:")
print(z)
print("Valoarea p:")
print (p_value)
print("Intervalul critic pentru alfa = 0.01:")
print (-z_critic_1)
print("la")
print(z_critic_1)
print("Intervalul critic pentru alfa = 0.05:")
print(-z_critic_5)
print("la")
print(z_critic_5)

if (abs(z) > z_critic_1) {
  print("La nivelul de semnificatie de 1%, respingem ipoteza nula.")
} else {
  print("La nivelul de semnificatie de 1%, nu respingem ipoteza nula.")
}

if (abs(z) > z_critic_5) {
  print("La nivelul de semnificatie de 5%, respingem ipoteza nula")
} else {
  print("La nivelul de semnificatie de 5%, nu respingem ipoteza nula.")
}
