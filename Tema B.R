#1
GEOM = function(n, p) {
  return (c(mean(rgeom(n, p)), 1 / p));
}
GEOM(5000, 0.2)
GEOM(10000, 0.6)
GEOM(100000, 0.6)
GEOM(500000, 0.8)

#2
CLT = function(r, n, N, z) {
  exp = 0
  st_dev = sqrt(r / (r - 2))
  upper = z * st_dev / sqrt(n) + exp
  sum = 0
  for (i in 1:N) {
    x_n = mean(rt(n, r))
    if (x_n <= uppeR) {
      sum = sum + 1
    }
  }
  return (sum / N)
}
prob = CLT(3, 50, 5000, -1.5)
true_prob = pnorm(-1.5)
eroare = abs(prob - true_prob) / true_prob
eroare

prob = CLT(4, 50, 10000, 0)
true_prob = pnorm(0)
eroare = abs(prob - true_prob) / true_prob
eroare

prob = CLT(5, 50, 20000, 1.5)
true_prob = pnorm(1.5)
eroare = abs(prob - true_prob) / true_prob
eroare

#3
ML_Binom = function(n, p, h, k) {
  expec = n * p
  dev = sqrt(n * p * (1 - p))
  q1 = (h - expec) / dev
  q2 = (k - expec) / dev
  approx_prob = pnorm(q2) - pnorm(q1)
  return (approx_prob)
}
ML_Binom(100, 0.3, 20, 40)
#Pentru x < 40
pbinom(40, 100, 0.3) - dbinom(40, 100, 0.3)
pbinom(39, 100, 0.3)
#Pentru x < 20
pbinom(20, 100, 0.3) - dbinom(20, 100, 0.3)
pbinom(19, 100, 0.3)
cat(pbinom(39, 100, 0.3) - pbinom(19, 100, 0.3))