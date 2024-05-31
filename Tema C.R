#I.a 
trei = function(k, n, p, lambda) {
  x = k:n
  y_pois = dpois(x, lambda)
  y_geom = dgeom(x, p)
  y_binom = dbinom(x, n, p)
  
  plot(x,                                           
       y_pois,                                      
       type = "l",                                  
       main = "Probabilitate Masa",       
       lwd = 2,                                   
       lty = 1,                                   
       col = "blue",                                
       ylim = c(0, max(y_pois, y_geom, y_binom)), 
       xlab = "k:n",                                
       ylab = "Probabilitate")                      
  lines(x, y_geom, type = "l", lwd = 2, lty = 2, col = "red")
  lines(x, y_binom, type = "l", lwd = 2, lty = 3, col = "green")
  legend("topright",                                
         c("Poisson", "Geometric", "Binomial"),     
         lty = c(1, 2, 3),                          
         lwd = 2,                                   
         col = c("blue", "red", "green"))           
}

#I.b
geom_stuff = function(p) {
  cat("P(x = impar) = ", 1 / (2 - p), "\n") 
  cat("P(x >= 4) = ", pgeom(4, p, lower.tail = FALSE) + (1 - p) ^ 3 * p, "\n")
  cat("P(x <= 20) = ", pgeom(20, p), "\n")
}

#I.c
pois_stuff = function(lambda) {
  k = 0
  limit = 10 ^ (-7)
  x = exp(-lambda)
  while (ppois(k, lambda, lower.tail = FALSE) + x < limit) {
    k = k + 1;
    x = x * lambda / k
  }
  cat("Cea mai mica valuare k0 pentru care P(Y >= k0) < 10 ^ (-7) este:", k, "\n")
}

#II.a
calculate_statistics = function(file_name) {
  data = read.csv(file_name)
  sample_P = data[["P"]]
  sample_S = data[["S"]]
  cat("Statistics for sample P:\n")
  cat("Median:", median(sample_P), "\n")
  cat("Mean:", mean(sample_P), "\n")
  cat("Standard Deviation:", sd(sample_P), "\n")
  cat("Quartiles:\n")
  cat("Q1:", quantile(sample_P, 0.25), "\n")
  cat("Q2:", quantile(sample_P, 0.5), "\n")
  cat("Q3:", quantile(sample_P, 0.75), "\n")
  cat("\n")
  cat("Statistics for sample S:\n")
  cat("Median:", median(sample_S), "\n")
  cat("Mean:", mean(sample_S), "\n")
  cat("Standard Deviation:", sd(sample_S), "\n")
  cat("Quartiles:\n")
  cat("Q1:", quantile(sample_S, 0.25), "\n")
  cat("Q2:", quantile(sample_S, 0.5), "\n")
  cat("Q3:", quantile(sample_S, 0.75), "\n")
}

#II.b
remove_outliers = function(file_name, sample_name) {
  data = read.csv(file_name)
  sample = data[[sample_name]]
  Q1 = quantile(sample, 0.25)
  Q3 = quantile(sample, 0.75)
  IQR = Q3 - Q1
  lower = Q1 - 1.5 * IQR
  upper = Q3 + 1.5 * IQR
  cat(lower, " : ", upper, "\n")
  trimmed_sample = sample[sample >= lower & sample <= upper]
  return (trimmed_sample)
}


#II.c
plot_frequency_distribution = function(file_name) {
  par(mfrow = c(1, 2))
  hist(remove_outliers(file_name, "P"),
       breaks = seq(1, 10, by = 1),
       main = "P",
       xlab = "Valori",
       ylab = "Frecventa",
       col = "blue",
       border = "yellow")
  hist(remove_outliers(file_name, "S"),
       breaks = seq(1, 10, by = 1),
       main = "S",
       xlab = "Valori",
       ylab = "Frecventa",
       col = "blue",
       border = "yellow")
}