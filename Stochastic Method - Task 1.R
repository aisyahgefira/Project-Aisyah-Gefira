###1
set.seed(42)  # atur seed untuk menghasilkan hasil yang sama setiap kali Anda melakukan simulasi

n_simulasi <- 10000  # jumlah simulasi
x <- rexp(n_simulasi, rate = 1/4)  # melakukan simulasi distribusi eksponensial

expon <- x^2  # menghitung kuadrat dari masing-masing hasil simulasi
expektasi <- mean(expon)  # menghitung rata-rata dari kuadrat hasil simulasi

expektasi  # output nilai E(X^2)



###3
set.seed(42)  # atur seed untuk reproduktibilitas

N <- 500  # jumlah sampel
n <- 50   # ukuran sampel
x_bar <- numeric(N)  # inisialisasi vektor untuk menyimpan mean dari masing-masing sampel

for (i in 1:N) {
  x <- runif(n, min = -5, max = 5)  # generate sampel dari distribusi Uniform
  x_bar[i] <- mean(x)  # hitung mean dari sampel dan simpan ke dalam vektor x_bar
}

hist(x_bar, breaks = 30, main = "Histogram of Sample Means", xlab = "Sample Mean")  # plot histogram dari sample mean


#atau
# Inisialisasi jumlah sampel dan ukuran sampel
N <- 500 # Jumlah sampel
n <- 50  # Ukuran sampel

# Membuat variabel untuk menyimpan mean sampel dari setiap sampel yang di-generate
means <- numeric(N)

# Generate sampel sebanyak N dengan ukuran n dari distribusi uniform [-5, 5]
for (i in 1:N) {
  sampel <- runif(n, min = -5, max = 5)  # Generate sampel dari distribusi uniform
  means[i] <- mean(sampel)  # Hitung mean dari setiap sampel dan simpan
}

# Plot distribusi sampling dari mean sampel
hist(means, breaks = 30, col = "skyblue", border = "white", 
     main = "Distribusi Sampling dari Mean Sampel",
     xlab = "Nilai Mean Sampel", ylab = "Frekuensi")

# Plot distribusi normal dengan mean dan standar error yang diharapkan
mu <- 0  # Mean populasi dari distribusi uniform [-5, 5]
sigma <- (5 - (-5)) / sqrt(n)  # Standar error per teorema limit pusat

curve(dnorm(x, mean = mu, sd = sigma), 
      col = "red", lwd = 2, add = TRUE)

# Menampilkan hasil
legend("topright", legend = c("Distribusi Sampling", "Distribusi Normal Teoretis"),
       col = c("skyblue", "red"), lwd = c(10, 2), bty = "n")



###3b
# Inisialisasi jumlah sampel dan ukuran sampel
N <- 500 # Jumlah sampel
n <- 50  # Ukuran sampel

# Membuat variabel untuk menyimpan mean sampel dari setiap sampel yang di-generate
means <- numeric(N)

# Generate sampel sebanyak N dengan ukuran n dari distribusi uniform [-5, 5]
for (i in 1:N) {
  sampel <- runif(n, min = -5, max = 5)  # Generate sampel dari distribusi uniform
  means[i] <- mean(sampel)  # Hitung mean dari setiap sampel dan simpan
}

# Menampilkan hasil
print(means)



###3c
# Inisialisasi jumlah sampel dan ukuran sampel
N <- 500 # Jumlah sampel
n <- 50  # Ukuran sampel

# Membuat variabel untuk menyimpan mean sampel dari setiap sampel yang di-generate
means <- numeric(N)

# Generate sampel sebanyak N dengan ukuran n dari distribusi uniform [-5, 5]
for (i in 1:N) {
  sampel <- runif(n, min = -5, max = 5)  # Generate sampel dari distribusi uniform
  means[i] <- mean(sampel)  # Hitung mean dari setiap sampel dan simpan
}

# Plot histogram dari 500 mean sampel
hist(means, breaks = 30, col = "skyblue", border = "white", 
     main = "Histogram dari 500 Mean Sampel",
     xlab = "Nilai Mean Sampel", ylab = "Frekuensi")

# Plot garis distribusi normal dengan mean 0
mu <- 0  # Mean yang diharapkan
sigma <- sd(means)  # Standar deviasi dari mean sampel

curve(dnorm(x, mean = mu, sd = sigma), 
      col = "red", lwd = 2, add = TRUE)

# Menampilkan hasil
legend("topright", legend = c("Distribusi Sampling", "Distribusi Normal Teoretis"),
       col = c("skyblue", "red"), lwd = c(10, 2), bty = "n")



###3d
simulate_sampling_distribution <- function(N, n) {
  set.seed(42)  # atur seed untuk reproduktibilitas
  x_bar <- numeric(N)  # inisialisasi vektor untuk menyimpan mean dari masing-masing sampel
  
  for (i in 1:N) {
    x <- runif(n, min = -5, max = 5)  # generate sampel dari distribusi Uniform
    x_bar[i] <- mean(x)  # hitung mean dari sampel dan simpan ke dalam vektor x_bar
  }
  
  hist(x_bar, breaks = 30, main = paste("Histogram of", N, "Sample Means (n =", n, ")"), xlab = "Sample Mean")  # plot histogram dari sample mean
}
simulate_sampling_distribution(500, 10)
simulate_sampling_distribution(500, 15)
simulate_sampling_distribution(500, 30)
simulate_sampling_distribution(500, 50)

#atau
createSamplingSimulation <- function(N, n) {
  # Membuat variabel untuk menyimpan mean sampel dari setiap sampel yang di-generate
  means <- matrix(0, nrow = N, ncol = 1)  # Inisialisasi matriks untuk menyimpan mean sampel
  
  # Generate sampel sebanyak N dengan ukuran n dari distribusi uniform [-5, 5]
  for (i in 1:N) {
    sampel <- runif(n, min = -5, max = 5)  # Generate sampel dari distribusi uniform
    means[i, 1] <- mean(sampel)  # Hitung mean dari setiap sampel dan simpan
  }
  
  # Plot histogram dari mean sampel
  hist(means, breaks = 30, col = "skyblue", border = "white", 
       main = paste("Histogram dari", N, "Mean Sampel (n =", n, ")"),
       xlab = "Nilai Mean Sampel", ylab = "Frekuensi")
  
  # Plot garis distribusi normal dengan mean 0
  mu <- 0  # Mean yang diharapkan
  sigma <- sd(means)  # Standar deviasi dari mean sampel
  
  curve(dnorm(x, mean = mu, sd = sigma), 
        col = "red", lwd = 2, add = TRUE)
  
  # Menampilkan hasil
  legend("topright", legend = c("Distribusi Sampling", "Distribusi Normal Teoretis"),
         col = c("skyblue", "red"), lwd = c(10, 2), bty = "n")
}
createSamplingSimulation(N = 500, n = 10)  # Menjalankan simulasi dengan n=10
createSamplingSimulation(N = 500, n = 15)  # Menjalankan simulasi dengan n=15
createSamplingSimulation(N = 500, n = 30)  # Menjalankan simulasi dengan n=30
createSamplingSimulation(N = 500, n = 50)  # Menjalankan simulasi dengan n=50
