# FINANČNI PRAKTIKUM
# 3.NALOGA : VREDNOTENJE EKSOTIČNIH OPCIJ

library(combinat)
library(Rlab)

# 1. Vaja
# IZRAČUN IZPLAČIL OB ZAPADLOSTI OPCIC S POVPREČNO IZVRŠILNO CENO NAKUPNEGA(X) OZIROMA PRODAJNEGA(Y) TIPA

# a) ROČNI IZRAČUN

W = c(1,2,3,4,5,6)

pot1 = c(50,52.5,49.88,47.38,45.01,47.26)
K1 <- sum((pot1*W)) / sum(W)
izplaciloX1 <- max(pot1[6] - K1,0) # 0
izplaciloY1 <- max(K1 - pot1[6],0) # 0.4909524

pot2 = c(50.00,52.50, 55.12, 57.88, 60.78, 63.81)
K2 <- sum((pot2*W)) / sum(W)
izplaciloX2 <- max(pot2[6] - K2,0) # 4.827143
izplaciloY2 <- max(K1 - pot2[6],0) # 0

pot3 = c(50.00, 47.50, 49.88, 47.38, 45.01, 42.76)
K3 <- sum((pot3*W)) / sum(W)
izplaciloX3 <- max(pot3[6] - K3,0) # 0
izplaciloY3 <- max(K3 - pot3[6],0) # 3.229048

pot4 = c(50.00, 47.50, 45.12, 47.38, 45.01, 47.26)
K4 <- sum((pot4*W)) / sum(W)
izplaciloX4 <- max(pot4[6] - K4,0) # 0.6652381
izplaciloY4 <- max(K4 - pot4[6],0) # 0

pot5 = c(50.00, 52.50, 49.88, 52.37, 54.99, 52.24)
K5 <- sum((pot5*W)) / sum(W)
izplaciloX5 <- max(pot5[6] - K5,0) # 0
izplaciloY5 <- max(K5 - pot5[6],0) # 0.2604762


# b) OBLIKOVANJE FUNCKIJE

izplacilo <- function(vrsta, W, type){
  K <- sum(vrsta*W)/sum(W)
  if(type == "call") {
    return(max(vrsta[length(vrsta)] - K, 0))
    }
  else {
    return(max(K - vrsta[length(vrsta)], 0))
  }
}

###########################################################################################################

# 2. Vaja

# a) DOLOČANJE PREMIJE OPCIJE Z ANALIZO BINOMSKEGA DREVESA

S0 = 50
u = 1.05
d = 0.95
T = 5
R = 0.03  
W = c(1, 2, 3, 4, 5, 6)


binomski<- function(S0,u,d,R,T,W,type) {
  kocka1 <- hcube(c(rep(2,T))) - 1
  kocka2 <- 2 - hcube(c(rep(2, T)))
  kocka <- u^kocka1 * d^kocka2
  matrika <- cbind(S0, kocka)
  bin_drevo <- t(apply(matrika, 1, cumprod))
  izplacila <- (apply(bin_drevo, 1, izplacilo,W = W, type =type))
  q <- (1 + R - d)/(u - d)
  st_u <- rowSums(kocka1)
  st_d <- T - st_u
  verjetnosti <- (q ^ st_u) *((1-q) ^ st_d)
  premija <- sum(izplacila * verjetnosti) / (1 + R)^T
  return(premija)
  
}

Bcall <- binomski(S0,u,d,R,T,W,type = "call") # 2.482287
Bput <- binomski(S0,u,d,R,T,W,type = "put") # 0.1245736


# b) OCENA PREMIJE OPCIJE S POMOČJO MONTE CARLO METODA

monte <- function(S0, u, d, R, T, W, type, N){
  q = (1+R-d) / (u-d)
  poti <- matrix(rbinom(N * T, 1, q), nrow = N, ncol = T)
  vektor_S0 <- rep(S0, N)
  matrikaM <- cbind(vektor_S0, u^poti * d^(1-poti))
  bin_drevoM <- t(apply(matrikaM,1, cumprod))
  izplacilaM <- apply(bin_drevoM, 1, izplacilo, W = W, type = type)
  st_u <- rowSums(poti)
  st_d <- T - st_u
  verjetnostiM <- (q ^ st_u) * ((1-q) ^ st_d)
  E_Q <- sum(izplacilaM) / N
  premija <- E_Q / (1+R) ^ T
  return (premija)
}

S0 <- 60
u <- 1.05
d <- 0.95
R <- 0.01
T <- 15
W <- rep(1,16)
type <- "put"
N1 <- 10
N2 <- 100
N3 <- 1000

M1 <- monte(S0, u, d, R, T, W, type, N1) # 0.5618354
M2 <- monte(S0, u, d, R, T, W, type, N2) # 0.9145231
M3 <- monte(S0, u, d, R, T, W, type, N3) # 1.026428
B <- binomski(S0,u,d,R,T,W,type)

###########################################################################################################

# 3. Vaja
# PRIKAZ REZULTATOV 2. NALOGE NA HISTOGRAMIH

# a) IZBOLJŠAMO NATANČNOST OCENE PREMIJE OPCIJE S PONAVLJANJEM MONTE CARLO METODE 100x

M = 100
simulacija_1 <- c()
for(i in 1:M ){
  simulacija_1 <- c(simulacija_1,monte(S0,u,d,R,T,W,type, N1))
}

simulacija_2 <- c()
for(i in 1:M ){
  simulacija_2 <- c(simulacija_2,monte(S0,u,d,R,T,W,type, N2))
}

simulacija_3 <- c()
for(i in 1:M ){
  simulacija_3 <- c(simulacija_3,monte(S0,u,d,R,T,W,type, N3))
}



# b) PONAZORIMO REZULTATE NA HISTOGRAMIH

min <- floor(min(c(simulacija_1,simulacija_2,simulacija_3))) 
max <- ceiling(max(c(simulacija_1,simulacija_2,simulacija_3))) 

# N1 = 10

histogram1 <- hist(simulacija_1,
                   main = "Monte Carlo: N=10",
                   xlab = "Premija",
                   ylab = "Frekvenca",
                   xlim = c(min, max),
                   col ="yellow")
abline(v = c(mean(simulacija_1), B), col=c("green","red"),lty=2)
arrows(mean(simulacija_1), 0, mean(simulacija_1) - sd(simulacija_1), 0, col = "green",length = 0.1)
arrows(mean(simulacija_1), 0, mean(simulacija_1) + sd(simulacija_1), 0, col = "green",length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

# N2 = 100

histogram2 <- hist(simulacija_2,
                   main = "Monte Carlo: N = 100",
                   xlab = "Premija",
                   ylab = "Frekvenca",
                   xlim = c(min, max),
                   col ="yellow")
abline(v = c(mean(simulacija_2), B), col=c("green","red"),lty=2)
arrows(mean(simulacija_2), 0, mean(simulacija_2) - sd(simulacija_2), 0, col = "green",length = 0.1)
arrows(mean(simulacija_2), 0, mean(simulacija_2) + sd(simulacija_2), 0, col = "green",length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))


# N3 = 1000

histogram3 <- hist(simulacija_3,
                   main = "Monte Carlo: N = 1000",
                   xlab = "Premija",
                   ylab = "Frekvenca",
                   xlim = c(min, max),
                   col ="yellow")
abline(v = c(mean(simulacija_3), B), col=c("green","red"),lty=2)
arrows(mean(simulacija_3), 0, mean(simulacija_3) - sd(simulacija_3), 0, col = "green",length = 0.1)
arrows(mean(simulacija_3), 0, mean(simulacija_3) + sd(simulacija_3), 0, col = "green",length = 0.1)

legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

###########################################################################################################

