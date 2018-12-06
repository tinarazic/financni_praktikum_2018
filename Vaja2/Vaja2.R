# FINANČNI PRAKTIKUM 
# 2.NALOGA: KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM

library(dplyr)
library(readr)
library(actuar)

# 1. naloga
# Porazdelitev individualnih škodnih zahtevkov

# a) izbira vzorca: vzorec 1

vzorec <- read_csv("Vaja2/vzorec1.txt", col_names = c("odskodnine"))

histogram1 <- hist(vzorec$odskodnine,
                   xlab = "Višina odškodnine", 
                   ylab = "Frekvenca",
                   main = "Histogram odškodnin", 
                   col = c("gainsboro"))

# b) histogram kaže na Paretovo porazdelitev

parametri <- mde(vzorec$odskodnine, 
                 ppareto1, 
                 start = list(shape = 1, min = 1), measure = "CvM")
shape <- as.numeric(parametri$estimate[1])
min <- as.numeric(parametri$estimate[2])

# parametri
# shape       min    
# 1.860671   2.053679 
# distance  
# 0.07064093 

# c) 

histogram2 <- hist(vzorec$odskodnine,
                   probability = TRUE, 
                   main = "Primerjava histograma in vzorčne gostote ", 
                   xlab = "Višina odškodnine", 
                   ylab = "Gostota",
                   col = "gainsboro",
                   ylim = c(0,0.8))
curve(dpareto1(x,shape, min),
      add = TRUE, 
      col = "mediumvioletred", 
      lwd = 2)
legend("right", lty = 1, col = c("mediumvioletred"),c("Paretova porazdelitev"))


porazdelitvi <- plot(ecdf(vzorec$odskodnine), 
                     main = "Porazdelitvena funkcija odškodnin",
                     xlab = "Višina odškodnine",
                     ylab = "Porazdelitvena funkcija",
                     col = "gray")
curve(ppareto1(x,shape, min),
      add = TRUE,
      from = min(vzorec$odskodnine),
      to = max(vzorec$odskodnine),
      col = "mediumvioletred",
      lwd = 2)
legend("right", lty = c(1, 6),
       lwd = c(2,3),
       c("Paretova porazdelitev", "Emipirična porazdelitev"), 
       col = c("mediumvioletred","gray"))

# d)
# za porazdelitev števila odškodninskih zahtevkov vzamemo:
# binomska -> binom -> 

size =  20  # = n 
prob = 1/2 # = p

upanje.Y <- as.numeric(shape * min / (shape - 1)) # = 4.439818
upanje.N <- size * prob # = 10

upanje.S <- upanje.N * upanje.Y # = 44.39818

var.Y <- Inf # ker je shape <= 2
var.N <- size * prob * (1 - prob) # = 5

var.S <- upanje.N * var.Y + ((upanje.Y)^2)*var.N # = Inf, ker je var.Y = Inf

# Torej:
# upanje kolektivne škode S = 44.39818
# disperzija kolektivne škode S = Inf

###########################################################################

# 2. naloga
# Določanje porazdelitve kumulativne škode s Panjerjevim algoritmom

# a)Diskretizacija porazdelitve Y

z <- 0  #začetek intervala
n <- 80 #konec intervala
h <- 0.25 #dolžina koraka

diskretnaY <- discretize(ppareto1(x,shape, min), 
                         from = z, 
                         to = n * h,
                         step = h,
                         method = "rounding")


# b) graf porazdelitvene funkcije Y ter njene diskretiziacije

poraz.Y <- stepfun(seq(z, n * h - h, h), diffinv(diskretnaY)) 

#diffinv nam da porazdelitveno funkcijo


diskretna <- plot(poraz.Y, 
     main = "Paretova porazdelitev",
     do.points = FALSE,
     xlab = "x",
     ylab = "Porazdelitvena funkcija",
     col = "gray", 
     lwd = 3)
curve(ppareto1(x, shape, min),
      from = z, 
      to = n*h,
      add = TRUE, 
      col = "mediumvioletred")
legend("right",c("Diskretizirana porazdelitev Y","Porazdelitvena funkcija Y"),
       col = c("gray48","mediumvioletred"),lwd = c(3,1))

# c)

diskretnaY2 <- discretize(ppareto1(x,shape, min), 
                         from = 0, 
                         to = 10000,
                         step = h,
                         method = "rounding")


diskr.S <- aggregateDist(method = "recursive",
                     model.freq = "binom",
                     model.sev = diskretnaY2,
                     size = 20,
                     prob = 1/2,
                     x.scale = h,
                     maxit = 10000,
                     convolve = 0,
                     tol=0.001)


# d) Nasvet: S je diskretna spremenljivka

# izračun upanja in disperzije

# upanje slučanje spremenljivke S = (vrednosti S)* (skoki v teh točkah) 
moment1.S <- sum( knots(diskr.S) * diff(diskr.S)) 
# = 43.71658

# drugi moment slučajne spremenljivke 
moment2.S <- sum( knots(diskr.S)^2 * diff(diskr.S))
# = 2375.265

# disperzija od S = E(S^2)-E(S)^2 
disperzija.S <- moment2.S - moment1.S^2 
# = 464.1263

#####################################################################################


# 3. naloga
# Določanje porazdelitve kumulativne škode z Monte Carlo simulacijami

# a) simulacija 10000 vrednosti slučajne spremenljivke S

#simulacija spremenljivke N
simN <- rbinom(10000, 20, 1/2)

#simulacija spremenljivke S
simS <- c()

for (n in simN){
  simS <- c(simS, sum(rpareto1(n, shape, min) ))
}

# b)ocena za upanje in disperzijo spremenljivke S

upanje.simS <- mean(simS)
# = 45.11756

var.simS <- var(simS)
# = 4707.446

# Vrednosti za upanje sta si zelo blizu, vrednosti za varianco pa se med seboj zelo razlikujeta.
# Če primerjamo vrednosti še z vrednsotmi iz prve naloge, vidimo, da smo v obeh primerih dobili dobro oceno
# za upanje, za varianco pa "boljšo" oceno pri Monte Carlo simulaciji.

# c)
sim.graf <- plot(diskr.S)
plot(ecdf(simS),col = 'firebrick1', add = TRUE)
legend('right', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'firebrick1'),
       lty=1:1)


