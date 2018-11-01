# FINANČNI PRAKTIKUM 
# 2.NALOGA: KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM

library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(shiny)
library(tidyr)
library(magrittr)
library(nlme)
library(reshape2)

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

# parametri =
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
                   col = "gainsboro")
curve(dpareto1(x,shape, min),
      add = TRUE, 
      from = min(vzorec$odskodnine), 
      to = max(vzorec$odskodnine), 
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

var.Y <- Inf
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
                         method = "rounding"
)


# b)graf porazdelitvene funkcije Y ter diskretne porazdelitvene funkcije

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
legend("right",c("Diskretna funkcija","Porazdelitvena funkcija"),
       col = c("gray48","mediumvioletred"),lwd = c(3,1))

# c)

diskretnaY2 <- discretize(ppareto1(x,shape, min),
                         from = 0, 
                         to = 100000,
                         step = h)

diskr.S <- aggregateDist(method = "recursive",
                     model.freq = "binom",
                     model.sev = diskretnaY2,
                     size = 20,
                     prob = 1/2,
                     x.scale = h,
                     convolve = 0,
                     maxit=1000000,
                     tol = 0.002)


# d) Nasvet: S je diskretna spremenljivka

#izračun upanja in disperzije

#upanje slučanje spremenljivke S = (vrednosti S)* (skoki v teh točkah) 
moment1.S <- sum( knots(diskr.S) * diff(diskr.S)) 
# = 63.51607

# drugi moment slučajne spremenljivke 
moment2.S <- sum( knots(diskr.S)^2 * diff(diskr.S))
# = 4824.193

#disperzija od S = E(S^2)-E(S)^2 
disperzija.S <- moment2.S - moment1.S^2 
# = 789.9022

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
# = 66.06927

var.simS <- var(simS)
# = 1054.335

# S pomočjo simulacije dobimo slabšo oceno za pričakovano vrednost, saj je vrednost, ki smo jo dobili
# v drugi nalogi bližja vrednosti iz 1. naloge.
# Lahko pa površno rečemo, da dobimo boljšo oceno za varianco kot oceno pri Panjerjevem algoritmu,
# saj je vrednost višja in tako bližje neskočnosti.

# c)

sim.graf <- plot(diskr.S)
plot(ecdf(simS),
     col = 'firebrick1',
     add = TRUE,
     lwd = 2)
legend('right', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'firebrick1'),
       lty=1:1)


