# FINANČNI PRAKTIKUM
# 4.Naloga: GLAJENJE ČASOVNIH VRST

library(readr)
library(graphics)
library(stats)
library(dplyr)

# 1. Vaja: Uvoz in predstavitev podatkov 
# a)

uvoz_srebro <- read_csv("srebro.csv")

# izberemo samo zaključne tečaje

srebro <- uvoz_srebro[,c(0,1,5)]

#obrnemo podatke
srebro <- srebro[123:1,]

#odstranimo znak za dolar
srebro$Close <- as.numeric(gsub("\\$","",srebro$Close))

# b) 
S <- ts(srebro$Close)
graf_S <- ts.plot(S,
                  xlab='Čas', 
                  ylab ='Vrednost v dolarjih', 
                  main = 'Vrednost zlata')
points(srebro$Close, pch = 20)

###########################################################################################################

# 2. Vaja: Glajenje z drsečim povprečjem reda k

# a)
G <- function(vrsta,k){
  glajene.vrednosti <- c()
  for (i in (k+1):(length(vrsta))){
    glajene.vrednosti[i] <- sum(vrsta[(i-1):(i-k)])/k
  }
  zglajena_vrsta <- ts(glajene.vrednosti)
  return(zglajena_vrsta)
}

# b)

napoved_vrsta <- function(vrsta,k){
  zglajena_vrsta <- G(vrsta,k)
  napoved <- rep(tail(zglajena_vrsta, n = 1),10)
  napoved_vrsta <- ts(c(zglajena_vrsta,napoved))
  return(napoved_vrsta)
}

napoved5 <- napoved_vrsta(S,5)

# c)
graf_napovedana5 <- ts.plot(S,napoved5,
                           main ="Drseče povprečje reda 5",
                           xlab = "Čas",
                           ylab = "Vrednost v dolarjih",
                           col = c("black", "red"),
                           lwd = c(1,2))
points(srebro$Close, pch = 20)


# d)
SKN <- function(vrsta, k){
  l <- length(vrsta)
  glajena.vrsta <- G(vrsta,k)
  napaka <- 0
  for (i in k:(l-1)){
    napaka <- napaka + (vrsta[i+1] - glajena.vrsta[i+1])^2
  }
  return (napaka/(l-k))
}


skn5 <- SKN(S, 5)
# 0.04239193


# e)

# RED GLAJENJA = 15

napoved15 <- napoved_vrsta(S,15)


graf_napovedana15 <- ts.plot(S,napoved15,
                           main ="Drseče povprečje reda 15",
                           xlab = "Čas",
                           ylab = "Vrednost v dolarjih",
                           col = c("black", "red"),
                           lwd = c(1,2))
points(srebro$Close, pch = 20)


skn15 <- SKN(S, 15)
# 0.08935058



# RED GLAJENJA = 30

napoved30 <- napoved_vrsta(S,30)


graf_napovedana30 <- ts.plot(S,napoved30,
                             main ="Drseče povprečje reda 30",
                             xlab = "Čas",
                             ylab = "Vrednost v dolarjih",
                             col = c("black", "red"),
                             lwd = c(1,2))
points(srebro$Close, pch = 20)


skn30 <- SKN(S, 30)
# 0.2195886


###########################################################################################################

# 3. Vaja:  Eksponentno glajenje
# a)

EG <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  glajene_vrednosti <- vrsta[1]
  for (i in 2:dolzina){
    glajene_vrednosti[i] <- alpha*vrsta[i] + (1-alpha)*glajene_vrednosti[i-1]
  }
  zglajena_vrsta <- ts(glajene_vrednosti)
  return(zglajena_vrsta)
}

# b)
# alpha = 0,15
eksponentno.glajena <- EG(S,0.15)

napoved_eks <- last(eksponentno.glajena) 
# 14.32263

graf_eksponentno <- ts.plot(S,eksponentno.glajena,
                             main ="Eksponentno glajenje alpha = 0.15",
                             xlab = "Čas",
                             ylab = "Vrednost v dolarjih",
                             col = c("black", "red"),
                             lwd = c(1,2))
points(srebro$Close, pch = 20)


# c)

SKN.E <-function(vrsta, alpha){
  dolzina <- length(vrsta)
  napaka <- 0
  glajena.vrsta <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - glajena.vrsta[i+1])^2
  }
  return(napaka/(dolzina-1))
}

optimal.alpha <- as.numeric(optimize(SKN.E, c(0,1), vrsta = S)[1])
# 0.9999315

# d) OPTIMALNA ALPHA
# alpha = 0.9999315
eksponentno.glajenaOPT <- EG(S,0.9999315)

napoved_eksOPT<- last(eksponentno.glajenaOPT) 
# 14.36998

graf_eksponentnoOPT <- ts.plot(S,eksponentno.glajenaOPT,
                            main ="Eksponentno glajenje optimalna alpha",
                            xlab = "Čas",
                            ylab = "Vrednost v dolarjih",
                            col = c("black", "red"),
                            lwd = c(1,2))
points(srebro$Close, pch = 20)



