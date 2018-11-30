#FINANČNI PRAKTIKUM: 4.Naloga

library(readr)
library(graphics)
library(stats)

# 1.Vaja: Uvoz in predstavitev podatkov 
# a)

uvoz_srebro <- read_csv("srebro.csv")

# izberemo samo zaključne tečaje

srebro <- uvoz_srebro[,c(0,1,5)]

#obrnemo podatke
srebro <- srebro[123:1,]

srebro$Close <- as.numeric(gsub("\\$","",srebro$Close))

# b) 
S <- ts(srebro$Close)
graf_S <- ts.plot(S,
                  xlab='Čas', 
                  ylab ='Vrednost v dolarjih', 
                  main = 'Vrednost zlata')
points(srebro$Close, pch = 20)

###########################################################################

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

napoved_vrsta(S,5)

# c)
graf_napovedana <- ts.plot(S,napoved_vrsta(S,5),
                           main ="Drseče povprečje",
                           xlab = "Čas",
                           ylab = "Vrednost v dolarjih",
                           col = c("black", "red"),
                           lwd = c(1,2))
points(srebro$Close, pch = 20)


# d)
SKN <- function(vrsta, zglajena_vrsta, k){
  l <- length(vrsta)
  napaka <- 0
  for (i in k:(l-1)){
    napaka <- napaka + (vrsta[i+1] - glajena.vrsta[i+1])^2
  }
  return (napaka/(l-k))
}











