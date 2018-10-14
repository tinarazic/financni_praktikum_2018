# FINAN?NI PRAKTIKUM - vaja 1
# obdobje 2011 - 2013, 3 x 6 
# 1. Uvoz podatkov in dinamika obrestnih mer v ?asu
#a)uvoz
library(dplyr)
library(readr)
library(data.table)

hist_EURIBOR_2011 <- read_csv("hist_EURIBOR_2011.csv")
hist_EURIBOR_2012 <- read_csv("hist_EURIBOR_2012.csv")
hist_EURIBOR_2013 <- read_csv("hist_EURIBOR_2013.csv")

#b)
# - izbira stolpcev

obresti_2011<- hist_EURIBOR_2011[,c(1,2,23,43,66,85,107,129,150,173,195,216,238)]
obresti_2012<- hist_EURIBOR_2012[,c(1,2,24,45,67,86,108,129,151,174,194,217,239)]
obresti_2013<- hist_EURIBOR_2013[,c(1,2,24,44,64,85,107,127,150,172,193,216,237)]

# - obrnemo tabelo

obresti_2011 <- t(obresti_2011)
obresti_2012 <- t(obresti_2012)
obresti_2013 <- t(obresti_2013)

# - poimenujemo stolpce
colnames(obresti_2011) <- obresti_2011[1, ]
obresti_2011 <- obresti_2011[-1, ]
colnames(obresti_2012) <- obresti_2012[1, ]
obresti_2012 <- obresti_2012[-1, ]
colnames(obresti_2013) <- obresti_2013[1, ]
obresti_2013 <- obresti_2013[-1, ]

# - zdru?imo tabele v eno
obresti <- data.table(rbind(obresti_2011,obresti_2012,obresti_2013), keep.rownames = TRUE)

#c) narišemo graf
#naredimo 2 ?asovni vrsti, T1 za 6 mesecev, U1 za 12 mesecev

T1 <- ts(data = obresti[,9],start = c(2011,1),frequency = 12)
U1 <- ts(data = obresti[,15],start = c(2011,1),frequency = 12)

#nari?emo graf
library(graphics)

grafEURIBOR <- ts.plot(T1,U1,xlab = "Time",ylab = "%", main = "EURIBOR", col = c("darksalmon","cornflowerblue"))
legend("topright",legend=c("6 mesecev", "12 mesecev"),col=c("darksalmon", "cornflowerblue"),lty=1)


#2. Oblika obrestne krivulje 
#a) izbira datumov:01/04/2011 razmik & 01/08/2012 hrib?ek1 & 01/02/2013 hrib?ek2
#b)

library(ggplot2)
library(tibble)

obrestiD <- data.table(t(obresti[c(4,20,26),]),keep.rownames = TRUE)






#3. Hipoteza pri?akovanj trga
#a)
izracun_terminske <- function(T,U,obrestT,obrestU){
  terminska = (1/(U-T))*(((1+U*obrestU)/(1+T*obrestT))-1)
  return (terminska)
}
