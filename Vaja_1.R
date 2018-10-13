# FINAN?NI PRAKTIKUM - vaja 1
# obdobje 2011 - 2013, 3 x 6 
# 1. Uvoz podatkov in dinamika obrestnih mer v ?asu
#a)uvoz

library(readr)
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

# - združimo tabele v eno
obresti <- rbind(obresti_2011,obresti_2012,obresti_2013)

#c) narišemo graf
#naredimo 2 časovni vrsti, T1 za 6 mesecev, U1 za 12 mesecev

T1 <- ts(data = obresti[,9],start = c(2011,1),frequency = 12)
U1 <- ts(data = obresti[,15],start = c(2011,1),frequency = 12)

#narišemo graf
require(graphics)

ts.plot(T1,U1,xlab = "Time",ylab = "%", main = "EURIBOR", col = c("darksalmon","cornflowerblue"))
legend("topright",legend=c("6 mesecev", "12 mesecev"),col=c("darksalmon", "cornflowerblue"),lty=1)

