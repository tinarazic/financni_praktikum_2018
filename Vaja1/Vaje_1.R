# FINANčNI PRAKTIKUM -  Vaja 1

# IZBRANO OBDOBJE: 2011 - 2013, 6m x 12m

# 1. Uvoz podatkov in dinamika obrestnih mer v času

# a)uvoz
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

hist_EURIBOR_2011 <- read_csv("hist_EURIBOR_2011.csv")
hist_EURIBOR_2012 <- read_csv("hist_EURIBOR_2012.csv")
hist_EURIBOR_2013 <- read_csv("hist_EURIBOR_2013.csv")

# b)

# izbira stolpcev
obresti_2011 <- hist_EURIBOR_2011[,c(1,2,23,43,66,85,107,129,150,173,195,216,238)]
obresti_2012 <- hist_EURIBOR_2012[,c(1,2,24,45,67,86,108,129,151,174,194,217,239)]
obresti_2013 <- hist_EURIBOR_2013[,c(1,2,24,44,64,85,107,127,150,172,193,216,237)]

# zdruzimo tabelo v eno
obresti <- cbind(obresti_2011,obresti_2012,obresti_2013)

# odstranimo nepotrebna stolpca
obresti <- obresti[,-c(14,27)]

# nastavimo imena vrstic
obresti1 <- obresti[,-1]
rownames(obresti1) <- obresti[,1]
obresti <- obresti1

# transponiramo tabelo
obresti <-t(obresti)

# c) narišemo graf

# naredimo 2 časovni vrsti, T1 za 6 mesecev, U1 za 12 mesecev

vrsta1 <- ts(data = obresti[,9],start = c(2011,1),frequency = 12)
vrsta2 <- ts(data = obresti[,15],start = c(2011,1),frequency = 12)

#narišemo graf
library(graphics)

grafEURIBOR <- ts.plot(vrsta1,vrsta2,
                       xlab = "Time",
                       ylab = "%", 
                       main = "EURIBOR", 
                       col = c("deeppink2","cyan"))
legend("topright",legend=c("6 mesecev", "12 mesecev"),col=c("deeppink2", "cyan"),lty=1)


# 2. Oblika obrestne krivulje 

# a) izbira datumov:01/04/2011  & 01/08/2012  & 01/02/2013 (4,20,26)

# b)

# izberemo  stolpce, ki predstavljajo izbrane datume
obrestiD <- data.frame(obresti1[,c(4,20, 26)])

# defeniramo x os
dospetja = c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,11,12)

obrestiD <- cbind(obrestiD,dospetja)

# narišemo graf
grafD <- plot(y = obrestiD[,1],
              x = obrestiD$dospetja,
              ylim=c(min(0),max(2.1)),
              xlab="Dospetje [mesec]", 
              ylab="%", 
              main="Časovna struktura Euribor")
lines(obrestiD[,1], x = dospetja,col = "dodgerblue1", type ="o", text(11.5,1.7,"1.4.2011", col="dodgerblue1"))
lines(obrestiD[,2], x = dospetja,col = "darkorange", type ="o", text(11.5,1.1,"1.8.2012", col="darkorange"))
lines(obrestiD[,3], x = dospetja,col = "green", type = "o", text(11.5,0.4,"1.2.2013", col="green"))

# OPIS OBRESTNIH KRIVULJ: Oblika vseh treh krivulj prikazanih na grafu je normalna.
# Krivulje kažejo na stabilno obdobje z inflacijo, kar povečuje višino obrestne mere, glede na dospelost.
# Prva in zadnja krivulja sta konkavni, druga pa je na začetku konveksna potem pa se nadaljuje v konkavno 
# obliko.
# Zanimivo je tudi videti, da se krivulji za leto 2012 in 2013 prekrivata. Lahko rečemo, da učinek svetovne
# krize iz preteklih letih upada in se obrestne mere vračajo v stabilne smernice, kjer ni presenetljivih
# nihanj.


#3. Hipoteza pričakovanj trga

#a) & b) izračun terminskih obrestnih mer

#vzamemo samo stolpa z dospetjem 6 mesecev in 12 mesecev
terminska <- obresti[,c(9,15)]

#izračunamo vse možne terminske obrestne mere
izracun <- (1/(12-6))*(((1+12*0.01*terminska[,2])/(1+6*0.01*terminska[,1]))-1)*100

#združimo v eno tabelo
terminska <- as.data.frame(cbind(terminska,izracun))
terminska[,3] <- c(c(NA, NA, NA, NA, NA, NA), izracun[-c(31:36)])

colnames(terminska) <- c("Euribor6m","Euribor12m","Napoved6m")

# c) razsevni grafikon, linearna regersija za sva leta skupaj

leto <- as.vector(cbind(seq(2011, 2011, length.out = 12), seq(2012, 2012, length.out = 12), seq(2013, 2013, length.out = 12))) %>%
  as.factor()
terminska <- as.data.frame(cbind(terminska,leto))

terminska1 <- as.data.frame(terminska[-c(1:6),])

# graf

g.razsevni <- ggplot(terminska1,aes(x = terminska1$'Napoved6m', y = terminska1$'Euribor6m')) +
  geom_point(aes(colour = terminska1$'leto'), size = 2) +
  geom_smooth(method='lm', se = FALSE, color = "darkgray") +
  geom_abline(slope=1, intercept= 0) +
  coord_cartesian(xlim=c(0,2.4),ylim=c(0,2.4)) + 
  labs(title ='6m Euribor 2011 - 2013', y='Opazovano', x = 'Napoved', color = "Leto:") +
  theme_classic()

print(g.razsevni)

# graf leto 2011

terminska2011 <- terminska1[c(1:6),]

g.leto2011 <- ggplot(terminska2011,aes(x = terminska2011$'Napoved6m', y = terminska2011$'Euribor6m')) +
  geom_point(colour = "darkorange", size = 2) +
  geom_abline() +
  geom_smooth(method ="lm",se = FALSE, color = "darkgray") +
  coord_cartesian(xlim=c(1.6,2.4),ylim=c(1.6,2.4)) +
  labs(title ='6m Euribor 2011', y='Opazovano', x = 'Napoved') +
  theme_classic()

print(g.leto2011)

# graf leto 2012

terminska2012 <- terminska1[c(7:18),]

g.leto2012 <- ggplot(terminska2012,aes(x = terminska2012$'Napoved6m', y = terminska2012$'Euribor6m')) +
  geom_point(colour = "green", size = 2) +
  geom_abline() +
  geom_smooth(method ="lm", se = FALSE, color = "darkgray") +
  coord_cartesian(xlim=c(0.3,2.3),ylim=c(0.3,2.3)) +
  labs(title ='6m Euribor 2012', y='Opazovano', x = 'Napoved') +
  theme_classic()

print(g.leto2012)

# graf leto 2013

terminska2013 <- terminska1[c(19:30),]

g.leto2013 <- ggplot(terminska2013,aes(x = terminska2013$'Napoved6m', y = terminska2013$'Euribor6m')) +
  geom_point(colour = "dodgerblue1", size = 2) +
  geom_abline() +
  geom_smooth(method ="lm",se = FALSE, color = "darkgray") +
  coord_cartesian(xlim=c(0.3,1.45),ylim=c(0.3,1.45)) +
  labs(title ='6m Euribor 2013', y='Opazovano', x = 'Napoved') +
  theme_classic()

print(g.leto2013)

# d) Hipotezo pričakovanj trga glede na naše podatke in grafične prikaze ne moremo potrditi.
#    Če bi želeli hipotezo potrditi, bi morale točke na grafih ležati na simetrali lihih kvadrantov 
#    oziroma v njeni bližini in posledično, bi se tudi regersijska premica bolj prilegala simetrali.
#    V prevodu na obrestne mere, bi potem lahko rekli, da smo s terminsko obrestno 
#    mero L(0,T,U) uspešno napovedali obrestno mero L(T,U).
#    Večina pik na grafu "6m Euribor 2011 -2013" leži pod simetralo lihih kvadrantov, saj so naše napovedi
#    velikokrat bile večje od opazovanih obrestnih mer.
#    Na skupnem grafu vidimo linerano korelacijo med opazovano in napovedano obrestno mero. Če pa pogledamo
#    grafe za posamezna leta, pa temu ni vedno tako. Že na grafu za leto 2011 vidimo negativno korelacijo,
#    saj se napovedane vrednosti večajo, dejanske vrednosti pa padajo. 
#    V letu 2012 so opazovane obrestne mere drastično padle in tudi naše napovedi začnejo padati. To 
#    pozitivno korelacijo lahko potrdimo z našo regresijsko premico na grafu.
#    V letu 2013 pa vidimo skoraj konstantno regresijsko premico, saj niti dejanske obrestne mere 
#    niti naše napovedi niso tako nihale. Mogoče bi lahko rekli, da se je po krizi gospodarstvo malo 
#    umirilo in stabiliziralo. Še vedno pa lahko vidimo veliko razliko med opazovano in napovedano 
#    vrednostjo obrestne mere. 

###############################################################################################################




