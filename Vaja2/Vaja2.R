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

# a) izbira vzorca: vzorec 1

vzorec <- read_csv("Vaja2/vzorec1.txt", col_names = c("odskodnine"))

histogram <- hist(vzorec$odskodnine,xlab = "Višina odškodnine", ylab = "Frekvenca",main = "Histogram odškodnin")

# b) histogram kaže na Paretovo porazdelitev

mde(vzorec, pareto1,measure = "CvM")