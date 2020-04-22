setwd('C:/Users/82103/Desktop/purdue project/shiny/My_App')
#read data with .csv file
Small <- read.csv("Data/small/small1_02_16.csv", stringsAsFactors=F)
Median <- read.csv("Data/middle/middle4_02_16.csv", stringsAsFactors=F)
propTest <- read.csv("Data/CoolTerm Capture 2020-02-23 16-05-53_big04.csv", stringsAsFactors=F)
library(tidyverse)
library(fpp2)
Bigprop0Holt <- function() {
  big0 <- holt(propTest$prop0, alpha = .9, h = 50)
  big1 <- holt(propTest$prop1, alpha = .9, h = 50)
  big2 <- holt(propTest$prop2, alpha = .9, h = 50)
  autoplot(big1)
  return(big0)
}
BigCO0Holt <- function() {
  bigCO0 <- holt(propTest$CO0, alpha = .9, h = 50)
  bigCO1 <- holt(propTest$CO1, alpha = .9, h = 50)
  bigCO2 <- holt(propTest$CO2, alpha = .9, h = 50)
  autoplot(bigCO1)
  return(bigCO0)
}

Smallprop0Holt <- function() {
  small0 <- holt(Small$prop0, alpha = .9, h = 50)
  small1 <- holt(Small$prop1, alpha = .9, h = 50)
  small2 <- holt(Small$prop2, alpha = .9, h = 50)
  autoplot(small1)
  return(small0)
}

SmallCO0Holt <- function() {
  small0 <- ses(Small$CO0, alpha = .9, h = 50)
  small1 <- ses(Small$CO1, alpha = .9, h = 50)
  small2 <- ses(Small$CO2, alpha = .9, h = 50)
  autoplot(small1)
  return(small0)
}

Medianprop0Holt <- function() {
  small0 <- holt(Median$prop0, alpha = .9, h = 50)
  small1 <- holt(Median$prop1, alpha = .9, h = 50)
  small2 <- holt(Median$prop2, alpha = .9, h = 50)
  autoplot(small1)
  return(small0)
}

MedianCO0Holt <- function() {
  small0 <- holt(Median$CO0, alpha = .9, h = 50)
  small1 <- holt(Median$CO1, alpha = .9, h = 50)
  small2 <- holt(Median$CO2, alpha = .9, h = 50)
  autoplot(small1)
  return(small0)
}
