library(tidyverse)
library(lubridate)
library(xts)
library(qrmdata)
library(qrmtools)
library(psych)
library(gmodels)
library(MASS)
library(survival)
library(fitdistrplus)
library(lmtest)
library (fdth)
library(readxl)
library(ggplot2)
library(PASWR2)
library(lattice)
library(descr)
library(openxlsx)
library(PASWR2)

#dataPath <- "D:/5 - Quinto Semestre/Inferencia Estadistica/cambios-regreso/proyecto_it.xlsx"
dataPath <- "D:/workspace/cambios-regreso/proyecto_it.xlsx"
dataSheet <- "Datos"

#Tiempo de Desplazamiento a la Universidad
time_u <- read_excel(path = dataPath,
                     sheet = dataSheet,
                     range = "G2:G314",
                     col_types = "numeric",
                     col_names = "col")

boxplot(time_u$col)

my_mean = mean(time_u$col, na.rm = TRUE)

hypothesis_t(time_u$col,my_mean,"Hipotesis Tiempo de Desplazamiento a la Universidad")

#Gastos Virtualidad
expenses_v <- read_excel(path = dataPath,
                         sheet = dataSheet, 
                         range = "H2:H314",
                         col_types = "numeric",
                         col_names = "col")

boxplot(expenses_v$col)

boxplot.stats(expenses_v$col)$out

my_mean = mean(expenses_v$col, na.rm = TRUE)

expernses_v_wa <- replace(expenses_v, expenses_v >= 8000, my_mean)

boxplot(expernses_v_wa$col)

my_mean = mean(expernses_v_wa$col, na.rm = TRUE)

hypothesis_t(expernses_v_wa$col,my_mean,"Hipotesis Gastos Virtualidad")

#Gastos Presencialidad
expenses_p <- read_excel(path = dataPath,
                         sheet = dataSheet,
                         range = "I2:I314",
                         col_types = "numeric",
                         col_names = "col")
boxplot(expenses_p$col)
out_values <- boxplot.stats(expenses_p$col)$out

my_mean = mean(expenses_p$col, na.rm = TRUE)

expernses_p_wa <- replace(expenses_p, expenses_p >= 45000, my_mean)

boxplot(expernses_p_wa$col)

my_mean = mean(expernses_p_wa$col, na.rm = TRUE)
my_mean

hypothesis_t(expernses_p_wa$col,my_mean,"Hipotesis Gastos Presencialidad")

#Tiempo Dedicado a Hobbies en la Virtualidad
hobbies_v <- read_excel(path = dataPath,
                        sheet = dataSheet,
                        range = "L2:L314",
                        col_types = "numeric",
                        col_names = "col")

boxplot(hobbies_v$col)
boxplot.stats(hobbies_v$col)$out

my_mean = mean(hobbies_v$col, na.rm = TRUE)

hobbies_v_wa <- replace(hobbies_v, hobbies_v >= 220, my_mean)

boxplot(hobbies_v_wa$col)

my_mean = mean(hobbies_v_wa$col, na.rm = TRUE)

hypothesis_t(hobbies_v_wa$col,my_mean,"Hipotesis Tiempo Hobbies Virtualidad")

#Tiempo dedicado a Hobbies en la Presencialidad
hobbies_p <- read_excel(path = dataPath,
                        sheet = dataSheet,
                        range = "M2:M314",
                        col_types = "numeric",
                        col_names = "col")

boxplot(hobbies_p$col)
out_values = boxplot.stats(hobbies_p$col)$out

my_mean <- mean(hobbies_p$col, na.rm = TRUE)
my_mean

hobbies_p$col <- remove_atypical(hobbies_p$col, out_values, my_mean)

my_mean <- mean(hobbies_p$col, na.rm = TRUE)
my_mean

boxplot(hobbies_p$col)

hypothesis_t(hobbies_p$col,my_mean,"Hipotesis Tiempo Hobbies Presencialidad")

#Functions
remove_atypical <- function(explore, out_values, mu) {
  i = 1
  n = length(explore)
  while(i <= n)
  {
    if(explore[i] %in% out_values){
      explore <- replace(explore, i, mu)
    }
    i = i +1
  }
  return(explore)
}

hypothesis_t <- function(explore, mean, hypothesis) {
  my_t <- t.test(hobbies_p$col, mu = mean )
  my_t
  p_value <- my_t$p.value
  paste(hypothesis)#Hipotesis Tiempo Hobbies Presencialidad
  if(p_value <= 0.05) {
    paste("Rechazar H0: media distinta de  ", my_mean)
  }else {
    paste("No rechazar H0: media igual a ", my_mean)
  }
}


