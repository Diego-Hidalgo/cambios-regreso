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

dataPath <- "D:/5 - Quinto Semestre/Inferencia Estadistica/cambios-regreso/proyecto_it.xlsx"
#dataPath <- "D:/workspace/cambios-regreso/proyecto_it.xlsx"
dataSheet <- "Datos"

#Tiempo de Desplazamiento a la Universidad
time_u <- read_excel(path = dataPath,
                     sheet = dataSheet,
                     range = "G2:G314",
                     col_types = "numeric",
                     col_names = "col")

boxplot(time_u$col)

my_mean = mean(time_u$col, na.rm = TRUE)

my_t <- t.test(time_u$col, mu = my_mean)
my_t

p_value <- my_t$p.value

paste("Hipotesis Tiempo de Desplazamiento a la Universidad")
if(p_value <= 0.05) {
  paste("Rechazar H0: media distinta de ", my_mean)
}else {
  paste("No rechazar H0: media igual a ", my_mean)
}

#Gastos Virtualidad
expenses_v <- read_excel(path = dataPath,
                         sheet = dataSheet, 
                         range = "H2:H314",
                         col_types = "numeric",
                         col_names = "col")

boxplot(expenses_v$col)

boxplot.stats(expenses_v$col)$out

my_mean = mean(expenses_v$col, na.rm = TRUE)

my_t <- t.test(expenses_v$col, mu = my_mean)
my_t

p_value <- my_t$p.value

paste("Hipotesis Gastos Virtualidad")
if(p_value <= 0.05) {
  paste("Rechazar H0: media distinta de ", my_mean)
}else {
  paste("No rechazar H0: media igual a ", my_mean)
}

#Gastos Presencialidad
expenses_p <- read_excel(path = dataPath,
                         sheet = dataSheet,
                         range = "I2:I314",
                         col_types = "numeric",
                         col_names = "col")

my_mean = mean(expense_p$col, na.rm = TRUE)

my_t <- t.test(expenses_p$col, mu = my_mean)
my_t

p_value <- my_t$p.value

paste("Hipotesis Gastos Presencialidad")
if(p_value <= 0.05) {
  paste("Rechazar H0: media distinta de  ", my_mean)
}else {
  paste("No rechazar H0: media igual a ", my_mean)
}

#Tiempo Dedicado a Hobbies en la Virtualidad
hobbies_v <- read_excel(path = dataPath,
                        sheet = dataSheet,
                        range = "L2:L314",
                        col_types = "numeric",
                        col_names = "col")

my_mean = mean(hobbies_v$col, na.rm = TRUE)

my_t <- t.test(hobbies_v$col, mu = my_mean)
my_t

p_value <- my_t$p.value

paste("Hipotesis Tiempo Hobbies Virtualidad")
if(p_value <= 0.05) {
  paste("Rechazar H0: media distinta de  ", my_mean)
}else {
  paste("No rechazar H0: media igual a  ", my_mean)
}

#Tiempo dedicado a Hobbies en la Presencialidad
hobbies_p <- read_excel(path = dataPath,
                        sheet = dataSheet,
                        range = "M2:M314",
                        col_types = "numeric",
                        col_names = "col")

my_mean <- mean(hobbies_p$col, na.rm = TRUE)

my_t <- t.test(hobbies_p$col, mu = my_mean)
my_t

p_value <- my_t$p.value

paste("Hipotesis Tiempo Hobbies Presencialidad")
if(p_value <= 0.05) {
  paste("Rechazar H0: media distinta de  ", my_mean)
}else {
  paste("No rechazar H0: media igual a ", my_mean)
}



