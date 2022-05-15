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
library(qrmdata)
library(qrmtools)
library(readxl)
library(xts)
library(lattice)
library (fdth)
library(descr)
library(MASS)
library(gmodels)
library(lmtest)
library(DescTools)

#dataPath <- "D:/5 - Quinto Semestre/Inferencia Estadistica/cambios-regreso/proyecto_it.xlsx"
dataPath <- "D:/workspace/cambios-regreso/proyecto_it.xlsx"
dataSheet <- "Datos"

#Semestre
semester <- read_excel(path = dataPath,
                       sheet = dataSheet,
                       range = "C2:C314",
                       col_types = "text",
                       col_names = "col")

#Tiempo de Desplazamiento a la Universidad
motivation_v <- read_excel(path = dataPath,
                           sheet = dataSheet,
                           range = "J2:J314",
                           col_types = "text",
                           col_names = "col")



time_u <- read_excel(path = dataPath,
                     sheet = dataSheet,
                     range = "G2:G314",
                     col_types = "numeric",
                     col_names = "col")

boxplot(time_u$col)

my_mean = mean(time_u$col, na.rm = TRUE)

hypothesis_t(time_u$col, my_mean,"Hipotesis Tiempo de Desplazamiento a la Universidad")

#Gastos Virtualidad
expenses_v <- read_excel(path = dataPath,
                         sheet = dataSheet, 
                         range = "H2:H314",
                         col_types = "numeric",
                         col_names = "col")

boxplot(expenses_v$col)

boxplot.stats(expenses_v$col)$out

my_mean = mean(expenses_v$col, na.rm = TRUE)

expenses_v_wa <- replace(expenses_v, expenses_v >= 8000, my_mean)

boxplot(expenses_v_wa$col)

my_mean = mean(expenses_v_wa$col, na.rm = TRUE)

hypothesis_t(expenses_v_wa$col,my_mean,"Hipotesis Gastos Virtualidad")

#Gastos Presencialidad
expenses_p <- read_excel(path = dataPath,
                         sheet = dataSheet,
                         range = "I2:I314",
                         col_types = "numeric",
                         col_names = "col")
boxplot(expenses_p$col)
out_values <- boxplot.stats(expenses_p$col)$out

my_mean = mean(expenses_p$col, na.rm = TRUE)

expenses_p_wa <- replace(expenses_p, expenses_p >= 45000, my_mean)

boxplot(expenses_p_wa$col)

my_mean = mean(expenses_p_wa$col, na.rm = TRUE)
my_mean

hypothesis_t(expenses_p_wa$col,my_mean,"Hipotesis Gastos Presencialidad")

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

#Prueba Correlación Gastos Presencialidad - Tiempo Desplazamiento
cor.test(expenses_p_wa$col, time_u$col)
regression <- lm(expenses_p_wa$col~time_u$col)
summary(regression)
dwtest(regression)
residue <- residuals(regression)
shapiro.test(residue)

hypothesis_t(hobbies_p$col,my_mean,"Hipotesis Tiempo Hobbies Presencialidad")

#Regression
plot(time_u$col,hobbies_p$col)
cor.test(expernses_p_wa$col, time_u$col)
Regresion <- lm(expernses_p_wa$col~time_u$col)
summary(Regresion)
dwtest(Regresion)

#Hipotesis para la proporción
#H0: p<= 0,90 los estudiantes tardan menos de 72 minutos en desplazarse a la universidad
#H1: p > 0,90 los estudiantes tardan más de 72 minutos en desplazarse a la universidad
x <-length(time_u$col[time_u$col<= 72])
n <- length(time_u$col)
p <- 0.90
prop.test(x,n,p, alternative = "greater", conf.level = 0.95, correct = FALSE)
#Chi cuadrado G

facultad_raw <- read_excel(path = dataPath,
                        sheet = dataSheet,
                        range = "B2:B314",
                        col_types = "text",
                        col_names = "col")

motivacion_raw <- read_excel(path = dataPath,
                       sheet = dataSheet,
                       range = "J2:J314",
                       col_types = "text",
                       col_names = "col")

facultad <- factor(facultad_raw$col, labels = c("CIENCIAS ADMINISTRATIVAS Y ECONÓMICAS",
                                                "CIENCIAS DE LA SALUD","CIENCIAS NATURALES",
                                                "DERECHO Y CIENCIAS SOCIALES","ESCUELA DE CIENCIAS DE LA EDUCACIÓN",
                                                "INGENIERÍA"))
barchart(facultad)
motivacion <- factor(motivacion_raw$col, labels = c("Indiferente","	Ligeramente motivado",
                                                    "Muy motivado","Nada motivado","Poco motivado"))
barchart(motivacion)

tabla.1 <- freq(facultad, plot = TRUE)
tabla.2 <- freq(motivacion, plot = TRUE)
table.3 <- crosstab(facultad)

chi_square(facultad_raw$col,motivacion_raw$col)

semestre_raw <- read_excel(path = dataPath,
                           sheet = dataSheet,
                           range = "C2:C314",
                           col_types = "text",
                           col_names = "col")

motivacion_raw <- read_excel(path = dataPath,
                             sheet = dataSheet,
                             range = "J2:J314",
                             col_types = "text",
                             col_names = "col")

chi_square(semestre_raw$col,motivacion_raw$col)

#ANOVA Varias medias, Semestre - gastos virtualidad.
FSemester <- as.factor(semester$col)

tapply(expenses_v_wa$col, FSemester, mean)

anova <- aov(lm(expenses_v_wa$col~FSemester))

summary(aov)

TukeyHSD(anova)

#Gastos Virtualidad - Presencialidad
paired_means_comparison("Gastos Virtualidad", "Gastos Presencialidad", expenses_v_wa$col, expenses_p_wa$col)

#Hobbies Virtualidad - Presencialidad
paired_means_comparison("Hobbies Virtualidad", "Hobbies Presencialidad", hobbies_v_wa$col, hobbies_p$col)

#Functions

#Removes the given atypical values from a given list
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

#Performs the t hypothesis on a given list with a given mean
hypothesis_t <- function(explore, mean, hypothesis) {
  my_t <- t.test(explore, mu = mean )
  print(my_t)
  p_value <- my_t$p.value
  paste(hypothesis)
  if(p_value <= 0.05) {
    paste("Rechazar H0: media distinta de  ", my_mean)
  }else {
    paste("No rechazar H0: media igual a ", my_mean)
  }
}
#Verify if two variables have a relationship between of them
chi_square <- function(x,y){
  paste("H0: No existe una relación entre la facultad y la moticación para estudiar en presencialidad")
  paste("H1: Si existe una relación")
  ch <- chisq.test(x,y, correct = TRUE, p = rep(1/length(x),leng(y)))
  print(ch)
  p_value <- ch$p.value
  if(p_value <= 0.05){
    paste("Se rechaza la hipotesis nula, si existe una relación entre las variables")
  }else{
    paste("No se rechaza la hipotesis nula, no existe una relación entre las variables")
  }
}

paired_means_comparison <- function(x_text, y_text, x, y) {
  my_int <- t.test(x, y, paired = TRUE)$conf.int
  if(my_int[1] <= 0 & my_int[2] >= 0) {
    print("Las medias son iguales")
  } else if(my_int[1] < 0) {
    paste("La media de ", y_text, " es mayor")
  } else {
    paste("La media de ", x_text, " es mayor")
  }
}

