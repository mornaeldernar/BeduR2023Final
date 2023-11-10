
install.packages("dplyr")
library(dplyr)
library(stringr)
library(ggplot2)

setwd("/cloud/project/2023")
archivo<-"Number of foreign visitors to Japan by month_ .csv"
df <- read.csv(archivo)
df <- data.frame(df)
df <- rename(df,  Pais=Country.Area, 
                 Mes=Month, 
                 Anio=Year,
                 Visitantes=Visitor)
df <- df %>% 
  mutate(Mes = str_replace(df$Mes, "[.]","")) 
df <- df %>%
  mutate(Mes = as.integer(factor(df$Mes, levels = month.abb)))  
df <- df %>%
  mutate(M0 =  ifelse(df$Mes < 10 , str_replace(paste(0, df$Mes),"[ ]",""), df$Mes)) 
df <- df %>%
  mutate(Periodo = paste(df$Anio, sep="-", df$M0,"01"))
df <- df %>%
  mutate(Periodo = as.Date(df$Periodo))
dfMex <- df[df$Pais == 'Mexico',]
summary(dfMex)

hist(dfMex$Visitantes, prob = T, ylab = "", xlab = "", main = "")
points(dfMex$Visitantes, dnorm(dfMex$Visitantes), type = "l")
japan_mex_timeseries <- ts(dfMex$Visitantes,start=c(2017,1),end=c(2023,12),frequency=12)
plot(japan_mex_timeseries)
japan_mex_timeseries.decomp <- decompose(japan_mex_timeseries, type = "mult")
japan_mex_timeseries.decomp
plot(japan_mex_timeseries.decomp, xlab = "Tiempo", 
     sub = "Descomposición de los datos de producción de electricidad")
acf(japan_mex_timeseries, main="")
