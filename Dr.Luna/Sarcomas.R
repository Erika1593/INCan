library(ggplot2)
library(tidyverse)
library(ggpubr)
library(installr)
library(AICcmodavg)
library(skimr)
library(gridExtra)
library(corrplot)
library(car)
library(scatterplot3d)
library(dplyr)
library(scales)
library(plotrix)


##Lectura de base de datos
SARCOMAS <- read.csv("~/Rstudio_jobs/Dr.Luna/Sarcoma_V3.csv", stringsAsFactors = FALSE) ##Mantener el orden de las filas

##Modificación de formato a fecha para lectura
SARCOMAS$Fecha_Fallecimiento <- as.Date(SARCOMAS$Fecha_Fallecimiento, format="%d/%m/%Y")
SARCOMAS$FECHA_INICIO_SINTOMAS <- as.Date(SARCOMAS$FECHA_INICIO_SINTOMAS, format="%d/%m/%Y")
SARCOMAS$FechaUltima_consulta <- as.Date(SARCOMAS$FechaUltima_consulta, format="%d/%m/%Y")
SARCOMAS$FECHA_TRATAMIENTO <- as.Date(SARCOMAS$FECHA_TRATAMIENTO, format="%d/%m/%Y")
SARCOMAS$Fecha_fin_Tx <- as.Date(SARCOMAS$Fecha_fin_Tx, format="%d/%m/%Y")


# Evento SG (fallecidos 1, no fallecidos 0)
SARCOMAS$Evento_SG <- ifelse(!is.na(SARCOMAS$Fecha_Fallecimiento), 1, 0)

##Excluir sujetos con fechas desfasadas 
SARCOMAS_SG <- SARCOMAS[!is.na(SARCOMAS$Tiempo_SG) & SARCOMAS$Tiempo_SG >= 0, ]

# Tiempo SG 
SARCOMAS_SG$Tiempo_SG <- ifelse(
  SARCOMAS_SG$Evento_SG == 1 & !is.na(SARCOMAS_SG$Fecha_Fallecimiento),
  as.numeric(SARCOMAS_SG$Fecha_Fallecimiento - SARCOMAS_SG$FECHA_TRATAMIENTO),
  ifelse(
    !is.na(SARCOMAS_SG$FechaUltima_consulta),
    as.numeric(SARCOMAS_SG$FechaUltima_consulta - SARCOMAS_SG$FECHA_TRATAMIENTO),
    NA
  )
)


##Cálculo SG en meses
SARCOMAS_SG$Tiempo_SG_meses <- SARCOMAS_SG$Tiempo_SG / 30.44

##Cálculo SG en años 
SARCOMAS_SG$Tiempo_SG_años <- SARCOMAS_SG$Tiempo_SG / 365.25


SG <- Surv(SARCOMAS_SG$Tiempo_SG_meses, SARCOMAS_SG$Evento_SG)  
T_SG <- survfit(SG ~ 1, data = SARCOMAS_SG, type = "kaplan-meier") 
summary(T_SG)


summary(SARCOMAS_SG$Tiempo_SG)     
table(SARCOMAS_SG$Evento_SG, useNA="ifany")

#Gráfica SG

SGp <- ggsurvplot(fit = T_SG, data = SARCOMAS, fun = "pct", conf.int = F, palette = c("#1F4E79"), size = 1.2, #conf.int.fill = c("#BFC9CA"), 
                  title = "SG",
                  break.time.by = 5, xlim = c(0, 36), ylim = c(0,100), mark.time = T,
                  xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                  ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"), 
                  legend.labs = c("SG")) 

SGp$plot



###____________________________________________##

## SLE ## 

##Modificación de formato a fecha para lectura
SARCOMAS$Fecha_recurrencia <- as.Date(SARCOMAS$Fecha_recurrencia, format="%d/%m/%Y")
SARCOMAS$Fecha_fin_Tx <- as.Date(SARCOMAS$Fecha_fin_Tx, format="%d/%m/%Y")

# Evento SLE
SARCOMAS$Evento_SLE <- ifelse(
  !is.na(SARCOMAS$Fecha_recurrencia) | !is.na(SARCOMAS$Fecha_Fallecimiento),
  1,
  0
)

# Primer evento entre recurrencia y muerte
SARCOMAS$Fecha_evento_SLE <- pmin(
  SARCOMAS$Fecha_recurrencia,
  SARCOMAS$Fecha_Fallecimiento,
  na.rm = TRUE
)

# Corregir pacientes sin evento
SARCOMAS$Fecha_evento_SLE[is.infinite(SARCOMAS$Fecha_evento_SLE)] <- NA

##Exclusión de datos con ffechas incongruentes 
SARCOMAS_SLE <- SARCOMAS_SLE[SARCOMAS_SLE$Tiempo_SLE_meses > 0, ]
summary(SARCOMAS_SLE$Tiempo_SLE_meses)

# Tiempo SLE en días
SARCOMAS$Tiempo_SLE <- ifelse(
  SARCOMAS$Evento_SLE == 1,
  as.numeric(SARCOMAS$Fecha_evento_SLE - SARCOMAS$Fecha_fin_Tx),
  as.numeric(SARCOMAS$FechaUltima_consulta - SARCOMAS$Fecha_fin_Tx)
)

# Convertir a meses
SARCOMAS$Tiempo_SLE_meses <- SARCOMAS$Tiempo_SLE / 30.44
##Cálculo SLE en años 
SARCOMAS$Tiempo_SLE_años <- SARCOMAS$Tiempo_SLE / 365.25

##Limpieza
SARCOMAS_SLE <- SARCOMAS[
  !is.na(SARCOMAS$Tiempo_SLE_meses) &
    SARCOMAS$Tiempo_SLE_meses >= 0,
]

summary(SARCOMAS_SLE$Tiempo_SLE_meses)
table(SARCOMAS_SLE$Evento_SLE)

##KM

SLE <- Surv(
  SARCOMAS_SLE$Tiempo_SLE_meses,
  SARCOMAS_SLE$Evento_SLE
)

T_SLE <- survfit(
  SLE ~ 1,
  data = SARCOMAS_SLE,
  type = "kaplan-meier"
)

summary(T_SLE)

summary(T_SLE, times = c(12, 36, 60))

##PLOT SLE
SLEp <- ggsurvplot(fit = T_SLE, data = SARCOMAS_SLE, fun = "pct", conf.int = F, palette = c("#256314"), size = 1.2, #conf.int.fill = c("#BFC9CA"), 
                   title = "SLE", 
                   break.time.by = 5, xlim = c(0,30), ylim = c(0,100), 
                   xlab = "Meses", ylab = "Supervivencia (%)", legend.title = "Estimación", 
                   ggtheme = theme_classic(base_size = 14, base_family = "Helvetica"), 
                   legend.labs = c("SLE")) 

##ver gráfica
SLEp$plot


#______________
