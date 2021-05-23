## [Análisis longitudinal de las encuestas Pulso Ciudadano] ##

#Carga de paquetes

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork")
ipak(packages)


# Carga de ambas bases

AbrilQ2 <-read_spss("Original Data/BBDD_Pulso Abril Q2_.sav")
MayoQ1 <-read_spss("Original Data/BBDD_Mayo_Q1.sav")

#Selección de variables a usar

AbrilQ2 <-select(AbrilQ2,SEXO,EDAD,RANGOEDAD,GSE_COD,P5,P8,P11_COD,P13_1,P14,P15,P29XPARTIDARIO,
                 P69,P156,P157,P162,P216,PONDERADOR)
MayoQ1 <-select(MayoQ1, SEXO,EDAD, RANGOEDAD,GSE_COD,P5,P8,P11_COD,P13_1,P14,P15,P29XPARTIDARIO,
                P69,P156,P157,P162,P236,P239,PONDERADOR)

#Crear variable de fecha en ambas bases

AbrilQ2$fecha <-c("Abril Q2")
MayoQ1$fecha <-c("Mayo Q1")

Pulso <-merge(AbrilQ2,MayoQ1, by = c("fecha","PONDERADOR", "SEXO","EDAD","RANGOEDAD","GSE_COD","P5","P8","P11_COD",
                                     "P13_1","P14","P15","P29XPARTIDARIO","P69","P156","P157", "P162"),all.x = T, all.y = T)

#Limpieza y recodificación de variables

#Recodificación de variables y nombres de variables

## Edad ##

Pulso$EDAD <-as.numeric(Pulso$EDAD)
class(Pulso$EDAD)
Pulso <- mutate(Pulso, Edadrec = car::recode(Pulso$EDAD, "17:21 = 1; 22:35 = 2;
                                                           36:50 = 3; 50:90 =4; else = NA"))
Pulso <- mutate(Pulso, Edadrec = recode(Pulso$Edadrec,"1" = "Centennials","2" = "Millennials",
                                        "3" = "Gen X", "4" = "Boomers"))


## Sexo ##

Pulso$SEXO <-as.numeric(Pulso$SEXO)

Pulso <-mutate(Pulso, SexoRecod = recode(Pulso$SEXO, "1" = "hombre", "2" = "mujer"))

## GSE ##

Pulso$GSE_COD <-as.numeric(Pulso$GSE_COD)

Pulso <-mutate(Pulso, GSERecod = recode(Pulso$GSE_COD, "1" = "C1", "2" = "C2", "3" = "C3",
                                        "4" = "D", "5" = "E"))


#Recodificación de variables

### [Cambio de nombres de etiquetas] ### ------------------------------------------


## Cambio de nombre a  Candidatos ##

Pulso$P11_COD <-as.numeric(Pulso$P11_COD)

Pulso <-mutate(Pulso, Candi = recode(Pulso$P11_COD, "21" = "Daniel Jadue", "36" = "Joaquín Lavín",
                                   "45" = "Pamela Jiles", "99" = "No sé"))

## Posición política ##

Pulso$P13_1 <-as.numeric(Pulso$P13_1)
Pulso <-mutate(Pulso, PosPol = recode(Pulso$P13_1, "1" = "Izquierda", "2" = "Centro Izquierda", "3" = "Centro",
                                    "4" = "Centro Derecha", "5" = "Derecha", "6" = "Sin posición política",
                                    "7" = "No sé"))

## Partidario u opositor

Pulso$P29XPARTIDARIO <-as.numeric(Pulso$P29XPARTIDARIO)

Pulso <-filter(Pulso, P29XPARTIDARIO%in% c(1,2,3))

Pulso<-mutate(Pulso, Parti = recode(Pulso$P29XPARTIDARIO, "1" = "Partidario", "2" = "Opositor", "3" = "Independiente"))

table(Pulso$Parti)

## Situación Laboral

Pulso$P69 <- as.numeric(Pulso$P69)

Pulso <-mutate(Pulso, Labor = recode(Pulso$P69, "1" = "Trabajo presencial", "2" = "Teletrabajo", "3" = "Cesante",
                                   "4" = "Sólo estudiando", "5" = "Jefe de Hogar", "6" = "Jubilado"))

Pulso$P162 <-as.numeric(Pulso$P162)

Pulso <-filter(Pulso, P162%in% c(1,2,3))
Pulso <-mutate(Pulso, Moni = recode(Pulso$P162, "1" = "No alcanza", "2" = "Alcanza justo", "3" = "Alcanza y sobra"))

table(Pulso$Moni)

## Niveles de felicidad ##

Pulso$P15 <-as.numeric(Pulso$P15)
Pulso <-mutate(Pulso,Felici = recode(Pulso$P15, "1" = "Nada Feliz", "2" = "Poco Feliz", "3" = "Medianamente feliz",
                                   "4" = "Feliz", "5" = "Muy Feliz"))

## ultimo guardado


##Fin y guardado de base"

saveRDS(Pulso, file = "Original Data/Pulso-total.RDS")

