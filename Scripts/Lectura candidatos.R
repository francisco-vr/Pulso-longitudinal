##Comparativa de los tres  candidatos más importantes (Jiles, Jadue y Lavin) y del sector indeciso ##

#Creación de la BBDD con la variable

Pulso <-filter(Pulso, P11_COD%in% c(21,36,45,99))


saveRDS(Pulso, file = "Original Data/BBDD_Pulso.rds")


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

saveRDS(Pulso, file = "Original Data/BBDD_Pulso.rds")


# Caracterización socioeconómica de los cuatro grupos -----------------


##Edades ##

Pulso$Edadrec <-factor(Pulso$Edadrec, levels = c("Centennials", "Millennials", "Gen X", "Boomers"))

EdadPlot <-ggplot(data = subset(Pulso, !is.na(Edadrec)),
                  aes(x = factor(Edadrec),
                      y = prop.table(stat(count)),
                      weight = PONDERADOR,
                      fill = factor(fecha),
                      label = scales::percent(prop.table(stat(count)),1))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Grupo etáreo', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_grid(~Candi)
plot(EdadPlot)

ggsave(EdadPlot, filename = "Results/EdadPlot.png",
       dpi = 400, width = 15, height = 7)


##Género ##

SexoPlot <-ggplot(data = subset(Pulso, !is.na(Candi)),
                  aes(x = factor(Candi),
                      y = prop.table(stat(count)),
                      weight = PONDERADOR,
                      fill = factor(SexoRecod),
                      label = scales::percent(prop.table(stat(count)),1))) +
  geom_bar(position = "dodge") + 
  labs(title = "Sexo de votantes",
       x = "Sexo", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril y Primera Quicena Mayo, 2021") +
  xlab("Sexo") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Sexo', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~fecha)
plot(SexoPlot)

ggsave(SexoPlot, filename = "Results/SexoPlot.png",
       dpi = 400, width = 10, height = 7)

#GSE ##

Pulso$GSERecod <-factor(Pulso$GSERecod, levels = c("C1", "C2", "C3", "D", "E"))

GSEPlot <-ggplot(data = subset(Pulso, !is.na(GSERecod)),
                 aes(x = factor(GSERecod),
                     y = prop.table(stat(count)),
                     weight = PONDERADOR,
                     fill = factor(Candi),
                     label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") + 
  labs(title = "GSE de votantes",
       x = "GSE", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("GSE") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'GSE', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_grid(~Candi)
plot(GSEPlot)

ggsave(GSEPlot, filename = "Results/GSEPlot.png",
       dpi = 400, width = 15, height = 7)


# Características laborales -----------------------------------------------

LaborPlot <-ggplot(data = subset(Pulso, !is.na(Candi)),
                   aes(x = factor(Candi),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Labor),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") + 
  labs(title = "Situación laboral según candidato",
       x = "GSE", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Candidato") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Candidato', fill = 'Situación Laboral') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Labor, nrow = 2)
plot(LaborPlot)

ggsave(LaborPlot, filename = "Results/LaborPlot.png",
       dpi = 400, width = 12, height = 7)


# Dinero

MoniPlot <-ggplot(data = subset(Pulso, !is.na(Candi)),
                  aes(x = factor(Candi),
                      y = prop.table(stat(count)),
                      weight = PONDERADOR,
                      fill = factor(Moni),
                      label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "¿Alcanza el dinero que gana hasta fin de mes?",
       x = "GSE", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Candidatos', fill = 'Suficiencia de renta') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(MoniPlot)

ggsave(MoniPlot, filename = "Results/MoniPlot.png",
       dpi = 400, width = 10, height = 7)



# Posición política de los votantes ---------------------------------------

Pulso$PosPol <-factor(Pulso$PosPol, levels = c("Izquierda", "Centro Izquierda", "Centro", "Centro Derecha", 
                                               "Derecha", "Sin posición política", "No sé"))

PolitPlot <-ggplot(data = subset(Pulso, !is.na(PosPol)),
                   aes(x = factor(PosPol),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Candi),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Posición política, según Pulso de candidatos",
       x = "Posición Política", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Posición política', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi)
plot(PolitPlot)

ggsave(PolitPlot, filename = "Results/PolitPlot.png",
       dpi = 400, width = 15, height = 7)

## Oposición o no a piñera


PartiPlot <-ggplot(data = subset(Pulso, !is.na(Candi)),
                   aes(x = factor(Candi),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Parti),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Posición ante el Gobierno de Sebatían Piñera, según candidatos al que votaría",
       x = "Posición Política", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Candidatos', fill = 'Posición ante el Gobierno') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(PartiPlot)

ggsave(PartiPlot, filename = "Results/PartiPlot.png",
       dpi = 400, width = 12, height = 7)



# Problemas de chile ------------------------------------------------------

#Principal problema

#Buena info. hay que depurar: sólo dejar los más importantes (5 primeros) y que se vean las barras.

Problem1Plot <-ggplot(data = subset(Pulso, !is.na(P10_1)),
                      aes(x = factor(P10_1),
                          y = prop.table(stat(count)),
                          weight = PONDERADOR,
                          fill = factor(Candi),
                          label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Problema principal del País, según Pulso",
       x = "Problema Principal", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Problema principal") + ylab("Porcentaje") +
  scale_fill_manual("Candidato",
                    values = c("#FF6666", "#00CC66", "#CC0000", "#FF9999"),
                    labels = c("Daniel Jadue", "Joaquin lavin", "Pamela Jiles", "No sé")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Problema principal', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(Problem1Plot)

ggsave(Problem1Plot, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)

# Segundo más importante
Problem2Plot <-ggplot(data = subset(Pulso, !is.na(P10_1)),
                      aes(x = factor(P10_1),
                          y = prop.table(stat(count)),
                          weight = PONDERADOR,
                          fill = factor(Candi),
                          label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Segundo problema principal del País, según Pulso",
       x = "Problema Principal", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Problema principal") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Problema principal', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(Problem2Plot)

ggsave(Problem2Plot, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)

# Tercer más importante

Problem3Plot <-ggplot(data = subset(Pulso, !is.na(P10_3)),
                      aes(x = factor(P10_1),
                          y = prop.table(stat(count)),
                          weight = PONDERADOR,
                          fill = factor(Candi),
                          label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Tercer problema principal del país, según Pulso",
       x = "Problema Principal", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  xlab("Problema principal") + ylab("Porcentaje") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Problema principal', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(Problem3Plot)

ggsave(Problem2Plot, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)



# Felicidad ---------------------------------------------------------------

Pulso$Felici <-factor(Pulso$Felici, levels = c("Nada Feliz", "Poco Feliz", "Medianamente feliz",
                                               "Muy Feliz"))

HappyPlot <-ggplot(data = subset(Pulso, !is.na(Felici)),
                   aes(x = factor(Felici),
                       y = prop.table(stat(count)),
                       weight = PONDERADOR,
                       fill = factor(Candi),
                       label = scales::percent(prop.table(stat(count)),2))) +
  geom_bar(position = "dodge") +
  labs(title = "Grado de felicidad, según votante",
       x = "Grado de felicidad", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta Pulso Ciudadano. Segunda Quincena de Abril 2021") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Grado de Felicidad', fill = 'Candidato') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~Candi, nrow = 4)
plot(HappyPlot)

ggsave(HappyPlot, filename = "Results/HappyPlot.png",
       dpi = 400, width = 10, height = 7)


## Tablas con resultados ##

## Sexo de Votantes ##

Sexo<-table(Pulso$SexoRecod)

SexoCandi <-Pulso%>%
  group_by(Candi)%>%
  summarise(Género = round((prop.table(Sexo)*100),2))

## GSE de votantes ##

GSE <-table(Pulso$GSERecod)

GSECandi <-Pulso%>%
  group_by(Candi)%>%
  summarise(GSE = round((prop.table(GSE)*100),2))

ctable()



# Tablas Socioeconómicas ------------------------------------------------------------------

## GSE ###

TablaGSE <-ctable(Pulso$GSERecod, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown',
                  headings = F, report.nas = FALSE)

## Sexo ##

TablaSexo <-ctable(Pulso$SexoRecod, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown', headings = F, report.nas = FALSE)
view(TablaSexo)

## Grupo etáreo ##

TablaEdad <-ctable(Pulso$Edadrec, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown',
                   headings = F, report.nas = FALSE)
view(TablaEdad)





# Tablas de situación económica -------------------------------------------

TablaLabor <-ctable(Pulso$Labor, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown',
                    chisq = T, headings = F, report.nas = FALSE)

TablaMoni <-ctable(Pulso$Moni, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown',
                   chisq = T, headings = F, report.nas = FALSE)

# Tablas de posición política ---------------------------------------------

TablaPosPol <-ctable(Pulso$PosPol, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown',
                     headings = F, report.nas = FALSE)
view(TablaPosPol)

TablaParti <-ctable(Pulso$Parti, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown',
                    chisq = T, headings = F, report.nas = FALSE)
view(TablaParti)


# Tablas de emociones -----------------------------------------------------


TablaFelici <-ctable(Pulso$Felici, Pulso$Candi, prop = "c", weights = Pulso$PONDERADOR, style = 'rmarkdown',
                     headings = F, report.nas = FALSE)
view(TablaFelici)


#Agrupar tablas y guardarlas

tablas <-list(TablaEdad, TablaGSE, TablaSexo, TablaLabor, TablaMoni, TablaPosPol, TablaParti, TablaFelici)

saveRDS(tablas, file = "Results/tablas.rds")