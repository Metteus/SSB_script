#################
#   Intervju    #
#               #
#################

#### Bibliotek ####
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(ggplot2)

#### Laste inn data ####
Manedslonn <- read_excel("Manedslonn (1).xlsx", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))

#### Fikse data ####
#Fjerne tomme/unyttige kolonner og rader
Manedslonn <- Manedslonn[-c(1, 2), ]
Manedslonn$...3 <- NULL

#Endre variabelnavn
Manedslonn <- Manedslonn %>%
  dplyr::rename("Yrke" = ...2,
                "Kjonn" = ...4, 
                "Type_ansettelse" = ...5,
                "Statistikkmal" = `11418: Yrkesfordelt månedslønn, etter statistikkmål, yrke, sektor, kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år`)

#Alle type ansettelser (ikke skille mellom heltid/deltid)
Manedslonn <- Manedslonn %>%
  filter(Type_ansettelse == "I alt")

#Endre format til lang data, ikke vid
Manedslonn <- pivot_longer(Manedslonn, cols = starts_with("..."),
                           names_to = "Ar")
#Endre navn på år
Manedslonn$Ar <- gsub("...6", "2015", Manedslonn$Ar)
Manedslonn$Ar <- gsub("...7", "2016", Manedslonn$Ar)
Manedslonn$Ar <- gsub("...8", "2017", Manedslonn$Ar)
Manedslonn$Ar <- gsub("...9", "2018", Manedslonn$Ar)
Manedslonn$Ar <- gsub("...10", "2019", Manedslonn$Ar)
Manedslonn$Ar <- gsub("...11", "2020", Manedslonn$Ar)

#Endre navn
Manedslonn <- Manedslonn %>%
  dplyr::rename("Lonn" = value)

#Begge kjønn
Manedslonn <- Manedslonn %>%
  filter(Kjonn == "Begge kjønn")


#lage variabel for prosentvis endring
Manedslonn <- Manedslonn %>%
  group_by(Yrke) %>%
  mutate(prosent_endring = (Lonn/lag(Lonn) - 1) * 100)

#### Lage figurer for gjennomsnittlig månedslønn, alle yrker, begge kjønn ####
#for å highlighte
Manedslonn <- Manedslonn %>% 
  mutate(ToHighlight = ifelse(Ar == 2020, "yes", "no" ))

#vanlig plot
Manedslonn %>%
  filter(Ar != "2015",
         Ar != "2016",         
         Statistikkmal == "Gjennomsnitt",
         Yrke == "0-9 Alle yrker") %>%
ggplot(mapping=aes(x=Ar, y=prosent_endring)) +
  geom_col(fill = "springgreen4") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
   ylab(NULL) + xlab(NULL) +
  ggtitle("Prosentvis endring i månedslønn, 2017-2020")

#highlight plot
Manedslonn %>%
  filter(Ar != "2015",
         Ar != "2016",         
         Statistikkmal == "Gjennomsnitt",
         Yrke == "0-9 Alle yrker") %>%
  ggplot(mapping=aes(x=Ar, y=prosent_endring, fill = ToHighlight)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  ylab(NULL) + xlab(NULL) +
  ggtitle("Prosentvis endring i månedslønn, 2017-2020") +
  scale_fill_manual(values = c("yes"="springgreen4", "no"="grey"), guide = FALSE)

#vanlig plot
Manedslonn %>%
  filter(Ar != "2015",
         Ar != "2016",         
         Statistikkmal == "Antall arbeidsforhold med lønn",
         Yrke == "0-9 Alle yrker") %>%
  ggplot(mapping=aes(x=Ar, y=prosent_endring)) +
  geom_col(fill = "springgreen4") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  ylab(NULL) + xlab(NULL) +
  ggtitle("Prosentvis endring i antall arbeidsforhold med lønn, 2017-2020")

#highlight plot
Manedslonn %>%
  filter(Ar != "2015",
         Ar != "2016",         
         Statistikkmal == "Antall arbeidsforhold med lønn",
         Yrke == "0-9 Alle yrker") %>%
  ggplot(mapping=aes(x=Ar, y=prosent_endring, fill = ToHighlight)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  ylab(NULL) + xlab(NULL) +
  ggtitle("Prosentvis endring i antall arbeidsforhold med lønn, 2017-2020")+
  scale_fill_manual(values = c( "yes"="springgreen4", "no"="gray" ), guide = FALSE)

#ENDA ET PLOT
Manedslonn[c(241:300), "Statistikkmal"] <- "Antall arbeidsforhold med lønn"

Manedslonn %>%
  filter(Ar == "2020",
         Statistikkmal == "Antall arbeidsforhold med lønn",
         Yrke != "0-9 Alle yrker") %>%
  ggplot(aes(y = reorder(Yrke, Lonn), x = Lonn)) +
  geom_col(fill ="springgreen4", position = "dodge") +
  ylab(NULL) + xlab("Antall arbeidsforhold") +
  scale_x_continuous(labels = comma)+
  ggtitle("Antall arbeidsforhold med lønn i 2020, med endring fra 2019, etter yrker")+
  geom_text(aes(label = round(prosent_endring, digits = 1)),
            position = position_dodge(width=1), hjust=-0.5)

#### Laste inn ny data ####
Manedslonn_2 <- read_excel("Manedslonn_2.xlsx", 
                                           col_types = c("text", "text", "text", 
                                                         "text", "text", "text", "numeric", 
                                                         "numeric"))

#Fjerne tomme/unyttige kolonner og rader
Manedslonn_2 <- Manedslonn_2[-c(1:3, 118:172), ]
Manedslonn_2$...2 <- NULL
Manedslonn_2$...3 <- NULL
Manedslonn_2$...6 <- NULL

Manedslonn_2[c(1:60), "11419: Månedslønn, etter statistikkmål, yrke, sektor, næring (SN2007), kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år"] <- "Gjennomsnitt"
Manedslonn_2[c(61:117), "11419: Månedslønn, etter statistikkmål, yrke, sektor, næring (SN2007), kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år"] <- "Antall heltidsekvivalenter"

#Endre variabelnavn
Manedslonn_2 <- Manedslonn_2 %>%
  dplyr::rename("Naering" = ...4, 
                "Kjonn" = ...5,
                "Statistikkmal" = `11419: Månedslønn, etter statistikkmål, yrke, sektor, næring (SN2007), kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år`)

#Endre format til lang data, ikke vid
Manedslonn_2 <- pivot_longer(Manedslonn_2, cols = starts_with("..."),
                           names_to = "Ar")

#Endre navn på år
Manedslonn_2$Ar <- gsub("...7", "2019", Manedslonn_2$Ar)
Manedslonn_2$Ar <- gsub("...8", "2020", Manedslonn_2$Ar)

#Endre variabelnavn
Manedslonn_2 <- Manedslonn_2 %>%
  dplyr::rename("Lonn" = value)

#filtrere
Manedslonn_2 <- Manedslonn_2 %>%
  filter(Kjonn == "Begge kjønn")

#lage variabel for prosentvis endring
Manedslonn_2 <- Manedslonn_2 %>%
  group_by(Naering) %>%
  mutate(prosent_endring = (Lonn/lag(Lonn) - 1) * 100)


#### FIGUR ####
Manedslonn_2 %>%
  dplyr::select(Statistikkmal, Naering, Kjonn, Ar, Lonn, prosent_endring) %>%
  filter(Ar == "2020",
         Statistikkmal == "Gjennomsnitt") %>%
ggplot(aes(y = reorder(Naering, Lonn), x = Lonn)) +
  geom_col(fill ="springgreen4", position = "dodge") +
  ylab(NULL) + xlab("Månedslønn") +
  ggtitle("Gjennomsnittlig månedslønn, og prosentvis endring fra 2019, etter hovednæringsområder") +
  geom_text(aes(label = round(prosent_endring, digits = 1)), 
            position = position_dodge(width=1), hjust=-0.5)
  
  
#### Bergfigur ####
berg <- read_excel("berg.xlsx", col_types = c("text", 
                                              "text", "text", "text", "text", "text", 
                                              "numeric", "numeric", "numeric", "numeric"))

#Fjerne tomme/unyttige kolonner og rader
berg <- berg[-c(1:3, 5:62), ]
berg$...2 <- NULL
berg$...3 <- NULL

#Endre variabelnavn
berg <- berg %>%
  dplyr::rename("Naering" = ...4, 
                "Kjonn" = ...5,
                "Type" = ...6,
                "Statistikkmal" = `11419: Månedslønn, etter statistikkmål, yrke, sektor, næring (SN2007), kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år`)


#Endre format til lang data, ikke vid
berg <- pivot_longer(berg, cols = starts_with("..."),
                             names_to = "Ar")

berg <- berg[-c(1,2), ]

#Endre navn på år
berg$Ar <- gsub("...9", "2019", berg$Ar)
berg$Ar <- gsub("...10", "2020", berg$Ar)

berg <- berg %>%
  mutate(prosent_endring = (value/lag(value) - 1) * 100)



berg %>%
  ggplot(aes(y = value, x = Ar)) +
  geom_col(fill = "springgreen4", position = "dodge")+
  ylab("Månedslønn") + xlab("År") +
  ggtitle("B Bergverksdrift og utvinning: avtalt månedslønn") +
  scale_y_continuous(limits = c(0,100000)) +
  geom_text(aes(label = round(prosent_endring, digits = 1)), 
            position = position_dodge(width=1), vjust=-0.5)


#### YRKE ####
yrke_data <- read_excel("YRKE.xlsx", col_types = c("text", 
                                              "text", "text", "text", "text", "numeric", 
                                              "numeric"))
yrke_data <- yrke_data[-c(1:3, 14:65), ]

yrke_data$`11418: Yrkesfordelt månedslønn, etter statistikkmål, yrke, sektor, kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år` <- NULL
yrke_data$...3 <- NULL
yrke_data$...4 <- NULL
yrke_data$...5 <- NULL


yrke_data <- yrke_data %>%
  dplyr::rename(Yrke = "...2")

yrke_data <- pivot_longer(yrke_data, cols = starts_with("..."),
                          names_to = "Ar")

yrke_data$Ar <- gsub("...6", "2019", yrke_data$Ar)
yrke_data$Ar <- gsub("...7", "2020", yrke_data$Ar)

yrke_data <- yrke_data %>%
  dplyr::rename(Lonn = "value")

#lage variabel for prosentvis endring
yrke_data <- yrke_data %>%
  group_by(Yrke) %>%
  dplyr::mutate(prosent_endring = (Lonn/lag(Lonn) - 1) * 100)

#figur
yrke_data %>%
  filter(Ar == "2020") %>%
  ggplot(aes(y = reorder(Yrke, Lonn), x = Lonn)) +
  geom_col(fill ="springgreen4", position = "dodge") +
  ylab(NULL) + xlab("Gjennomsnittlig månedslønn") +
  ggtitle("Gjennomsnittlig månedslønn, og prosetnvis endring fra 2019, etter yrke") +
  geom_text(aes(label = round(prosent_endring, digits = 1)), 
            position = position_dodge(width=1), hjust=-0.5) +
  scale_x_continuous(labels = comma)

#### MER DATA ####
Manedslonn_3 <- read_excel("Manedslonn_3.xlsx", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "numeric", 
                                         "numeric"))
  

#Fjerne tomme/unyttige kolonner og rader
Manedslonn_3 <- Manedslonn_3[-c(1:3, 64:115), ]
Manedslonn_3$...2 <- NULL
Manedslonn_3$...3 <- NULL
Manedslonn_3$...6 <- NULL

Manedslonn_3[c(1:60), "11419: Månedslønn, etter statistikkmål, yrke, sektor, næring (SN2007), kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år"] <- "Antall arbeidsforhold med lønn"


#Endre variabelnavn
Manedslonn_3 <- Manedslonn_3 %>%
  dplyr::rename("Naering" = ...4, 
                "Kjonn" = ...5,
                "Statistikkmal" = `11419: Månedslønn, etter statistikkmål, yrke, sektor, næring (SN2007), kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år`)

#Endre format til lang data, ikke vid
Manedslonn_3 <- pivot_longer(Manedslonn_3, cols = starts_with("..."),
                             names_to = "Ar")

#Endre navn på år
Manedslonn_3$Ar <- gsub("...7", "2019", Manedslonn_3$Ar)
Manedslonn_3$Ar <- gsub("...8", "2020", Manedslonn_3$Ar)

#Endre variabelnavn
Manedslonn_3 <- Manedslonn_3 %>%
  dplyr::rename("Antall" = value)

#filtrere
Manedslonn_3 <- Manedslonn_3 %>%
  filter(Kjonn == "Begge kjønn")

#lage variabel for prosentvis endring
Manedslonn_3 <- Manedslonn_3 %>%
  group_by(Naering) %>%
  mutate(prosent_endring = (Antall/lag(Antall) - 1) * 100)
  
#### FIGUR ####
Manedslonn_3 %>%
  filter(Ar == "2020",
         Naering != "A-S Alle næringer") %>%
  ggplot(aes(y = reorder(Naering, Antall), x = Antall)) +
  geom_col(fill ="springgreen4", position = "dodge") +
  ylab(NULL) + xlab("Antall arbeidsforhold med lønn") +
  ggtitle("Antall arbeidsforhold med lønn, og prosentvis endring fra 2019, etter hovednæringsområder") +
  geom_text(aes(label = round(prosent_endring, digits = 1)), 
            position = position_dodge(width=1), hjust=-0.5) +
  scale_x_continuous(labels = comma)
  
#### KJØNNSFORDELTE DATA ####
Manedslonn_kjonn <- read_excel("Manedslonn.xlsx", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))

Manedslonn_kjonn <- Manedslonn_kjonn[-c(1:3, 544:596), ]
Manedslonn_kjonn$`11418: Yrkesfordelt månedslønn, etter statistikkmål, yrke, sektor, kjønn, avtalt/vanlig arbeidstid per uke, statistikkvariabel og år` <- NULL
Manedslonn_kjonn$...3 <- NULL
Manedslonn_kjonn$...5 <- NULL
Manedslonn_kjonn$...6 <- NULL
Manedslonn_kjonn$...7 <- NULL
Manedslonn_kjonn$...9 <- NULL

Manedslonn_kjonn <- Manedslonn_kjonn %>%
  dplyr::rename("Statistikkmal" = ...2,
                "Yrke" = ...4,
                "Kjonn" = ...8,
                "Ansettelse" = ...10) %>%
  filter(Ansettelse == "I alt")

Manedslonn_kjonn <- pivot_longer(Manedslonn_kjonn, cols = starts_with("..."), 
             names_to = "Ar")

Manedslonn_kjonn$Ar <- gsub("...11", "2015", Manedslonn_kjonn$Ar)
Manedslonn_kjonn$Ar <- gsub("...12", "2016", Manedslonn_kjonn$Ar)
Manedslonn_kjonn$Ar <- gsub("...13", "2017", Manedslonn_kjonn$Ar)
Manedslonn_kjonn$Ar <- gsub("...14", "2018", Manedslonn_kjonn$Ar)
Manedslonn_kjonn$Ar <- gsub("...15", "2019", Manedslonn_kjonn$Ar)
Manedslonn_kjonn$Ar <- gsub("...16", "2020", Manedslonn_kjonn$Ar)

Manedslonn_kjonn[c(1:180), "Statistikkmal"] <- "Gjennomsnitt"
Manedslonn_kjonn[c(1:18), "Yrke"] <- "Alle yrker"

#lage variabel for prosentvis endring
Manedslonn_kjonn <- Manedslonn_kjonn%>%
  group_by(Kjonn) %>%
  mutate(prosent_endring = (value/lag(value) - 1) * 100)

##tabell for kjønn
Manedslonn_kjonn%>%
  filter(Ar == "2020",
         Yrke == "Alle yrker",
         Kjonn != "Begge kjønn") %>%
  ggplot(aes(y = reorder(Kjonn, value), x = value)) +
  geom_col(fill ="springgreen4", position = "dodge") +
  ylab(NULL) + xlab("Månedslønn") +
  ggtitle("Månedslønn i 2020, og prosentvis endring fra 2019, etter kjønn") +
  geom_text(aes(label = round(prosent_endring, digits = 1)), 
            position = position_dodge(width=1), hjust=-0.5) +
  scale_x_continuous(labels = comma)
#### SEKTORFORDELTE DATA ####
            







