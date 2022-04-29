#------------------------------------------------------------
#---------------Pakete installieren und laden----------------  
#------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(raster)

#Pakete für Heatmap und Co
library(leaflet)
library(leaflet.extras)
library(leafsync)

dim(storms)
range(storms$year)

storms <- storms %>% 
  dplyr::select(name:pressure) #dplyr muss explizit angegeben werden
View(storms)


#------------------------------------------------------------
#--------------- Implementierung der Sturm-ID ---------------  
#------------------------------------------------------------
id <- c()
j <- 1
k <- 1
for(i in storms$name) {
  if(k-1 == 0){
    id <- append(id, j)
    k <- k+1
    next
  }
  if(i != storms$name[k-1])
  {
    j <- j+1
  }
  id <- append(id, j)
  k <- k+1
}
storms <- cbind(id,storms)
storms <- storms %>% 
  dplyr::select(id:pressure)  
storms

View(storms)

#------------------------------------------------------------
#--------------- Korrigieren eines Tupels -------------------  
#------------------------------------------------------------

# Aufbereitung für Präsentation
init_data <- storms %>% dplyr::select(name,wind, pressure, category, status)
print(which(init_data$status == "tropical storm" & init_data$category == 1))
print(init_data["6904",])
init_data["6904", "status"] <- "hurricane"
print(init_data["6904",])

# Korrigierung im Datensatz für die weitere Verwendung
storms["6904", "status"] <- "hurricane"
print(storms["6904",])
View(storms)

#------------------------------------------------------------
#-------- Verteilung der Kategorien je Monat ----------------  
#------------------------------------------------------------

ggplot(storms, aes(factor(month), fill=factor(category))) + 
  geom_bar() +
  ggtitle("Verteilung der Kategorien je Monat") +
  scale_colour_manual(
    name = "Kategorien",
    breaks = c("-1","0","1","2","3","4","5"),
    values = c("-1"="#5EBAFF","0"="#00FAF4","1"="#FFFFCC","2"="#FFE775","3"="#FFC140","4"="#FF8E0D","5"="#FF6060"),
    aesthetics = c("fill")
  ) +
  scale_y_continuous(
    name = "Anzahl der Stürme in Tausend",
    labels = c("0"="0","1000"="1T","2000"="2T","3000"="3T","4000"="4T","5000"="5T"),
  )+ 
  scale_x_discrete(
    name ="",
    guide = guide_axis(n.dodge = 2),
    labels = c("1"="Januar","2"="Februar","3"="März","4"="April","5"="Mai","6"="Juni","7"="Juli","8"="August","9"="September","10"="Oktober","11"="November","12"="Dezember"),
  ) +
  theme(plot.title = element_text(hjust = 0.45, vjust =3.5,size=16, face = "bold.italic"),
        axis.title.y = element_text(vjust = 2,  face="bold"),
        legend.title = element_text(vjust = 2,  face="bold"),
        legend.position = c(0.9,0.6)
  ) 

#------------------------------------------------------------
#-------- Verteilung der max. Kategorie je Sturm ------------  
#------------------------------------------------------------
max_category_per_storm <- aggregate(storms$category~storms$id, storms, max) #mittels aggregate werden zwei Variablen in Kontext gesetzt
View(max_category_per_storm)

rel_max_category_per_storm <- prop.table(table(max_category_per_storm$`storms$category`)) # Erstellung der relativen Häufigkeit für die Anzahl der max. vorgekommenden Kategorie
rel_max_category_per_storm

plot_max_category_per_storm <- pie(rel_max_category_per_storm,
                                   col = c("#5EBAFF", "#00FAF4", "#FFFFCC", "#FFE775", "#FFE140", "#FF8E0D", "#FF6060"),
                                   main = "Verteilung max. Kategorie je Sturm",
                                   radius = 1)

#------------------------------------------------------------
#--------------- Analyse von Wind und Druck -----------------  
#------------------------------------------------------------

#-# Wind und Pressure genauer beschreiben #-#
#Allgemein "wind" in Knoten & Allgemein "pressure" in Millibars
var(storms$wind)
sd(storms$wind)
summary(storms$wind)

var(storms$pressure)
sd(storms$pressure)
summary(storms$pressure)

#------------------------------------------------------------
#----------- Wertteverteilung von Wind und Druck ------------  
#------------------------------------------------------------

# Histogramm
#wind
ggplot(storms, aes(x=wind)) %>%
  + geom_histogram(fill="orange", alpha=1, binwidth = 5) %>%
  + geom_hline(yintercept=0, color="black", size=0.3) %>%
  + labs(x='Windgeschwindigkeit [Knoten]', y='Anzahl', 
         title='Werteverteilung Windgeschwindigkeit') %>%
  + scale_y_continuous(limits = c(0,1900), breaks = seq(0,1850, by = 200)) %>%
  + scale_x_continuous(limits = c(0,160), breaks = seq(0,160, by = 20)) %>%
  + theme_minimal()

#pressure
ggplot(storms, aes(x=pressure)) %>%
  + geom_histogram(fill="orange", alpha=1, binwidth = 1) %>%
  + geom_hline(yintercept=0, color="black", size=0.1) %>%
  + labs(x='Luftdruck [mb]', y='Anzahl', title='Werteverteilung Luftdruck') %>%
  + scale_y_continuous(limits = c(0,700), breaks = seq(0,700, by = 100)) %>%
  + scale_x_continuous(limits = c(890,1030),breaks = seq(890,1030, by = 10)) %>%
  + theme_minimal()

#------------------------------------------------------------
#---------- Zusammenhang zwischen Wind und Druck ------------  
#------------------------------------------------------------

# Kovarianz & Korrelationskoeffizient
cov(storms$wind,storms$pressure)
cor(storms$wind,storms$pressure, method = "pearson")

#------------------------------------------------------------
#----------- Lineare Regression Wind und Druck --------------  
#------------------------------------------------------------

# Lineare Regression
#Zielvariable "pressure" / erklärende Variable "wind"
ggplot(storms, aes(x = wind, y = pressure)) %>%
  + geom_point(shape = 19, color = "orange") %>% 
  + geom_smooth(method = "lm", color = "brown") %>%
  + labs(x='Windgeschwindigkeit [Knoten]', y='Luftdruck [mb]', 
         title='Lineare Regression') %>%
  + scale_y_continuous(limits = c(880,1040),
                       breaks = c(880,920,960,1000,1040))
lm01 <- lm(storms$pressure ~ storms$wind)
summary(lm01)

#------------------------------------------------------------
#---- Durchschnitt Wind und Druck abhängig der Kategorie ----  
#------------------------------------------------------------

# Durchschnitt pressure pro Sturm und Durchschnitt wind pro Kategorie
avg_data <- init_data %>% group_by(category,status) %>%
  summarise(
    avg_wind = mean(wind),
    avg_pressure = mean(pressure)
  )
View(avg_data)
print(avg_data)

ggplot(avg_data, aes(x = avg_wind, y = avg_pressure, 
                     label = c("tropical depression","tropical storm",
                               "hurricane level 1","hurricane level 2",
                               "hurricane level 3","hurricane level 4",
                               "hurricane level 5"))) %>%
  + geom_point(color = "orange") %>%
  + geom_line(color = "orange") %>%
  + geom_text(hjust="inward", vjust=0) %>%
  + theme_minimal()

#------------------------------------------------------------
#------------ Beschreibung der Dauer_in_Tagen ---------------
#------------------------------------------------------------

newDataStorms <- storms %>%                                                
  group_by(id) %>%                                                             
  summarise(Messungen=n(),                                                      
            name = head(name,1)) %>%                              
  mutate(Stunden=Messungen*6,                                                  
         Tage=Stunden/24) 

newDataStorms <- newDataStorms %>%                                               
  dplyr::select(id,name,Tage) %>%                                                     
  rename(Sturm_ID = id,                                      
         Name = name,                                                        
         Dauer_in_Tagen = Tage)  

#------------------------------------------------------------
#-------------- Summary und Plot Sturmdauer -----------------
#------------------------------------------------------------

#Varianz
var(newDataStorms$Dauer_in_Tagen)
#Standardabweichung
sd(newDataStorms$Dauer_in_Tagen)
#IQR
IQR(newDataStorms$Dauer_in_Tagen)
#Zusammenfassung
summary(newDataStorms$Dauer_in_Tagen)

#Sturmdauer
boxplot(newDataStorms$Dauer_in_Tagen,
        main="Sturmdauer",
        ylab="Sturmdauer   [Tage]",
        col="orange",
        border = "brown")

#------------------------------------------------------------
#------------------- Histrogramm Dauer-----------------------
#------------------------------------------------------------

#Sturmdauerverteilung
ditplot<-ggplot(newDataStorms , aes (x=Dauer_in_Tagen))+
  geom_histogram(fill="orange",colour="brown",binwidth = 0.25)+
  scale_y_continuous(breaks = seq(0,60,by=5),
                     name="Anzahl der gemessenen Stürme")+
  scale_x_continuous(breaks = seq(0,25,by=2),
                     name="Dauer in Tagen")
ditplot


#Storms per year
spy<-storms %>% 
  group_by(id,year) %>% 
  summarise(Messungen=n())
View(spy)
spy<- spy %>% 
  group_by(year) %>% 
  summarise(Anzahl=n())
spy

View(spy)

#------------------------------------------------------------
#------------------- Stürme pro Jahr ------------------------
#------------------------------------------------------------

#summary
summary(spy$Anzahl)
#Varianz
var(spy$Anzahl)
#Standardabweichung
sd(spy$Anzahl)
#IQR
IQR(spy$Anzahl)

#Barplot
spyplot<-ggplot(spy , aes (x=year , y=Anzahl))+
  geom_bar(stat="identity",color="brown",fill="orange")+
  geom_hline(yintercept = mean(spy$Anzahl),color="red",)+
  scale_x_continuous(breaks = seq(1975,2020,by=3))+
  scale_y_continuous(breaks = c(0:28))

spyplot


#------------------------------------------------------------
#------------------- Vergleich Jahre ------------------------
#------------------------------------------------------------

# Unterteilt Storms in Zeitabschnitte
Storms_1975_1984 <- storms[storms$year==1975 | storms$year==1976 | 
                             storms$year==1977 | storms$year==1978 | 
                             storms$year==1979 | storms$year==1980 | 
                             storms$year==1981 | storms$year==1982 | 
                             storms$year==1983 | storms$year==1984,]
View(Storms_1975_1984)

Storms_2010_2019 <- storms[storms$year==2010 | storms$year==2011 | 
                             storms$year==2012 | storms$year==2013 | 
                             storms$year==2014 | storms$year==2015 | 
                             storms$year==2016 | storms$year==2017 | 
                             storms$year==2018 | storms$year==2019,]                    
View(Storms_2010_2019)


# Fügt die zeitlichen Werte hinzu
#1975-1984
Data_per_Storm_1975_1984 <- Storms_1975_1984 %>%
  group_by(id) %>%
  summarize(Messungen=n(), #Fügt zusätzliche Spalte "Messungen" hinzu
            Name = head(name,1), #Name je Sturm
            year = head(year,1)) %>% #Name je Sturm
  mutate(Stunden = Messungen * 6, #Fügt Spalte "Stunden" hinzu
         Tage = Stunden / 24) #Fügt die Spalte "Tage" hinzu
View(Data_per_Storm_1975_1984)


#2010-2019
Data_per_Storm_2010_2019 <- Storms_2010_2019 %>% 
  group_by(id) %>%
  summarize(Messungen=n(), #Fügt zusätzliche Spalte "Messungen" hinzu
            Name = head(name,1),#Name je Sturm
            year = head(year,1)) %>%#Name je Sturm
  mutate(Stunden = Messungen * 6,#Fügt Spalte "Stunden" hinzu
         Tage = Stunden / 24)#Fügt die Spalte "Tage" hinzu
View(Data_per_Storm_2010_2019)

#------------------------------------------------------------
#-------------- Lage- und Streumaße Dauer -------------------
#------------------------------------------------------------

# Berechnung der Lage- & Streumaße
summary(Data_per_Storm_1975_1984$Tage)

quantil_1975_1985_25 <- quantile(Data_per_Storm_1975_1984$Tage, 0.25)
quantil_1975_1985_75 <- quantile(Data_per_Storm_1975_1984$Tage, 0.75)
IQR_1975_1985 <- IQR(Data_per_Storm_1975_1984$Tage)
sd_1975_1985 <- sd(Data_per_Storm_1975_1984$Tage)
varianz_1975_1985 <- var(Data_per_Storm_1975_1984$Tage)

summary(Data_per_Storm_2010_2019$Tage)

quantil_2010_2019_25 <- quantile(Data_per_Storm_1975_1984$Tage, 0.25)
quantil_2010_2019_75 <- quantile(Data_per_Storm_1975_1984$Tage, 0.75)
IQR_2010_2019 <- IQR(Data_per_Storm_2010_2019$Tage)
sd_2010_2019 <- sd(Data_per_Storm_2010_2019$Tage)


#------------------------------------------------------------
#------------------- Boxplot-Vergleich ----------------------
#------------------------------------------------------------
#Boxplot: Gegenüberstellung der Entwicklung der Sturmdauer
boxp_1975_1984 <- boxplot(Data_per_Storm_1975_1984$Tage,
                          main = "Sturmdauer 1975-1984",
                          ylab = "Sturmdauer in Tagen",
                          ylim = c(0,20),
                          col = "orange",
                          border = "brown")

boxp_2010_2019 <- boxplot(Data_per_Storm_2010_2019$Tage,
                          main = "Sturmdauer 2010-2019",
                          ylab = "Sturmdauer in Tagen",
                          ylim = c(0,20),
                          col = "chocolate1",
                          border = "brown")


#------------------------------------------------------------
#------------------ Bar-Chart-Vergleich ---------------------
#------------------------------------------------------------

# Berechnung der Anzahl der Stürme pro Jahr

# Aufteilung der Stürme nach Jahr & der dazugehörigen Anzahl an Stürmen pro Jahr
Data_per_Year_1975_1984 <- Storms_1975_1984%>%
  group_by(year) %>%
  summarise(Storms_per_Year = n_distinct(name))
Data_per_Year_1975_1984

# Summiert die angefallenen Stürme pro Zeitperiode zusammen zu einer Zahl, die für den Zeitraum bestimmt ist
No_Storms_1975_1984 <- sum(Data_per_Year_1975_1984$Storms_per_Year)
No_Storms_1975_1984
View(Data_per_Year_1975_1984)


# Aufteilung der Stürme nach Jahr & der dazugehörigen Anzahl an Stürmen pro Jahr
Data_per_Year_2010_2019 <- Storms_2010_2019%>%
  group_by(year) %>%
  summarise(Storms_per_Year = n_distinct(name))


# Summiert die angefallenen Stürme pro Zeitperiode zusammen zu einer Zahl,
# die für den Zeitraum bestimmt ist
No_Storms_2010_2019 <- sum(Data_per_Year_2010_2019$Storms_per_Year)
No_Storms_2010_2019
View(Data_per_Year_2010_2019)


# Zusammenfassung der Anzahl der Stürme über Zeitraum in einer Variablen 
#-> Einfacher für den Plot
no_storms <- c(No_Storms_1975_1984, No_Storms_2010_2019)
no_storms


# Barplot für die Präsentation: Gegenüberstellung der Summe der Stürme in den Zeitperioden

barp_comparison  <- barplot(no_storms,
                            col = c("orange", "chocolate1"),
                            names.arg = c("1975-1984", "2010-2019"),
                            main = "Vergleich: Stürme gesamt über Betrachtungszeitraum",
                            ylab = "Zeiträume",
                            xlab = "Anzahl der Stürme",
                            ylim = c(0,160),
                            border = "brown") 

y <- as.matrix(no_storms,2)
text(barp_comparison, y+8, labels = as.character(y)) #Fügt die Häufigkeit der Stürme über dem Bar je Zeitperiode ein

#------------------------------------------------------------
#----------------------- Heatmap ----------------------------
#------------------------------------------------------------
ost <- storms[which.max(storms$long),]
nord <- storms[which.max(storms$lat),]
west <- storms[which.min(storms$long),]
sued <- storms[which.min(storms$lat),]

#Kartenmitte definieren
lat_center <- mean(storms$lat)
long_center <- mean(storms$long)
lat_center
long_center

#Marker für die äußerten Punkte erstellen
suedIcon <- awesomeIcons(
  text = 'S',
  iconColor = 'black',
  markerColor = 'red'
)
nordIcon <- awesomeIcons(
  text = 'N',
  iconColor = 'black',
  markerColor = 'red'
)
westIcon <- awesomeIcons(
  text = 'W',
  iconColor = 'black',
  markerColor = 'red'
)
ostIcon <- awesomeIcons(
  text = 'O',
  iconColor = 'black',
  markerColor = 'red'
)

# Heatmap mit Markern erstellen
geo_map <- storms %>%
  leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(long_center,lat_center,6) %>%
  addHeatmap(lng=~long,lat=~lat,max=1,radius=2,blur=1) %>%
  addAwesomeMarkers(~nord$long, ~nord$lat, icon=nordIcon) %>%
  addAwesomeMarkers(~ost$long, ~ost$lat, icon=ostIcon) %>%
  addAwesomeMarkers(~west$long, ~west$lat, icon=westIcon) %>%
  addAwesomeMarkers(~sued$long, ~sued$lat, icon=suedIcon)

#Map anzeigen
geo_map

#------------------------------------------------------------
#------------------ Errechnung Distanz ----------------------
#------------------------------------------------------------

data <- storms %>% group_by(id) %>% #Nach ID gruppieren
  #Distanz ausrechnen
  mutate(Distance = 
           pointDistance(cbind(long,lat),
                         cbind(lag(long),lag(lat)),lonlat = TRUE)) %>%
  summarise(
    #Namen für jeden Sturm auslesen
    name = head(name,1),
    #Kategorie für jeden Sturm auslesen
    #Status für jeden Sturm auslesen
    cat = max(category),
    status = status[which.max(category)],
    wind = max(wind),
    #Distanz summieren für jeden Sturm
    distance = sum(Distance, na.rm=TRUE) / 1000)

View(head(data,5)) #Ersten 5 Reihen anzeigen

#------------------------------------------------------------
#------------------- Analyse Distanz ------------------------
#------------------------------------------------------------

summary(data$distance) #Summary für die Distanz erzeugen
var(data$distance)
sd(data$distance)
IQR(data$distance)

#Boxplot für die Distanz der Stürme
boxplot(data$distance,main="Distanz Stürme",
        ylab="Distanz in km", col="orange",border="brown")

#------------------------------------------------------------
#----------- Zusammenhang Distanz und Stärke ----------------
#------------------------------------------------------------

#Boxplot für die Distanz der Stürme nach Kategorien
plot(data$cat, data$distance, main="Distanz Stürme nach Kategorie", xlab="Kategorie", 
     ylab="Distanz in km", col="orange",border="brown")

cov(data$wind, data$distance)
cor(as.numeric(data$cat), data$distance, method="spearman")
cor(data$wind, data$distance, method="pearson")

#------------------------------------------------------------
#----------- Lineare Regression Distanz und Stärke -------------
#------------------------------------------------------------

plot(data$wind, data$distance, xlab="Maximale Windstärke [Knoten]", 
     ylab="Distanz in km", col="orange")
lm1 <- lm(data$distance ~ data$wind)
plot(data$wind, data$distance, xlab="Maximale Windstärke [Knoten]", 
     ylab="Distanz in km", col="orange", abline(lm1), main="Lineare Regression")
summary(lm1)
predict(lm1)
