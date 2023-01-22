################################PACKAGES LADEN##############################################

library("sparsevar")
library("FIAR")
library("forecast")
library(tsDyn)
library(bruceR)
library(ggcorrplot)
library(knitr)
library("xfun")
library(gtable)
library(memisc)
library(stargazer)
library(magrittr)
library(gtsummary)
library("strucchange")
library(cowplot)
library("SciViews")
library(gridExtra)
library("ggcorrplot")
library("lattice")
library("Formula")
library("survival")
library(corrplot)
library("Hmisc")
library("readxl")
library("rvest")
library("dplyr")
library("janitor")
library(dplyr)
library(stringr)
library(tidyverse)
library(xts)
library(tibble)
library("writexl")
library(ggplot2)
library("zoo")
library("kableExtra")
library("gridExtra")
library("readxl")
library("tseries")
library("urca")
library("ggfortify")
library("ggplot2")
library("tseries")
library("aTSA")
library("forecast")
library("vars")
library("MLmetrics")
library("mFilter")
library("magick")
library("webshot")
library("lmtest")

############################################################################################
############################################################################################
############################################################################################


######################################Daten laden#############################################################


#Unsicherheitsindikatoren:

GI = as.data.frame(read.csv("Google Insolvenz.csv",sep=",",header = T))
colnames(GI)=c("Suche")
GI$Suche = as.numeric(GI$Suche)
GI$Datum = row.names(GI)
GI=GI[-1,]
GI$Datum = as.yearmon(GI$Datum)
GI$Datum = as.Date(GI$Datum,format = "%Y-%m")
rownames(GI) = NULL

EPU = read_excel("Europa Policy Uncertainty.xlsx")
EPU = EPU[205:nrow(EPU),]
EPU = data.frame(cbind(EPU$Year,EPU$Month,EPU$Germany_News_Index))
colnames(EPU) = c("Jahr","Monat","German News Index")
for(i in (1:ncol(EPU))){
  EPU[,i]=as.numeric(EPU[,i])
}

PI = read.csv("Produktionsindex.csv",sep=",")
PI=PI[-c(1:8),]
PI = PI[,-c(3)]
colnames(PI) = c("Datum","Kurs")
PI$Kurs = as.numeric(PI$Kurs)
rownames(PI)=NULL

#Preisindizes:

VPIFood = read.csv("Consumer Price Index Food.csv",sep=",")
VPIFood = VPIFood[-c(1:7),-3]
colnames(VPIFood) = c("Datum","VPI Food")
VPIFood$`VPI Food` = as.numeric(VPIFood$`VPI Food`)
VPIFood$Datum = as.yearmon(VPIFood$Datum)
VPIFood$Datum = as.Date(VPIFood$Datum,format = "%Y-%m")
row.names(VPIFood)=NULL

VPIEnergy = read.csv("Consumer Price Index Energy.csv",sep=",")
VPIEnergy = VPIEnergy[-c(1:7),-3]
colnames(VPIEnergy) = c("Datum","VPI Energy")
VPIEnergy$Datum = as.yearmon(VPIEnergy$Datum)
VPIEnergy$Datum = as.Date(VPIEnergy$Datum,format = "%Y-%m")
VPIEnergy$`VPI Energy` = as.numeric(VPIEnergy$`VPI Energy`)
row.names(VPIEnergy)=NULL

VPI = read.csv("Consumer Price Index.csv",sep=",")
VPI = VPI[-c(1:8),-3]
colnames(VPI) = c("Datum","VPI")
VPI$`VPI` = as.numeric(VPI$`VPI`)
VPI$Datum = as.yearmon(VPI$Datum)
VPI$Datum = as.Date(VPI$Datum,format = "%Y-%m")
row.names(VPI)=NULL

#Alle Zeitreihen zwischenspeichern und Zeitraum eingrenzen 2004-2022

Datum=GI$Datum[1:217]
PI = PI$Kurs[157:373]
EPU=EPU$`German News Index`[1:217]
GI=GI$Suche[1:217]
VPIF=VPIFood$`VPI Food`[157:373]
VPIE=VPIEnergy$`VPI Energy`[157:373]
VPI=VPI$VPI[158:374]

############################################################################################
############################################################################################
############################################################################################

##############################GRAPHISCHE DARSTELLUNGEN DER DATEN#################################################

#1. Darstellung der Preisindizes --> Anhang 3

graph = data.frame(Datum,VPI,VPIF,VPIE,EPU,PI,GI)


plot1 = ggplot(graph, aes(x = graph$Datum)) +
  geom_line(aes(y = graph$VPI, colour = "black"), size = 0.8) +
  geom_line(aes(y = graph$VPIF, colour = "red"), size = 0.8) +
  geom_line(aes(y = graph$VPIE, colour = "snow4"), size = 0.8) +
  geom_rect(aes(xmin = graph$Datum[49], xmax = graph$Datum[72], ymin = 125, ymax = 130,fill="blueviolet") ,alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[84], xmax = graph$Datum[120], ymin = 125, ymax = 130,fill="cyan"), alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[195], xmax = graph$Datum[217], ymin = 125, ymax = 130,fill="darkolivegreen"), alpha = 0.5)+
  scale_fill_identity(name=NULL,labels = c(blueviolet="Finanzkrise",cyan="Euro-/Schuldenkrise",darkolivegreen="Coronakrise"),guide="legend")+
  scale_color_identity(name = NULL, 
                       labels = c(black = "VPI", red = "VPI Lebensmittel",
                                  snow4 = "VPI Energie"
                       ),  guide = "legend") +
    theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Verbraucherpreisindex: Gesamt, Lebensmittel, Energie", subtitle = "2015=100",
       y     = "Prozent",
       x     = "Jahre")

plot1

ggsave("Anhang3.png",plot = plot1,scale = 1,width = 12,height = 8,device='png', dpi=100)


#2. Darstellung der Unsicherheitsindikatoren --> Anhang 4

plot2b = ggplot(graph, aes(x = graph$Datum)) +
  geom_line(aes(y = graph$EPU, colour = "red"), size = 0.8) +
  geom_rect(aes(xmin = graph$Datum[49], xmax = graph$Datum[72], ymin = 620, ymax = 640,fill="blueviolet") ,alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[84], xmax = graph$Datum[120], ymin = 620, ymax =640,fill="cyan"), alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[195], xmax = graph$Datum[217], ymin = 620, ymax = 640,fill="darkolivegreen"), alpha = 0.5)+
  scale_fill_identity(name=NULL,labels = c(blueviolet="Finanzkrise",cyan="Euro-/Schuldenkrise",darkolivegreen="Coronakrise"),guide="legend")+
  scale_color_identity(name = NULL, 
                       labels = c(black = "VPI", red = "VPI Lebensmittel",
                                  snow4 = "VPI Energie",orange="Google Suche"
                       ),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "Null",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Europäischer Nachrichten Index", subtitle = "Politische Unsicherheit",
       y     = "PU",
       x     = "Jahre")

plot2b

plot2c = ggplot(graph, aes(x = graph$Datum)) +
  geom_line(aes(y = PI), colour = "orange", size = 0.8) +
  geom_rect(aes(xmin = graph$Datum[49], xmax = graph$Datum[72], ymin = 125, ymax = 130,fill="blueviolet") ,alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[84], xmax = graph$Datum[120], ymin = 125, ymax = 130,fill="cyan"), alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[195], xmax = graph$Datum[217], ymin = 125, ymax = 130,fill="darkolivegreen"), alpha = 0.5)+
  scale_fill_identity(name=NULL,labels = c(blueviolet="Finanzkrise",cyan="Euro-/Schuldenkrise",darkolivegreen="Coronakrise"),guide="legend")+
  #scale_color_identity(name = NULL, 
   #                    labels = c(black = "VPI", red = "VPI Lebensmittel",
    #                              snow4 = "VPI Energie",orange="Google Suche"
     #                  ),
      #                 guide = "legend") +
  theme_bw() +
  theme(legend.position = "null",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Produktionsindex", subtitle = "Unterehmerische Unsicherheit",
       y     = "PI",
       x     = "Jahre")

plot2c

plot2d = ggplot(graph, aes(x = graph$Datum)) +
  geom_line(aes(y = graph$GI), colour = "blue", size = 0.8) +
  geom_rect(aes(xmin = graph$Datum[49], xmax = graph$Datum[72], ymin = 115, ymax = 120,fill="blueviolet") ,alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[84], xmax = graph$Datum[120], ymin = 115, ymax = 120,fill="cyan"), alpha = 0.5)+
  geom_rect(aes(xmin = graph$Datum[195], xmax = graph$Datum[217], ymin = 115, ymax = 120,fill="darkolivegreen"), alpha = 0.5)+
  scale_fill_identity(name=NULL,labels = c(blueviolet="Finanzkrise",cyan="Euro-/Schuldenkrise",darkolivegreen="Coronakrise"),guide="legend")+
  scale_color_identity(name = NULL, 
                       labels = c(black = "VPI", red = "VPI Lebensmittel",
                                  snow4 = "VPI Energie",orange="Google Suche"
                       ),
                       guide = "legend") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Google Suche: Insolvenz", subtitle = "Individuelle Unsicherheit",
       y     = "Anzahl Suchen",
       x     = "Jahre")

plot2d

plot2= grid.arrange(plot2b,plot2c,plot2d,ncol=1,nrow=3)


ggsave("Anhang4.png",plot = plot2,scale = 1,width = 20,height = 18,device='png', dpi=500)


#Nicht in der Arbeit berücksichtigte Darstellungen:

ggplot(graph, aes(x=GI, y=VPIE, fill=GPR, color=EPU, size=PI)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,12))+
  theme_bw() 

ggplot(graph, aes(x=GI, y=VPIF, fill=GPR, color=EPU, size=PI)) +
  geom_point(shape=21) +
  scale_color_gradient(low="red", high="green") +
  scale_size_continuous(range=c(1,12))+
  theme_bw() 

############################################################################################
############################################################################################
############################################################################################

######################################Einfache Korrelationsanalyse#############################################################


cordata = cbind(VPI,VPIF,VPIE,EPU,PI,GI)
colnames(cordata)=c("VPI","VPI Lebensmittel","VPI Energie","Europäischer Nachrichten Index","Produktionsindex","Google-Trend Statistik Insolvenz")
cor1 = cor(cordata)
ggcorrplot(cor1[,6:1], hc.order = FALSE,lab = TRUE,lab_col = "black",outline.col = "white",colors = c("#6D9EC1", "white", "#E46726"))+
  ggtitle("Korrelationsmatrix: Unsicherheitsindikatoren und Inflation")

stargazer(cor1,title = "Korrelationsmatrix: Unsicherheitsindikatoren und Preisindizes",out = "Anhang1.html") #Anhang 1

############################################################################################
############################################################################################
############################################################################################

######################################Kerndichteschätzung#############################################################

#1. Kerndichteschätzung der Preisindizes --> Anhang 5
den1 = data.frame(VPI,VPIE,VPIF)


dichte1 = ggplot(data=den1) +
  stat_density(aes(x=VPI,fill="black"),adjust=1.5, alpha=.2)+
  stat_density(aes(x=VPIE,fill="cyan3"),adjust=1.5, alpha=.4)+  
  stat_density(aes(x=VPIF,fill="red"),adjust=1.5, alpha=.4) +
  scale_fill_identity(name = NULL, 
                      labels = c(black = "VPI", red = "VPI Lebensmittel",
                                 cyan3 = "VPI Energie"
                      ),
                      guide = "legend")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Dichtefunktion: VPI, VPI Lebensmittel, VPI Energie", subtitle = "2015=100",
       y     = "Dichtefunktion",
       x     = "x")

dichte1

ggsave("Anhang5.png",plot = dichte1,scale = 1,width = 12,height = 5,device='png', dpi=500)

#2. Kerndichteschätzung der Unsicherheitsindikatoren --> Anhang 6
den2 = data.frame(EPU,PI,GI)

dichte2 = ggplot(data=den2) +
  stat_density(aes(x=EPU,fill="red"),adjust=1.5, alpha=.4)+  
  stat_density(aes(x=PI,fill="orange"),adjust=1.5, alpha=.4) +
  stat_density(aes(x=GI,fill="blue"),adjust=1.5, alpha=.4) +
  scale_fill_identity(name = NULL, 
                      labels = c(red = "Europäischer Nachrichten Index",
                                 orange = "Produktionsindex",blue="Google-Trend Statistik"
                      ),
                      guide = "legend")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Dichtefunktion: Europäischer Nachrichten Index, Produktionsindex, Google-Trend Statistik",
       y     = "Dichtefunktion",
       x     = "x")

dichte2

ggsave("Anhang6.png",plot = dichte2,scale = 1,width = 12,height = 5,device='png', dpi=500)

############################################################################################
############################################################################################
############################################################################################

##############################PRÜFEN DER DATEN AUF SAISONALE MUSTER --> Nicht in der Arbeit##########################################################################

#1. Schritt: Monatliche Saisondummies erstellen:
jahre <- as.numeric(format(Datum, '%Y'))
monate <- as.numeric(format(Datum, '%m'))
Dat = data.frame(Datum,jahre,monate)

M1=as.numeric(ifelse(Dat$monate==1, 1, 0))
M2=as.numeric(ifelse(Dat$monate==2, 1, 0))
M3=as.numeric(ifelse(Dat$monate==3, 1, 0))
M4=as.numeric(ifelse(Dat$monate==4, 1, 0))
M5=as.numeric(ifelse(Dat$monate==5, 1, 0))
M6=as.numeric(ifelse(Dat$monate==6, 1, 0))
M7=as.numeric(ifelse(Dat$monate==7, 1, 0))
M8=as.numeric(ifelse(Dat$monate==8, 1, 0))
M9=as.numeric(ifelse(Dat$monate==9, 1, 0))
M10=as.numeric(ifelse(Dat$monate==10, 1, 0))
M11=as.numeric(ifelse(Dat$monate==11, 1, 0))
M12=as.numeric(ifelse(Dat$monate==12, 1, 0))
M = cbind(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12)
colnames(M) = c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")

#2. Schritt: Makroökonomische Variablen mittels LR-Test auf monatliche Saisonstruktur testen:
VPIBE = (lm(VPI ~ Datum + M - 1))
V1 = (lm(VPI ~ Datum - 1))
lrtest(VPIBE,V1) 

VPIEBE = (lm(VPIE ~ M- 1))
V2=(lm(VPIE ~ Datum- 1))
lrtest(VPIEBE,V2) 

VPIFBE = (lm(VPIF ~ Datum + M- 1))
V3 = (lm(VPIF ~ Datum- 1))
lrtest(VPIFBE,V3) 

EPUBE = (lm(EPU ~ Datum +M - 1))
V4 = (lm(EPU ~ Datum  - 1))
lrtest(EPUBE,V4) 

GIBE = (lm(GI ~ Datum + M- 1))
V5 = (lm(GI ~ Datum- 1))
lrtest(GIBE,V5) 


GPRBE = (lm(GPR ~ Datum +M- 1))
V6 = (lm(GPR ~ Datum- 1))
lrtest(GPRBE,V6) 

PIBE = (lm(PI ~ Datum+M - 1))
V7 = (lm(PI ~Datum-1))
lrtest(PIBE,V7) 

#3. Schritt: Darstellen der Ergebnisse: 
stargazer(VPIBE,VPIEBE,VPIFBE, type = "html",
          title = "Saisondummies VPI: Monate",out="table1.html",dep.var.labels = c("VPI","VPI Energie","VPI Lebensmittel"))

stargazer(EPUBE,GPRBE,GIBE,PIBE, type = "html",
          title = "Saisondummies Unsicherheitsindikatoren: Monate",out="table2.html",dep.var.labels = c("Politische Unsicherheit (EPU)","Globale Unsicherheit (GRP)","Individuelle Unsicherheit (GI)","Unternehmerische Unsicherheit (PI)"))

############################################################################################
############################################################################################
############################################################################################

#######################################TESTEN DER ZEITREIHEN AUF STATIONARITÄT###############################################################################

#Vorgehen: KPSS-Test und ADF-Test werden verwendet. Erst auf die Zeitreihen in Niveaus, dann je nach Ergebnis in Wachstumsratne.

#1. Schritt: Zeitreihen in Niveaus auf Stationarität testen:
summary(ur.df(VPIE, lags = 18, type = "drift")) #nicht stationär
summary(ur.kpss(VPIE, type = "mu", lags = "long")) #nicht stationär

summary(ur.df(VPIF, lags = 18, type = "drift")) #nicht stationär
summary(ur.kpss(VPIF, type = "mu", lags = "long")) #nicht stationär

summary(ur.df(EPU, lags = 18, type = "drift")) #nicht stationär
summary(ur.kpss(EPU, type = "mu", lags = "long")) #nicht-stationär

summary(ur.df(GI, lags = 18, type = "drift")) #nicht stationär
summary(ur.kpss(GI, type = "mu", lags = "long")) #nicht stationär

summary(ur.df(PI, lags = 18, type = "drift")) #nicht stationär
summary(ur.kpss(PI, type = "mu", lags = "long")) #nicht stationär

#Ergebnis: Alle Zeitreihen in Niveaus sind nicht stationär.

#2. Schritt: Zeitreihen in Wachstumsraten auf Stationarität testen
GISt = (diff(GI,differences = 1)/GI[1:216])*100
EPUSt = (diff(EPU,differences = 1)/EPU[1:216])*100
GPRSt = (diff(GPR,differences = 1)/GPR[1:216])*100
VPISt = (diff(VPI,differences = 1)/VPI[1:216])*100
VPIESt = (diff(VPIE,differences = 1)/VPIE[1:216])*100
VPIFSt = (diff(VPIF,differences = 1)/VPIF[1:216])*100
row.names(PI) <- NULL
PISt = (diff(PI,differences = 1)/PI[1:216])*100
  
summary(ur.df(VPIESt, lags = 18, type = "drift")) 
summary(ur.kpss(VPIESt, type = "mu", lags = "long")) 

summary(ur.df(VPIFSt, lags = 18, type = "drift")) 
summary(ur.kpss(VPIFSt, type = "mu", lags = "long")) 

summary(ur.df(GISt, lags = 18, type = "drift"))
summary(ur.kpss(GISt, type = "mu", lags = "long"))

summary(ur.df(EPUSt, lags = 18, type = "drift")) 
summary(ur.kpss(EPUSt, type = "mu", lags = "long")) 

summary(ur.df(PISt, lags = 18, type = "drift")) 
summary(ur.kpss(PISt, type = "mu", lags = "long")) 

#Ergebnis: Alle Zeitreihen in Wachstumsraten sind stationär --> alle Zeitreihen weisen
#einen Integrationsgrad von 1 auf.

#3.Schritt: Graphische Darstellung --> Anhang 7
Modell <- c("Europäischer Nachrichten Index","Google-Trend Statistik","Produktionsindex","VPI Energie","VPI Lebensmittel")
BG1 <- c("0.005","2.07","2.56","1.18","0.41")
BG2 <- c("1.20","1.19","0.83","0.98","1.53")
Entscheidung = c("I(1)","I(1)","I(1)","I(1)","I(1)")

dt <- data.frame(cbind(Modell,BG1,BG2,Entscheidung))
dt %>%
  kbl(caption = "Übersicht des Betrags der Teststatistiken des ADF- und KPSS-Tests der Zeitreihen in Niveaus",col.names = c("Variable","ADF-Test","KPSS-Test","Integrationsgrad")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:5, align = "c") %>% footnote( symbol = c("Testentscheidung zum 5%-Signifikanzniveau ; KPSS-Test mit 14 Lags; ADF-Test mit 18 Lags") )  %>% 
  save_kable(file = "Anhang7.png",zoom=4)


############################################################################################
############################################################################################
############################################################################################

#######################################TESTEN DER ZEITREIHEN AUF KOINTEGRATION###############################################################################

#1.Schritt: Y-Matrix und Saisondummies:
dummy = data.frame(M)
vardata1 = cbind(VPIE,VPIF,GI,EPU) 

#2. Schritt: Optimale Lag-Länge der VAR-Darstellung des Modells
VARselect(vardata1,lag.max =14,exogen = dummy,type = "none") #2 Lags nach AIC
stargazer(VARselect(vardata1,lag.max = 14,season=12),title = "Ergebnis der optimalen Lag-Wahl", out="Anhang8.html") 
#Darstellung der Ergebnisse --> Anhang 8

#3. Schritt: VEC-Darstellung des VAR-Modells schätzen --> Aus 2 VAR-Lags wird 1 VEC-Lag
VECM1 = VECM(vardata1,lag=1, r=3, estim = c("ML"),exogen = M[,1:11],include = "const") 

#4. Schritt: Rang testen --> Eigenvalue- und Trace-Test, um Anzahl der Kointegrationsbeziehungen festzustellen
summ = summary(rank.test(VECM1, type = c("eigen", "trace"), cval = 0.05))
summary(VECM1)
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
#Ergebnis: Laut Tracetest r=1: Laut Eigenwerttest r=2

#5. Schritt: Ergebnisse darstellen --> Anhang 9
dt <- data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Ergebnisse des Kointegrationstest",col.names = c("Rang","P-Wert Trace","P-Wert Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") %>% 
  save_kable(file = "Anhang9.png",zoom=4)

############################################################################################
############################################################################################
############################################################################################

#######################################SCHÄTZEN DES VAR-MODELLS###############################################################################

#VEC-Modell ist voll spezifiziert --> Y-Vektor, Anzahl Kointegrationsbeziehungen + Lag-Länge ist festgelegt, aber 
#Langfristiges stabiles Gleichgewicht kann nur durch weitere Annahmen eindeutig identifiziert werden --> mit VAR-Darstellung weiterarbeiten

Var1 = VAR(vardata1,p = 2,type = "const",season=12)

############################################################################################
############################################################################################
############################################################################################

#######################################RESIDUENANALYSE###############################################################################

vardata0 = cbind(VPIE,VPIF,GI,EPU,PI) 
Var0 = VAR(vardata0,p = 2,type = "const",season=12)
#Model mit PI schätzen, um zu zeigen, dass nur ohne PI keine Autokorrelation vorliegt.

#1. Auf Autokorrelation : H0 = keine Autokorrelation bis Lag...
serial.test(Var0, lags.bg = 12, type = "BG")
serial.test(Var1, lags.bg = 12, type = "BG")

#2.Test auf Normalverteilung: H0 = Normalverteilung liegt vor 
normality.test(Var0)
normality.test(Var1)

#3. Test auf Homoskedastie: H0 = Homoskedastie
Homosked0 <- arch.test(Var0,lags.multi = 10)
Homosked0
Homosked1 <- arch.test(Var1,lags.multi = 10)
Homosked1

#Fazit: Nur VAR-Modell ohne Produktionsindex erfüllt alle Annahmen.

#4. Ergebnisse darstellen --> Anhang 10
rang= c("0.11","<0.01","1")
trace = c("0.01","<0.01","1")
eigen = c("Breusch-Godfrey LM-Test","Jarque-Bera-Test","ARCH-Test")
dt <- data.frame(cbind(eigen,trace,rang))
dt %>%
  kbl(caption = "Analyse der Residuen einer VAR(2)-Schätzung mit Produktionsindex (links) und ohne Produktionsindex (rechts)",col.names = c("Test","P-Wert VAR(2) mit PI","P- Wert VAR(2) ohne PI")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:3, align = "c") %>% 
  save_kable(file = "Anhang10.png",zoom=4)

############################################################################################
############################################################################################
############################################################################################

#######################################INTERPRETATION DES VAR(2)-MODELLS###############################################################################

#1. Impuls-Antwort-Folgen

#1.1. Ist eine Orthogonalisierung notwendig oder gibt es zeitgleiche Effekte? --> LM-Test nach Schröder (2012)
corr1 <- (summary(Var1)$corres) 
lmtest <- (corr1[upper.tri(corr1)])^2
test = sum(lmtest)*216 #test = 39.71 ; d=6
p =1-pchisq(39.71 , df=6)

#Fazit: Residuen sind nicht unabhängig --> es bestehen zeitgleiche Effekte --> Orthogonalisierung notwendig
#Notwendigkeit der Orthogonalisierung wurde bereits beim Festlegen der Reihenfolge der Daten berücksichtigt

#1.2. Ergebnisse darstellen --> Anhang 11 und 12
rang= round(test,2)
trace = round(p,6)
eigen = "Nullhypothese verwerfen"
dt <- data.frame(cbind(rang,trace,test))
dt %>%
  kbl(caption = "LM-Test auf das Bestehen zeitgleicher Effekte",col.names = c("Teststatistik","P-Wert","Testentscheidung")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:1, align = "c") %>% 
  save_kable(file = "Anhang11.png",zoom=4)

stargazer(corr1,type = "html",out="Anhang12.html",title = "Korrelationsmatrix der Residuen des VAR(2)-Modells")

#1.3. Berechnen und plotten der Impuls-Antwort-Funktionen:

#1.3.1. Reaktion der Unsicherheitsindikatoren nach einem Schock der VPI --> Anhang 14
png("irfvpifgi.png",width = 1250,height = 800,res=100)
plot(irf(Var1,impulse = "VPIF",response = "GI",n.ahead = 20,ortho = T,seed = 3000),main= "Auswirkung eines Schocks des VPI für Lebensmittel",xlab ="Perioden",ylab="Google-Trend-Statistik-Insolvenz (Individuelle Unsicherheit)")
dev.off()

png("irfvpifpi.png",width = 1250,height = 800,res=100)
plot(irf(Var1,impulse = "VPIF",response = "EPU",n.ahead = 20,ortho = T,seed=3000),main= "Auswirkung eines Schocks des VPI für Lebensmittel",xlab ="Perioden", ylab = "Europäischer-Nachrichten-Index (Politische Unsicherheit)")
dev.off()

png("irfvpiegi.png",width = 1250,height = 800,res=100)
plot(irf(Var1,impulse = "VPIE",response = "GI",n.ahead = 20,ortho = T,seed=1000,runs = 100),xlab ="Perioden",main= "Auswirkung eines Schocks des VPI für Energie",ylab="Google-Trend-Statistik-Insolvenz (Individuelle Unsicherheit)")
dev.off()

png("irfvpiepi.png",width = 1250,height = 800,res=100)
irfvpifpi = plot(irf(Var1,impulse = "VPIE",response = "EPU",n.ahead = 20,ortho = T,seed=2000),xlab ="Perioden",main= "Auswirkung eines Schocks des VPI für Energie",ylab = "Europäischer-Nachrichten-Index (Politische Unsicherheit)")
dev.off()

#1.3.2. Reaktion der Preisindizes nach einem Schock der Unsicherheitsindikatoren --> Anhang 13

png("irfgivpie.png",width = 1250,height = 800,res=100)
plot(irf(Var1,impulse = "GI",response = "VPIE",n.ahead = 20,ortho = T,seed=2000),xlab="Perioden",ylab="VPI Energie",main= "Auswirkung eines Schocks der Google-Trend-Statistik-Insolvenz (Individuelle Unsicherheit)")
dev.off()

png("irfepuvpie.png",width = 1250,height = 800,res=100)
plot(irf(Var1,impulse = "EPU",response = "VPIE",n.ahead = 20,ortho = T,seed=2000),xlab="Perioden",ylab="VPI Energie",main= "Auswirkung eines Schocks des Europäischen-Nachrichten-Index (Politische Unsicherheit)")
dev.off()

png("irfgivpif.png",width = 1250,height = 800,res=100)
plot(irf(Var1,response = "VPIF",impulse = "GI",n.ahead = 20,ortho = T,seed=3000),xlab="Perioden",ylab="VPI Lebensmittel",main= "Auswirkung eines Schocks der Google-Trend-Statistik-Insolvenz (Individuelle Unsicherheit)")
dev.off()

png("irfepuvpif.png",width = 1250,height = 800,res=100)
plot(irf(Var1,response = "VPIF",impulse = "EPU",n.ahead = 20,ortho = T,seed=5000),xlab="Perioden",ylab="VPI Lebensmittel",main= "Auswirkung eines Schocks des Europäischen-Nachrichten-Index (Politische Unsicherheit)")
dev.off()

#2. Granger-Kausalitätstest --> Nicht in die Arbeit aufgenommen

set.seed(2000)

#2.1. Unsicherheit granger-kausal für Preisindizes?
granger_causality(Var1,var.y = c("VPIE","VPIF"),var.x = c("GI","EPU"))

#2.2. Preisindizes granger-kausal für Unsicherheit?
granger_causality(Var1,var.x = c("VPIF","VPIE"),var.y = c("GI","EPU"))

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################