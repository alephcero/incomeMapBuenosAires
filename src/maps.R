#Cartografia
#Tutorial https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/intro-spatial.Rmd

# Hay dos radios censales que difieren entre el mapa de la ciudad y censo
#14_10_2 lagos de palermo que ni siquiera figura como vacia en censo, no hay viviendas
#1_13_15 Puerto madero, con tabla vacia en leerTablasREDATAM.R, puede tener otros datos como casas vacias

#14_10_2 Esta es la zona de lagos de palermo la cartografia de ciudad tiene los dos radios de esa fraccion 14_10_2 y 14_10_1 (para esta ultima hay data)
#1_13_15 no esta porque En censo no esta #"AREA#020011315" esta es la tabla vacia, es un radio en puerto madero

#Mapa BA
 
caba = readOGR(dsn = "maps/radiosBA", layer = "radios_censo_2010")

#Lectura y modificacion de bases

##CONDHAB
archivo.condhab = "data/CENSO/condhab/baseCondhab.csv" 
if (file.exists(archivo.condhab)) {
  baseCondhab = read.csv(archivo.condhab)
} else {
  baseCondhab = leerTablasRedatam(archivo = "data/CENSO/condhab/condhabREDATAM.csv")
  write.csv(baseConhab,archivo.condhab,row.names = F)
  baseCondhab = read.csv(archivo.condhab)
}
names(baseCondhab) = gsub(" ",".",names(baseCondhab))

baseCondhab = 
  baseCondhab %>%
  mutate(condhabNoSuficiente = Parcialmente.insuficiente + Insuficiente) %>%
  select(radios,LINK,comuna,CO_FRAC_RA,condhabNoSuficiente)


##NBI

archivo.NBI = "data/CENSO/nbi/baseNBI.csv" 
if (file.exists(archivo.NBI)) {
  baseNBI = read.csv(archivo.NBI)
} else {
  baseNBI = leerTablasRedatam(archivo = "data/CENSO/nbi/nbiREDATAM.csv", chequeo = TRUE)
  write.csv(baseNBI,archivo.NBI,row.names = F)
  baseNBI = read.csv(archivo.NBI)
}

baseNBI = select(baseNBI,radios,LINK,comuna,CO_FRAC_RA,Hogares.con.NBI)

##Hacinamiento

archivo.hacinamiento = "data/CENSO/hacinamiento/baseHacinamiento.csv" 
if (file.exists(archivo.hacinamiento)) {
  baseHacinamiento = read.csv(archivo.hacinamiento)
} else {
  baseHacinamiento = leerTablasRedatam(archivo = "data/CENSO/hacinamiento/hacinamientoREDATAM.csv", chequeo = TRUE)
  write.csv(baseHacinamiento,archivo.hacinamiento,row.names = F)
  baseHacinamiento = read.csv(archivo.hacinamiento)
}

baseHacinamiento = select(baseHacinamiento,radios,LINK,comuna,CO_FRAC_RA,Más.de..3.00.personas.por.cuarto)
names(baseHacinamiento)[ncol(baseHacinamiento)] = "hacinamiento"

##Jefe Argentino
archivo.jefeArgentino = "data/CENSO/jefeArgentino/baseJefeArgentino.csv" 
if (file.exists(archivo.jefeArgentino)) {
  baseJefeArgentino = read.csv(archivo.jefeArgentino)
} else {
  baseJefeArgentino = leerTablasRedatam(archivo = "data/CENSO/jefeArgentino/jefeArgentinoREDATAM.csv", chequeo = TRUE)
  write.csv(baseJefeArgentino,archivo.jefeArgentino,row.names = F)
  baseJefeArgentino = read.csv(archivo.jefeArgentino)
}

baseJefeArgentino = select(baseJefeArgentino,radios,LINK,comuna,CO_FRAC_RA,X0)
names(baseJefeArgentino)[ncol(baseJefeArgentino)] = "jefeNoArgentino"


##Capeco
source("src/readCapecoAverages.R")

#Este subscript es para capeco por deciles
#archivo.capeco = "data/CENSO/capeco/baseCapeco.csv" 
#if (file.exists(archivo.capeco)) {
#  baseCapeco = read.csv(archivo.capeco)
#} else {
#  baseCapeco = leerTablasRedatam(archivo = "data/CENSO/capeco/capecoREDATAM.csv")
#  write.csv(baseCapeco,archivo.capeco,row.names = F)
#  baseCapeco = read.csv(archivo.capeco)
#}


#Join de data al mapa

caba@data = left_join(caba@data,baseCondhab)
caba@data = left_join(caba@data,baseHacinamiento)
caba@data = left_join(caba@data,baseJefeArgentino)


#Mapeo

#Condhab
mapCondhab = qtm(caba, "condhabNoSuficiente",fill.palette="Reds") +
  tm_layout(scale = 0.8,legend.position = c("right","top"),
            inner.margins = c(0.01,0.01,0.01,0.01),outer.margins = c(0.01,0.01,0.01,0.01)) 

#Hacinamiento
mapHacinamiento = qtm(caba, "hacinamiento",fill.palette="Reds") +
  tm_layout(scale = 0.8,legend.position = c("right","top"),
            inner.margins = c(0.01,0.01,0.01,0.01),outer.margins = c(0.01,0.01,0.01,0.01)) 

#JefeNoArgentino
mapJefeNoArg=qtm(caba, "jefeNoArgentino",fill.palette="Reds") +
tm_layout(scale = 0.8,legend.position = c("right","top"),
          inner.margins = c(0.01,0.01,0.01,0.01),outer.margins = c(0.01,0.01,0.01,0.01)) 


cabaCapeco = readOGR(dsn = "maps/radiosBA", layer = "radios_censo_2010")
cabaCapeco@data = left_join(cabaCapeco@data,capecoFinal)


cabaNBI = readOGR(dsn = "maps/radiosBA", layer = "radios_censo_2010")
cabaNBI@data = left_join(caba@data,baseNBI)


#CAPECO
mapCapeco = qtm(cabaCapeco, "capeco",fill.palette="Greens")+
  tm_layout(scale = 0.8,legend.position = c("right","top"),
            inner.margins = c(0.01,0.01,0.01,0.01),outer.margins = c(0.01,0.01,0.01,0.01)) 

#NBI
mapNBI = qtm(cabaNBI, "Hogares.con.NBI",fill.palette="Reds") +
  tm_layout(scale = 0.8,legend.position = c("right","top"),
            inner.margins = c(0.01,0.01,0.01,0.01),outer.margins = c(0.01,0.01,0.01,0.01))    




#CAPECO + NBI
pobres = !is.na(cabaNBI@data$Hogares.con.NBI) & cabaNBI@data$Hogares.con.NBI > 20

mapFinal = qtm(cabaCapeco, "capeco",fill.palette="Greens",fill.title = "CAPECO") + 
  qtm(cabaNBI[pobres,], "Hogares.con.NBI",fill.palette="Reds",fill.title = "% de hogares con NBI",alpha=0.5)  +
  tm_layout(scale = 0.7,legend.position = c("right","top"),inner.margins = c(0.01,0.01,0.01,0.01),outer.margins = c(0.01,0.01,0.01,0.01))

