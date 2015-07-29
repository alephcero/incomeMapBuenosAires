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
  mutate(NoSuficiente = Parcialmente.insuficiente + Insuficiente) %>%
  select(radios,LINK,comuna,CO_FRAC_RA,NoSuficiente)


##NBI

archivo.NBI = "data/CENSO/nbi/baseNBI.csv" 
if (file.exists(archivo.NBI)) {
  baseNBI = read.csv(archivo.NBI)
} else {
  baseNBI = leerTablasRedatam(archivo = "data/CENSO/nbi/nbiREDATAM.csv", chequeo = TRUE)
  write.csv(baseNBI,archivo.NBI,row.names = F)
  baseNBI = read.csv(archivo.NBI)
}

##Hacinamiento

archivo.hacinamiento = "data/CENSO/hacinamiento/baseHacinamiento.csv" 
if (file.exists(archivo.hacinamiento)) {
  baseHacinamiento = read.csv(archivo.hacinamiento)
} else {
  baseHacinamiento = leerTablasRedatam(archivo = "data/CENSO/hacinamiento/hacinamientoREDATAM.csv", chequeo = TRUE)
  write.csv(baseHacinamiento,archivo.hacinamiento,row.names = F)
  baseHacinamiento = read.csv(archivo.hacinamiento)
}

##Jefe Argentino

archivo.jefeArgentino = "data/CENSO/jefeArgentino/baseJefeArgentino.csv" 
if (file.exists(archivo.jefeArgentino)) {
  baseJefeArgentino = read.csv(archivo.jefeArgentino)
} else {
  baseJefeArgentino = leerTablasRedatam(archivo = "data/CENSO/jefeArgentino/jefeArgentinoREDATAM.csv", chequeo = TRUE)
  write.csv(baseJefeArgentino,archivo.jefeArgentino,row.names = F)
  baseJefeArgentino = read.csv(archivo.jefeArgentino)
}

##Capeco

archivo.capeco = "data/CENSO/capeco/baseCapeco.csv" 
if (file.exists(archivo.capeco)) {
  baseCapeco = read.csv(archivo.capeco)
} else {
  baseCapeco = leerTablasRedatam(archivo = "data/CENSO/capeco/capecoREDATAM.csv")
  write.csv(baseCapeco,archivo.capeco,row.names = F)
  baseCapeco = read.csv(archivo.capeco)
}


#Join de data al mapa

caba@data = left_join(caba@data,baseCondhab)
caba@data = left_join(caba@data,baseCapeco)
#caba@data = left_join(caba@data,baseNBI)
#caba@data = left_join(caba@data,baseExtrangero)



comuna = !is.na(caba@data$comuna) & caba@data$comuna == 8

plot(caba[completo,])

#Mapeo

qtm(caba, "NoSuficiente",fill.palette="Reds") # plot the basic map
qtm(caba[comuna,],"NoSuficiente",fill.palette="Reds") # plot the basic map



tm_shape(caba) +
  tm_fill("NoSuficiente", thres.poly = 0) +
  tm_layout(legend.show = FALSE, title.position = c("right", "bottom"), title.size = 3)


tm_shape(caba) +
  tm_fill("jefeUnivQ", thres.poly = 0) +
  tm_facets("comuna", free.coords=TRUE, drop.shapes=TRUE) +
  tm_layout(legend.show = FALSE, title.position = c("right", "bottom"), title.size = 3)
