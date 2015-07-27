#Cartografia
#Tutorial https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/intro-spatial.Rmd

# Hay dos radios censales que difieren entre el mapa de la ciudad y censo
#14_10_2 lagos de palermo que ni siquiera figura como vacia en censo, no hay viviendas
#1_13_15 Puerto madero, con tabla vacia en leerTablasREDATAM.R, puede tener otros datos como casas vacias

#14_10_2 Esta es la zona de lagos de palermo la cartografia de ciudad tiene los dos radios de esa fraccion 14_10_2 y 14_10_1 (para esta ultima hay data)
#1_13_15 no esta porque En censo no esta #"AREA#020011315" esta es la tabla vacia, es un radio en puerto madero

 
 
#CONDHAB

baseCondhab = leerTablasRedatam(archivo = "data/CENSO/condhab/condhabREDATAM.csv")
write.csv(baseConhab,"bases/Censo/exportacionesRedatamOnline/condhab/baseCondhab.csv",row.names = F)


caba = readOGR(dsn = "bases/Censo/cartografia", layer = "radios_censo_2010")

caba@data = left_join(caba@data,baseCondhab)

caba@data$NoSuficiente = caba@data$`Parcialmente insuficiente` + caba@data$Insuficiente 

comuna = !is.na(caba@data$comuna) & caba@data$comuna == 8

plot(caba[completo,])


qtm(caba, "NoSuficiente",fill.palette="Reds") # plot the basic map
qtm(caba[comuna,],"NoSuficiente",fill.palette="Reds") # plot the basic map



tm_shape(caba) +
  tm_fill("NoSuficiente", thres.poly = 0) +
  tm_layout(legend.show = FALSE, title.position = c("right", "bottom"), title.size = 3)


#CALMAT
baseCalmat = leerTablasRedatam(archivo = "bases/Censo/exportacionesRedatamOnline/calmat/calmatNuevo.csv")
write.csv(baseCalmat,"bases/Censo/exportacionesRedatamOnline/calmat/baseCalmat.csv",row.names = F)

caba = readOGR(dsn = "bases/Censo/cartografia", layer = "radios_censo_2010")

caba@data = left_join(caba@data,baseCalmat)

caba@data$CalmatBajo = caba@data$`Calidad 3` + caba@data$`Calidad 4` 

comuna = !is.na(caba@data$comuna) & caba@data$comuna == 1


qtm(caba, "CalmatBajo",fill.palette="Reds") # plot the basic map
qtm(caba[comuna,],"CalmatBajo",fill.palette="Reds") # plot the basic map

#CAPECO
baseCapeco = leerTablasRedatam(archivo = "bases/Censo/exportacionesRedatamOnline/CAPECO/capecoCABAnuevo.csv")
write.csv(baseCapeco,"bases/Censo/exportacionesRedatamOnline/CAPECO/baseCapeco.csv",row.names = F)

baseCapeco$quintil5= baseCapeco$`10` + baseCapeco$`9`

caba@data = left_join(caba@data,baseCapeco)

comuna = !is.na(caba@data$comuna) & caba@data$comuna == 1

qtm(caba, "quintil5") # plot the basic map
qtm(caba[comuna,], "jefeUnivQ") # plot the basic map




#Tenencia
baseTenencia = leerTablasRedatam(archivo = "bases/Censo/exportacionesRedatamOnline/regimenTenencia/regimenTenenciaNuevo.csv",chequeo = TRUE)
write.csv(baseTenencia,"bases/Censo/exportacionesRedatamOnline/regimenTenencia/baseTenencia.csv",row.names = F)

baseTenencia$prop = baseTenencia$`Propietario de la vivienda y del terreno` + baseTenencia$`Propietario sólo de la vivienda`

caba@data = left_join(caba@data,baseTenencia)

comuna = !is.na(caba@data$comuna) & caba@data$comuna == 1

qtm(caba, "prop") # plot the basic map
qtm(caba[comuna,], "prop") # plot the basic map


caca1 = select(baseCapeco,LINK,quintil5)
caca2 = select(baseTenencia,LINK,prop)

caca = merge(caca1,caca2)

ggplot(caca, aes(x=quintil5, y=prop)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)





#Fuente CIUDAD

caba = readOGR(dsn = "maps/radiosBA", layer = "radios_censo_2010")
caba@data = left_join(caba@data,jefeUniversitario)


comuna = !is.na(caba@data$comuna) & caba@data$comuna == 1

plot(caba[completo,])


qtm(caba, "jefeUniv") # plot the basic map
qtm(caba[comuna,], "jefeUnivQ") # plot the basic map



tm_shape(caba) +
  tm_fill("jefeUnivQ", thres.poly = 0) +
  tm_facets("comuna", free.coords=TRUE, drop.shapes=TRUE) +
  tm_layout(legend.show = FALSE, title.position = c("right", "bottom"), title.size = 3)
