#Enumerar las variables a mantener
variablesJefatura = c('idHogar',
                      'ch03',
                      'ch04',
                      'ch12',
                      'ch13')

#Crear una nueva base con todas las PERSONAS con su relacion al jefe con esas variables
baseJefatura = personas[,variablesJefatura]
rm(variablesJefatura)
baseJefatura = baseJefatura[order(baseJefatura$idHogar),]


#Crear una base con solo las Jefaturas Femeninas
hogaresJefaturaFemenina = filter(baseJefatura, ch03 == 'jefe', ch04 == 'mujer')
#Dejar unicamente el idHogar
hogaresJefaturaFemenina = select(hogaresJefaturaFemenina,idHogar)
#Crear una variable Jefatura Femenina con un 1 para el join
hogaresJefaturaFemenina$jefaturaFemenina = rep(1,nrow(hogaresJefaturaFemenina))

#Hacer el Join
hogares = left_join(hogares, hogaresJefaturaFemenina)
rm(hogaresJefaturaFemenina)
#Cambiar NAs por 0 para la regresion 
hogares$jefaturaFemenina[is.na(hogares$jefaturaFemenina)] = 0



#JEFATURA COMPLETA

#Crear un factor unico para cada hogar

#Agrupar en una nueva base los hogares
insumo = group_by(baseJefatura,idHogar)
#Detectar la presencia de jefatura completas via la presencia del conyuge
insumo = summarise(insumo,
          jefaturaCompleta = sum(ch03 == 'c\xf3nyuge/pareja')
          )

#Hacer el join
hogares = left_join(hogares, insumo)
rm(insumo)




#HOGAR DE 1 HABITANTE
hogares$habitanteUnico = hogares$ix_tot == 1
hogares$habitanteUnico[hogares$habitanteUnico] = 1



#Hacinamiento 
#Cociente entre la cantidad total de personas del hogar y la cantidad total de 
#habitaciones o piezas de que dispone el mismo.
#más de tres personas por cuarto. 

totalPersonas =
  personas %>%
  select(idHogar) %>%
  group_by(idHogar) %>%
  summarise(totalMiembros = n())

hogares = left_join(hogares, totalPersonas)

rm(totalPersonas)

#ii1 ¿Cuántos ambientes / habitaciones tiene este hogar para su uso exclusivo?
#ii2 De esos, ¿cuántos usan habitualmente para dormir?

hogares$hacinamiento = hogares$totalMiembros/hogares$ii1

#VIVIENDA CON MAS DE UN HOGAR
hogares$hogaresMult = as.numeric(hogares$nro_hogar > 1)


#NBI - Necesidades Básicas Insatisfechas
#Al menos uno de los siguientes indicadores de privación: 
     
#  Hacinamiento (NBI 1): hogares con más de tres personas por cuarto.

hogares$nbi1 = as.numeric(hogares$hacinamiento>3)

#  Tipo de Vivienda (NBI 2): hogares que habitan una vivienda de tipo inconveniente
    ##(pieza de inquilinato, vivienda precaria u otro tipo, lo que excluye casa, departamento y rancho).

hogares$nbi2 = 0
hogares$nbi2[hogares$iv1 != "casa" & hogares$iv1 !="departamento"] = 1

#  Condiciones sanitarias (NBI 3): Hogares que habitan en viviendas que no tienen retrete o tienen retrete sin descarga de agua.


hogares$nbi3 = 0
hogares$nbi3[hogares$iv8 == "no" | hogares$iv10 == 3] = 1

#  Asistencia escolar (NBI 4): hogares que tienen al menos un niño en edad escolar 
    ##(6 a 12 años) que no asiste a la escuela.
personasNbi4 = 
  personas %>%
  filter(ch06 >= 6 & ch06 <= 12 & ch10 != 1) %>%
  select(idHogar)

hogares$nbi4 = 0
hogares$nbi4[hogares$idHogar %in% personasNbi4$idHogar] = 1

rm(personasNbi4)

#Fuente: Situación y Evolución Social (Sintesis Nº4); INDEC


#Capacidad de subsistencia (NBI 5): hogares que tienen cuatro o más personas por miembro
    ##ocupado, cuyo jefe no hubiese completado el tercer grado de escolaridad primaria.)
hogares$nbi5 = 0

personasNbi5 = 
  personas %>%
  group_by(idHogar) %>%
  summarise(miembrosTotal = n(),
            miembrosOcupados = sum(estado == "ocupado"),
            jefeBajaEduc = 
              sum(
                (ch03 == "jefe") & (                                        #sea jefe  Y
              (ch12==1 | ch12==9) |                                         #haya cursado mas alto solo jardin o especial O
              (
                (ch12==2 | ch12==3) &                                       #haya cursado primaria o egb como mayor grado
                (ch13 != 1) &                                               #no terminado
                (ch14 == "00" | ch14 == "01" | ch14 == "02" | ch14 == "99") #no hubiese completado el tercer grado
                 ) #cierra el bloque primaria pero incompleta
                ) #cierra el bloque educacion del jefe
                ) #cierra la creacion de la nueva variable
              ) %>% #cierra summarise
  mutate(prop = miembrosTotal / miembrosOcupados) %>%
  mutate(nbi5 = 
           ifelse(
             (prop == Inf & miembrosTotal >= 4) |                          # sin ocupados y mas de 3 en el hogar
               (prop != Inf & prop >= 4 & jefeBajaEduc == 1),
         1,
         0
         )
  ) %>%
  filter(nbi5==1) %>%
  select(idHogar)

hogares$nbi5[hogares$idHogar %in% personasNbi5$idHogar] = 1
rm(personasNbi5,baseJefatura)
hogares$nbi = as.numeric((hogares$nbi1 + hogares$nbi2 + hogares$nbi3 + hogares$nbi4 + hogares$nbi5)>0)

# Índice de Privación Material de los Hogares (IPMH)
source("src/ipmh.R")
#1 CONDHAB
hogares = left_join(hogares, condhab)
rm(condhab)

#2 para cada hogar CAPECO = (CP * VAE) / AE
hogaresCAPECO = 
  personas %>%
  group_by(idHogar) %>%
  summarise(capeco = sum(cp * vae)/sum(adultoEquivalente))

hogares = left_join(hogares, hogaresCAPECO)

capecoPersonas = select(hogares,idHogar,capeco)

personas = left_join(personas,capecoPersonas)

capecoPersonas = arrange(personas,capeco,desc(ipcf),idHogar)

rm(hogaresCAPECO)




#Años de escolaridad
# From questions ch12 and ch13, it's posible to get the same categories there are in CENSUS 
## para cada uno de los 810 hogares los años del jefe
## para cada hogar los años del conyuje


aniosEscolaridadJefe = 
  personas %>%
  filter(ch03=="jefe") %>%
  select(idHogar,aniosEscolaridad)
names(aniosEscolaridadJefe)[2] = "aniosEscolaridadJefe" 

aniosEscolaridadConyu = 
  personas %>%
  filter(ch03=="c\xf3nyuge/pareja") %>%
  select(idHogar,aniosEscolaridad)
names(aniosEscolaridadConyu)[2] = "aniosEscolaridadConyu"



hogares = left_join(hogares, aniosEscolaridadJefe)
hogares = left_join(hogares, aniosEscolaridadConyu)

rm(aniosEscolaridadJefe,aniosEscolaridadConyu)

#Regimen de posesión de la vivienda
#Homologar censo y eahu. Censo pregunta:
#La vivienda que ocupa este hogar, ¿es...
#ropia?
#alquilada?
#prestada?
#cedida por trabajo?
#Otra situación


#Propietario de la vivienda

hogares$propietario = as.numeric(hogares$ii7 == 1 |hogares$ii7 == 2 )



#TIPO DE CASA
#Calidad de los materiales (INMAT): 
#Calidad de conexión a servicios básicos (INCALSERV)
#Casa tipo B: la que cumple por lo menos una de las siguientes condiciones: no tiene provisión de agua por cañería dentro de la vivienda; no dispone de retrete con descarga de agua; tiene piso de tierra u otro material precario. 
#El resto de las casas es considerado como casas de tipo A. Rancho o casilla: el rancho (propio de áreas rurales) tiene generalmente paredes de adobe, piso de tierra y techo de chapa o paja. La casilla (propia de áreas urbanas) está habitualmente construida con materiales de baja calidad o desecho. Departamento: vivienda con baño y cocina propios, en la que se entra por patios, zaguanes, ascensores, escaleras o pasillos interiores de uso común. Casa de inquilinato: vivienda con salida independiente al exterior construida o remodelada deliberadamente para que tenga varios cuartos con salida a uno o más espacios de uso común. Algunas formas son conocidas como conventillos. Cada casa de inquilinato es una única vivienda en cuyo interior se reconocen los hogares particulares que la habitan. Pensión u hotel: vivienda en donde se alojan en forma permanente hogares particulares en calidad de pensionistas, bajo un régimen especial caracterizado por el pago mensual, quincenal o semanal de su alojamiento. Se incluyen los hoteles o pensiones no turísticos con capacidad menor de quince habitaciones en la Capital Federal y menor de diez en las provincias. Local no construido para habitación: lugar no destinado originariamente a vivienda, pero que estaba habitado el día del Censo. Vivienda móvil: la que puede transportarse a distintos lugares (barco, vagón de ferrocarril, casa rodante, etcétera). Viviendas deficitarias: incluye las casas tipo B y las viviendas precarias. Viviendas precarias: para permitir la comparabilidad entre ambos censos, se consideraron viviendas precarias en 1980 las piezas de inquilinato, los ranchos, las viviendas precarias y otros; en 1991, los ranchos o casillas, los hogares en casa de inquilinato, los locales no construidos para habitación y las viviendas móviles. No se consideraron los hoteles y pensiones. Vivienda sin cañería de agua dentro de la vivienda: incluye tanto a las viviendas cuya provisión de agua es exterior a las mismas pero dentro del lote en que fueron construidas, como a las viviendas que obtienen el agua fuera del terreno en que fueron construidas. Fuente: Situación y Evolución Social (Sintesis Nº4); INDEC.
#Algunas definiciones afines: Hogares con hacinamiento critico, Viviendas ocupadas y Viviendas particulares


#CALMAT


