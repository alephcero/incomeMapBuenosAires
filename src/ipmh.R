#Construccion IPMH = CONDHAB + CAPECO

#1 CONDHAB
condhab = select(hogares,
                 idHogar,
                 iv3,                      # Material predominante de los pisos interiores
                 iv4,                      # Material predominante de la cubierta externa de los techos
                 iv5,                      # Cielo raso
                 iv8,                      # ¿Tiene baño / letrina?
                 iv10                      # Arrastre de agua del baño
                 )
#1.1) Indicador de Materiales constructivos de la vivienda (MATCO)
#iv3  Material predominante de los pisos interiores



condhab$matcoPiso = NA
#- SUFICIENTE (S): baldosa, cerámica, plástico, madera, mármol, goma, alfombra o similares
suficiente = condhab$iv3 == "mosaico/baldosa/madera/cer\xe1mica/alfombra"
condhab$matcoPiso[suficiente] = 2 

#- PARCIALMENTE INSUFICIENTE (PI): cemento o ladrillo, y otros
parcialmenteInsuficiente = condhab$iv3 == "cemento/ladrillo fijo"
condhab$matcoPiso[parcialmenteInsuficiente] = 1

#- INSUFICIENTE (I): tierra.
insuficiente = condhab$iv3 == "ladrillo suelto/tierra" | condhab$iv3 == "otro" | condhab$iv3 == "ns./nr."
condhab$matcoPiso[insuficiente] = 0

condhab$matcoPiso = factor(condhab$matcoPiso,levels = 0:2,
                           labels = c("INSUFICIENTE","PARCIALMENTE INSUFICIENTE","SUFICIENTE"))

# Material predominante de la cubierta externa de los techos
#Presencia de revestimiento o Cielorraso en la parte interior de los techos
#iv4 Material predominante de la cubierta externa de los techos
#iv5 Cielo raso

condhab$matcoTecho = NA 


#- PARCIALMENTE INSUFICIENTE (PI): teja, baldosa, membrana o capa asfáltica sin cielorraso
parcialmenteInsuficiente = condhab$iv4 == "membrana/cubierta asf\xe1ltica" |
  condhab$iv4 == "baldosa/losa sin cubierta" |
  condhab$iv4 == "pizarra/teja"
condhab$matcoTecho[parcialmenteInsuficiente] = 1


#- INSUFICIENTE (I): madera, cartón, paja o desechos y chapa de metal o fibrocemento sin
#cielorraso y otros sin cielorraso
insuficiente = condhab$iv4 == "chapa de metal sin cubierta" |
  condhab$iv4 == "chapa de fibrocemento/pl\xe1stico" |
  condhab$iv4 == "chapa de cart\xf3n" |
  condhab$iv4 == "ca\xf1a/tabla/paja con barro/paja sola"
condhab$matcoTecho[insuficiente] = 0

#- SUFICIENTE (S): teja, baldosa, membrana o capa asfáltica con cielorraso y chapa de
#metal o fibrocemento con cielorraso y otros con cielorraso
#Esta condicion va l final ya que con solo tener cielo raso va a suficiente
suficiente = condhab$iv5 == "s\xed" | condhab$iv4 == "n/s. depto. de propiedad horizontal"
condhab$matcoTecho[suficiente] = 2


condhab$matcoTecho = factor(condhab$matcoTecho,levels = 0:2,
                           labels = c("INSUFICIENTE","PARCIALMENTE INSUFICIENTE","SUFICIENTE"))



#Se considera que la calidad de los materiales de la vivienda es:

condhab$matco = NA

#Suficiente: si la calidad del techo y el piso es suficiente.
suficiente = condhab$matcoPiso == "SUFICIENTE" & condhab$matcoTecho == "SUFICIENTE"

#Parcialmente Insuficiente: si al menos una de las partes constitutivas de la vivienda es de calidad parcialmente insuficiente.
parcialmenteInsuficiente = condhab$matcoPiso == "PARCIALMENTE INSUFICIENTE" | condhab$matcoTecho == "PARCIALMENTE INSUFICIENTE"

#Insuficiente: si la calidad del piso o el techo es insuficiente.
insuficiente = condhab$matcoPiso == "INSUFICIENTE" | condhab$matcoTecho == "INSUFICIENTE"



condhab$matco[suficiente] = 2
condhab$matco[parcialmenteInsuficiente] = 1
condhab$matco[insuficiente] = 0

condhab$matco = factor(condhab$matco,levels = 0:2,
                            labels = c("INSUFICIENTE","PARCIALMENTE INSUFICIENTE","SUFICIENTE"))

#NORMA MATCO: Parcialmente insuficiente


#1.2) Indicador de Condiciones Sanitarias de la vivienda
#iv8  ¿Tiene baño / letrina?
#iv10 Arrastre de agua del baño

condhab$consan = NA

#SUFICIENTE: si el hogar dispone de instalación de baño con descarga de agua en el inodoro.
suficiente = condhab$iv8 == "s\xed" & condhab$iv10 != 3

#INSUFICIENTE: si el hogar no dispone de instalación de baño con descarga de agua en el inodoro.
insuficiente = condhab$iv8 == "no" | condhab$iv10 == 3

condhab$consan[suficiente] = 1
condhab$consan[insuficiente] = 0


condhab$consan = factor(condhab$consan,levels = 0:1,
                            labels = c("INSUFICIENTE","SUFICIENTE"))


#NORMA CONSAN: Suficiente

#1.3) Integración de ambos indicadores en Condiciones Habitacionales (CONDHAB)

#Se considera que la vivienda presenta condiciones habitacionales INSUFICIENTES cuando 
#sus materiales constructivos o sus instalaciones sanitarias se encuentran por debajo de la norma.

#NORMA CONSAN: Suficiente
#NORMA MATCO: Parcialmente insuficiente

#Esto haria que condhab tenga 2 niveles, suficiente y no, y luego usar capeco como booleana con suficiente e insuficiente
# Aca decido no hacer esto, sino mantener condhab con los niveles de matco y cambiar a insuficente cuando consan lo sea

condhab$condhab = condhab$matco
condhab$condhab[condhab$consan == "INSUFICIENTE"] = "INSUFICIENTE"
  
condhab = select(condhab,-(iv3:iv10))

#2 CAPECO = (CP * VAE) / AE

#2.1 CP solo me preocupa CABA

#el cp en censo nivel radio va a sobre estimar el CAPECO porque le voy a asignar jubilacion a todos los que tenga 65 o 60 segun sexo

#estado es ocupacion
#ch04 sexo
#ch06 edad
#estado 

condicionPercepcion = function(edad,sexo,estado){
  cp = NA
  #inactivo
  if (estado == "inactivo") {
    #mujer
    if (sexo == "mujer") {
      if(edad >= 60) {
        cp = 0.35
      } else {cp = 0}
    } else {#varon
      if(edad >= 65) {
        cp = 0.5
    } else {cp = 0}
    }
  #ocupado  
  } else if (estado == "ocupado") {
    #mujer
    if (sexo == "mujer") {
      if (edad >= 14 & edad <= 24){cp = 0.33}
      else if (edad >= 25 & edad <= 34){cp = 0.54}
      else {cp = 0.6}
    } else {#varon
      if (edad >= 14 & edad <= 24){cp = 0.46}
      else if (edad >= 25 & edad <= 34){cp = 0.83}
      else {cp = 1}
    }
  #desocupado o menor de 10años
  } else {
    cp = 0
  }
  cp
}



#hacer el loop y chequear

personas$cp = NA

for (i in 1:nrow(personas)) {
#for (i in 1:10) {
  personas$cp[i] = condicionPercepcion(edad = personas$ch06[i],sexo = personas$ch04[i],estado = personas$estado[i])
}

#hay 244 inactivos mujeres con cp NA

#2.2 VAE
source("src/schoolYears.R")

#Establezco los años de escolaridad
#NO TIENEN DATO EN CH12 Y CH13, PERO EN NIVEL EDUC DICE ALGO. HAY QUE COTEJAR ESTO

personas$aniosEscolaridad = NA

for (i in 1:nrow(personas)) {
  personas$aniosEscolaridad[i] = aniosEscolaridadCAPECO(personas$ch10[i],personas$ch12[i],personas$ch13[i],personas$ch14[i])
}
personas$aniosEscolaridad[personas$aniosEscolaridad>17] = 17

#Hay 3 casos que no saben el nivel al que asistieron o si lo terminaron pero en la variable Nivel_ed aparece dato. 

#a secundario incompleto le doy 8 años
personas$aniosEscolaridad[is.na(personas$aniosEscolaridad) & personas$nivel_ed == "secundaria incompleta" ] = 8
#a primario incompleto le doy 7 años
personas$aniosEscolaridad[is.na(personas$aniosEscolaridad) & personas$nivel_ed == "primaria completa" ] = 7

#Estos son los casos, con componente de hogar 1
#View(personas[personas$codusu == 261209 | personas$codusu == 311871,])



#En funcion de los años de escolaridad, le voy un Valor de Años de Escolaridad (VAE)
vae = function(aniosEscolaridad){
  vae = data.frame (aniosEscolaridad = 0:17,
                    vae= c(4,4.4,4.7,5.1,5.5,6,6.5,7,7.7,8.4,9.2,10.1,11.1,12.6,14.4,16.4,18.6,21.2)
  )
  vae$vae[aniosEscolaridad == vae$aniosEscolaridad]
}

personas$vae = NA

for (i in 1:nrow(personas)) {
  personas$vae[i] = vae(personas$aniosEscolaridad[i])
}


#2.3 AE

adultoEquivalente = function(sexo,edad){
  ae = NA
  if (edad < 1) {ae = 0.33}
  if (edad == 1) {ae = 0.43}
  if (edad == 2) {ae = 0.50}
  if (edad == 3) {ae = 0.56}
  if (edad >= 4 & edad <= 6) {ae = 0.63}
  if (edad >= 7 & edad <= 9) {ae = 0.72}
  if (edad >= 10 & edad <= 12 & sexo == "varon") {ae = 0.83}
  if (edad >= 13 & edad <= 15 & sexo == "varon") {ae = 0.96}
  if (edad >= 16 & edad <= 17 & sexo == "varon") {ae = 1.05}
  if (edad >= 10 & edad <= 12 & sexo == "mujer") {ae = 0.73}
  if (edad >= 13 & edad <= 15 & sexo == "mujer") {ae = 0.79}
  if (edad >= 16 & edad <= 17 & sexo == "mujer") {ae = 0.79}
  if (edad >= 18 & edad <= 29 & sexo == "varon") {ae = 1.06}
  if (edad >= 30 & edad <= 59 & sexo == "varon") {ae = 1.00}
  if (edad >= 60 & sexo == "varon") {ae = 0.82}
  if (edad >= 18 & edad <= 29 & sexo == "mujer") {ae = 0.74}
  if (edad >= 30 & edad <= 59 & sexo == "mujer") {ae = 0.74}
  if (edad >= 60 & sexo == "mujer") {ae = 0.64}
  ae
}

personas$adultoEquivalente = NA
for (i in 1:nrow(personas)) {
  personas$adultoEquivalente[i] = adultoEquivalente(personas$ch04[i],personas$ch06[i])
}




