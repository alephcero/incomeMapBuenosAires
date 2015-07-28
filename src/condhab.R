#INDICADOR CONDHAB
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