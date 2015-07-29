# Este scrip toma las bases descargadas de la EAHU de Hogares y personas en formato .dta de STATA, selecciona las variables presentes en el Censo, 
# genera una variable única de identificaciòn (idHogar) y modifica las categorías de respuesta con problemas de encodign

#Files are to be downloaded from "http://www.indec.gov.ar/ftp/cuadros/menusuperior/eahu/EAHU_2010_dta.zip"
#in Stata format into a folder named "bases".

hogares = read.dta("data/EAHU/Hogar_EAHU_10.dta")                #leer base Hogares
personas = read.dta("data/EAHU/Individual_EAHU_10.dta")          #leer base Personas

#Seleccionar los casos de capital federal
hogares = hogares[hogares$jurisdiccion == 'ciudad de buenos aires',]
personas = personas[personas$jurisdiccion == 'ciudad de buenos aires',]

###############################################################################################
# Dejar las variables comparables con el CENSO
###############################################################################################


#Explicitar las variables a mantener

#HOGARES
  ##iv7 all cases have "agua corriente", thas's why it's not included in the analysis 
variablesHogaresMantener = c('codusu',                
                             'nro_hogar',
                             'pondera',
                             'iv1',
                             'iv2',
                             'iv3',
                             'iv4',
                             'iv5',
                             'iv6',
                             'iv7',
                             'iv8',
                             'iv9',
                             'iv10',
                             'iv11',
                             'ii1',
                             'ii2',
                             'ii7',
                             'ii8',
                             'ipcf',
                             'ix_tot',
                             'prodeccfr'                             
                             )

hogares = hogares[,variablesHogaresMantener]
rm(variablesHogaresMantener)

#PERSONAS
  ##ch15  CENSO solo toma booleana argentina u otro
variablesPersonasMantener = c('codusu',
                              'nro_hogar',
                              'componente',
                              'pondera',
                              'ch03',
                              'ch04',
                              'ch06',
                              'ch09',
                              'ch10',
                              'ch12',
                              'ch13',
                              'ch14',
                              'ch15',
                              'nivel_ed',
                              'estado',
                              'cat_ocup',
                              'cat_inac',
                              'p47t',
                              'prodecindr',
                              'ipcf',
                              'prodeccfr',
                              'idimpp'
                              )                             

personas = personas[,variablesPersonasMantener]
rm(variablesPersonasMantener)

#Crear una variable unica idHogar 
personas$idHogar = paste(personas$codusu,personas$nro_hogar,sep='-')
personas$idHogar = factor(personas$idHogar)


hogares$idHogar = paste(hogares$codusu,hogares$nro_hogar,sep='-')
hogares$idHogar = factor(hogares$idHogar)

#Eliminar codusu y nro_hogar
#personas = select(personas,-(codusu:nro_hogar))
#hogares = select(hogares,-(codusu:nro_hogar))

#RECODIFICACIÓN DE VARIABLES CON PROBLEMAS DE ENCODING

#Hogares
pasarAutf8 = function(texto.factor) {
  etiquetas = levels(texto.factor)
  etiquetas = iconv(etiquetas, "latin1", "utf-8")
  texto.factor = iconv(texto.factor, "latin1", "utf-8")
  texto.factor = factor(texto.factor,levels = etiquetas)
  texto.factor
} 

hogares$iv1 = pasarAutf8(hogares$iv1)

hogares$iv2[hogares$iv2=="99"] = NA

hogares$iv3 = pasarAutf8(hogares$iv3)

hogares$iv4 = pasarAutf8(hogares$iv4)

hogares$iv5 = pasarAutf8(hogares$iv5)

hogares$iv6 = pasarAutf8(hogares$iv6)

hogares$iv7 = pasarAutf8(hogares$iv7)

hogares$iv8 = pasarAutf8(hogares$iv8)

hogares$iv9[hogares$iv9=="0"] = NA

hogares$iv9 = factor(hogares$iv9,levels = 1:3,labels = c("Dentro de la vivienda",
                                                         "Fuera de la vivienda pero dentro del terreno",
                                                         "Fuera del terreno"))

hogares$iv10[hogares$iv10=="0"] = NA

hogares$iv10 = factor(hogares$iv10,levels = 1:3,labels = c("Inodoro con botón / mochila / cadena y arrastre de agua",
                                                           "Inodoro sin botón / cadena y con arrastre de agua (a balde)",
                                                           "Letrina (sin arrastre de agua)"))


hogares$iv11[hogAares$iv11=="0"] = NA

hogares$iv11 = factor(hogares$iv11,levels = 1:4,labels = c("A red pública (cloaca)",
                                                           "A cámara séptica y pozo ciego",
                                                           "Sólo a pozo ciego",
                                                           "A hoyo / excavación en la tierra"))



hogares$ii7[hogares$ii7=="0"] = NA

hogares$ii7 = factor(hogares$ii7,levels = 1:9,labels = c("Propietario de la vivienda y el terreno",
                                                         "Propietario de la vivienda solamente",
                                                         "Inquilino / arrendatario de la vivienda",
                                                         "Ocupante por pago de impuestos / expensas",
                                                         "Ocupante en relación de dependencia",
                                                         "Ocupante gratuito (con permiso)",
                                                         "Ocupante de hecho (sin permiso)",
                                                         "Está en sucesión",
                                                         "Otra situación"
                                                         ))

hogares$ii8[hogares$ii8=="0"] = NA

hogares$ii8 = factor(hogares$ii8,levels = 1:4,labels = c("Gas de red",
                                                           "Gas de tubo / garrafa",
                                                           "Kerosene / leña/ carbón",
                                                           "Otro"))




#Personas
#Sexo
personas$ch04 = pasarAutf8(personas$ch04)

