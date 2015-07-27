rm(list=ls())
options(encoding="utf-8")
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

#Recodifico varones en sexo
personas$ch04 = as.character(personas$ch04)
personas$ch04[personas$ch04=="var\xf3n"] = "varon"
#Ordeno por idHogar
personas = personas[order(personas$idHogar),]
hogares = hogares[order(hogares$idHogar),]
