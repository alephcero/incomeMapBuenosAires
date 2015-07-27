#this function takes level of oeducation (ch12) and whether the person finished it or not (ch13)
#and returns years of education

#Esc_1 es escolaridad primaria y puede asumir valores desde 1 a 7
#Esc_2 es escolaridad secundaria y puede asumir valores de 1 a 5
#Esc_3 es escolaridad superior y puede asumir valores de 1 a 5


#Asistencia ch10 ¿Asiste o asistió a algún establecimiento? si NO dice 1 o 2, 0 anios de escolaridad
#nivel ch12 el nivel, primario, secundario, etc. 
#completo ch13 si finalizo el nivel
#anio ch14 ultimo anio

#Fuente para años de escolaridad, es de 2001
#http://200.51.91.245/redarg/CENSOS/CPV2001ARG/docs/Definiciones%20CD%20Base%20CNPHV2001_d.pdf


#NO TIENEN DATO EN CH12 Y CH13, PERO EN NIVEL EDUC DICE ALGO. HAY QUE COTEJAR ESTO

aniosEscolaridadCAPECO = function(asistencia,nivel,completo,anio){
  anioIncompleto = as.integer(anio)
  anioIncompleto[anioIncompleto > 97] = 0
  anioIncompleto[completo == 9] = 0
  anios = NA
  if (asistencia != 1 & asistencia != 2) {
      anios = 0
    } else {
      #completo
      if (completo == 1) {
        #jardin
        if (nivel == 1){anios = 0}
        #primario
        if (nivel == 2){anios = 7}
        #EGB
        if (nivel == 3){anios = 9}
        #Secundario
        if (nivel == 4){anios = 12}
        #Polimodal
        if (nivel == 5){anios = 12}
        #Terciario
        if (nivel == 6){anios = 15}
        #Universitario
        if (nivel == 7){anios = 17}
        #Postuniversitario
        if (nivel == 8){anios = 17}
        #Especial
        if (nivel == 9){anios = 0}
        } else {
          #no completo, le sumo los anios al anterior
          #jardin
          if (nivel == 1){anios = 0}
          #primario
          if (nivel == 2){anios = 0 + anioIncompleto}
          #EGB
          if (nivel == 3){anios = 0 + anioIncompleto}
          #Secundario
          if (nivel == 4){anios = 7 + anioIncompleto}
          #Polimodal
          if (nivel == 5){anios = 7 + anioIncompleto}
          #Terciario
          if (nivel == 6){anios = 12 + anioIncompleto}
          #Universitario
          if (nivel == 7){anios = 12 + anioIncompleto}
          #Postuniversitario
          if (nivel == 8){anios = 17}
          #Especial
          if (nivel == 9){anios = 0}
          }
  }
  anios
}

#chequeo escolaridad EAHU
