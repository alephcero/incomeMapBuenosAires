#BA income map 
# This script takes CAPECO data set from Argentine 2010 Households and Population Census, exported as ASCII from READATAM software with averages per RADIOS (the minimun areabreak posible), 
# and returns a map of the spacial distribution for this index as well as the data table with the information.

#source libraries
source("src/libraries.R")

#source function for read REDATAM data
redatamFunctionAverages = "https://raw.githubusercontent.com/alephcero/REDATAMtoR/master/redatamAveragesToR.R"
script <- getURL(redatamFunctionAverages, ssl.verifypeer = FALSE)
eval(parse(text = script))

#Read capeco averages for each Comune
for (i in 1:15){
  if(i ==1){
    capeco = redatamAverages(
      paste("data/CENSO/capeco/capecoPorComuna/capecoComuna",i,"RadiosASCII.txt",sep="")
      )
  }else{
    capeco = rbind(capeco,
                   redatamAverages(
                     paste("data/CENSO/capeco/capecoPorComuna/capecoComuna",i,"RadiosASCII.txt",sep="")
                   )
    )
  }
}
#write table into csv file
write.csv(capeco,"data/capeco.csv",row.names = FALSE)

#Read shapes for BA map
BAmap = readOGR(dsn = "maps/radiosBA", layer = "radios_censo_2010")

#Join shapes with capeco data
BAmap@data = left_join(BAmap@data,capeco)

#Generate income map
map = qtm(BAmap, "capeco",fill.palette="Greens")+
  tm_layout(scale = 0.8,legend.position = c("right","bottom")) 

map

#Save map into a png file in the Figures directory
dev.copy(png,"fig/incomeMapBuenosAires.png",
         width = 1024, height = 768, res = 80)
dev.off()

