#Read capeco averages
for (i in 1:15){
  assign(
    paste("capecoComuna",i,sep=""),
    redatamAverages(paste("data/CENSO/capeco/capecoPorComuna/capecoComuna",i,"RadiosASCII.txt",sep="")
                    )
    )
}

capecoFinal = rbind(capecoComuna1,
                    capecoComuna2,
                    capecoComuna3,
                    capecoComuna4,
                    capecoComuna5,
                    capecoComuna6,
                    capecoComuna7,
                    capecoComuna8,
                    capecoComuna9,
                    capecoComuna10,
                    capecoComuna11,
                    capecoComuna12,
                    capecoComuna13,
                    capecoComuna14,
                    capecoComuna15
                    )

rm(capecoComuna1,
   capecoComuna2,
   capecoComuna3,
   capecoComuna4,
   capecoComuna5,
   capecoComuna6,
   capecoComuna7,
   capecoComuna8,
   capecoComuna9,
   capecoComuna10,
   capecoComuna11,
   capecoComuna12,
   capecoComuna13,
   capecoComuna14,
   capecoComuna15
  )
