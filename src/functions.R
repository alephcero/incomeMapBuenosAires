#Functions

pasarAutf8 = function(texto.factor) {
  etiquetas = levels(texto.factor)
  etiquetas = iconv(etiquetas, "latin1", "utf-8")
  texto.factor = iconv(texto.factor, "latin1", "utf-8")
  texto.factor = factor(texto.factor,levels = etiquetas)
  texto.factor
}


redatamFunction = "https://raw.githubusercontent.com/alephcero/REDATAMtoR/master/retadamFrequenciesToR.R"
redatamFunctionAverages = "https://raw.githubusercontent.com/alephcero/REDATAMtoR/master/redatamAveragesToR.R"

source_https <- function(url) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  script <- getURL(url, ssl.verifypeer = FALSE)
  eval(parse(text = script))
}

# Example

script <- getURL(redatamFunctionAverages, ssl.verifypeer = FALSE)
eval(parse(text = script))

script <- getURL(redatamFunction, ssl.verifypeer = FALSE)
eval(parse(text = script))