
library(tidyverse)


#to copy a document from materials into portfolio:
#cp ~/Documents/MICB425_materials/Saanich... ~/Documents/MICB_425_portfolio
read.table(file="Saanich.metadata.txt")




metadata = read.table(file="Saanich.metadata.txt", header = TRUE, row.names=1, sep= "\t", na.strings= c("NAN", "NA", "."))

OTU = read.table(file="Saanich.OTU.txt", header = TRUE, row.names=1, sep= "\t", na.strings= c("NAN", "NA", "."))
                      

           