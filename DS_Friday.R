
library(tidyverse)


#to copy a document from materials into portfolio:
#cp ~/Documents/MICB425_materials/Saanich... ~/Documents/MICB_425_portfolio
read.table(file="Saanich.metadata.txt")




metadata = read.table(file="Saanich.metadata.txt", header = TRUE, row.names=1, sep= "\t", na.strings= c("NAN", "NA", "."))

OTU = read.table(file="Saanich.OTU.txt", header = TRUE, row.names=1, sep= "\t", na.strings= c("NAN", "NA", "."))
                      
#02.02.2018
library(tidyverse)
# %>% pushes data or so in the function after the %>% -> eg data %>% function -> this is the same as function(data)

oxygen = metadata %>% select(O2_uM)
#selects oxygen concetntration column from metadata table, safes it in oxygen var.

oxygentotal = metadata %>% select(matches("O2|oxygen")) #selects everyhing with O2 or oxygen in title..

#select is for columns
#filter is for rows

#filter rows (samples) were oxygen == 0:

metadata %>%
  filter(O2_uM == 0)

#now combine selection and filter:

metadata %>%
  filter(O2_uM == 0) %>%
  select(Depth_m)
#shows all depths where there is no oxygen present


#using dplyr, find at what depth(s) methane is above 100 nM while temperature is below 10 degrees.  print out a table showing only the depth, methane and temperature data

metadata %>%
  filter(CH4_nM > 100 & Temperature_C < 10) %>%
  select(Depth_m & CH4_nM & Temperature_C)



#solution together:
#first find names of the variables..

metadata %>% 
  select(matches("CH4|methane"))
#std means std. deviation

metadata %>% 
  select(matches("Temp"))

#-> variables are CH4_nM and Temperature_C

#metadata %>%
# filter(CH4_nM > 100) %>%
# filter(Temperature < 10) would also work, but longer..

metadata %>%
  filter(CH4_nM > 100 , Temperature_C < 10) %>% # a , instead of & would also work
  select(Depth_m, CH4_nM, Temperature_C)
####################

metadata %>%
  mutate(N2O_uM = N2O_nM/1000) %>%
select(N2O_uM, N2O_nM)
#adds new column with N2O in uM concentration instead of nM
# a new column is generated!!! not the old column overwritten


#Convert all varaibles that are in nM to uM. output a table showing only the original nM and converted uM variables
metadata %>% 
  select(matches("nM"))
metadata %>%
  mutate(N2O_uM = N2O_nM/1000 , Std_N2O_uM = Std_N2O_nM/1000 , CH4_uM = CH4_nM/1000 , Std_CH4_uM = Std_CH4_nM/1000) %>%
  select(N2O_nM, N2O_uM, Std_N2O_nM, Std_N2O_uM, CH4_nM, CH4_uM, Std_CH4_nM, Std_CH4_uM)
