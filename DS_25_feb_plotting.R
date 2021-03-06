library(tidyverse)
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
library(phyloseq)

metadata = read.table(file="Saanich.metadata.txt", header = TRUE, row.names=1, sep= "\t", na.strings= c("NAN", "NA", "."))

OTU = read.table(file="Saanich.OTU.txt", header = TRUE, row.names=1, sep= "\t", na.strings= c("NAN", "NA", "."))

load("phyloseq_object.RData")


ggplot(metadata, aes(x=O2_uM, y=Depth_m))

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point()

ggplot(metadata, aes(x=O2_uM, y=Depth_m, color="blue")) +
  geom_point()

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(color="blue")
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(shape="square")

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(size=10)

ggplot(metadata, aes(x=O2_uM, y=Depth_m, size=OxygenSBE_V)) +
  geom_point()

#ex1
ggplot(metadata, aes(x=NO3_uM, y=Depth_m)) +
  geom_point(shape=17, color = "purple")

#ex2
metadata %>% 
  select(matches("temp"))
metadata2 = metadata %>%
  mutate(Temperature_F = Temperature_C *9/5 +32) 


metadata2 %>%
  select(Temperature_F)

ggplot(metadata2, aes(x=Temperature_F, y=Depth_m)) +
  geom_point(shape=19, color = "purple")
###############
#phyloseq
plot_bar(physeq, fill="Phylum")

physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))

plot_bar(physeq_percent, fill="Phylum")

plot_bar(physeq_percent, fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity")

#ex3
plot_bar(physeq_percent, fill="Domain") + 
  geom_bar(aes(fill=Domain), stat="identity") +
  labs(title="Domains from 10 to 200m in Saanich Inlet", x = "Sample depth", y = "Percent relative anbundance")


#####
#faceting
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum)

plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum, scales="free_y") +
  theme(legend.position="none")

#ex4



faceted = gather(metadata, key = "Nutrient", value = "uM", NH4_uM, NO2_uM, NO3_uM, O2_uM, PO4_uM, SiO2_uM)

ggplot(faceted, aes(x=Depth_m, y=uM))+
  geom_line()+
  geom_point()+
  facet_wrap(~Nutrient, scales="free_y") +
  theme(legend.position="none")

  