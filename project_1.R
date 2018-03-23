load("~/MICB425_materials/Module_03/Project1/data/mothur_phyloseq.RData")
library(tidyverse)
library(phyloseq)
mothur_percent = transform_sample_counts(mothur, function(x) 100 * x/sum(x))


plot_bar(mothur_percent, fill="Class") + 
  geom_bar(aes(fill=Class), stat="identity") +
  labs(title="Class from 10 to 200m in Saanich Inlet", x = "Sample depth", y = "Percent relative anbundance")

plot_bar(mothur_percent, fill="Class") +
  geom_bar(aes(fill=Class), stat="identity") +
  facet_wrap(~Class, scales="free_y") +
  theme(legend.position="none")

gp = subset_taxa(mothur, Family== "Ectothiorhodospiraceae") 
plot_bar(gp, fill="Genus")


gp2 = subset_taxa(mothur, Genus== "Candidatus_Scalindua") 
plot_bar(gp2, fill="Genus")

library(tidyverse)
mothur_taxa=data.frame(mothur@tax_table) 
mothur_otu=data.frame(mothur@otu_table) 
mothur_taxa_t=t(mothur_taxa) 
total=rbind(mothur_otu, mothur_taxa_t) 
total_t=t(total) 
total_t2=data.frame(total_t) 
total_scalindua=total_t2%>%filter(Genus== "Candidatus_Scalindua")
total_scalindua

#Setting seed for our group
set.seed(9376)

#Rarefying
m.norm = rarefy_even_depth(mothur, sample.size=100000)

#Transforming to Abundance Perc.
m.perc = transform_sample_counts(m.norm, function(x) 100 * x/sum(x))

#estimating richness
m.alpha = estimate_richness(m.norm, measures = c("Chao1", "Shannon"))

#combining alpha diversity data with biogeochemical data
m.meta.alpha = full_join(rownames_to_column(m.alpha), rownames_to_column(data.frame(m.perc@sam_data)), by = "rowname")

m.meta.alpha %>%  
  ggplot() +
  geom_point(aes(x=Depth_m, y=Shannon, colour= "Shannon Diversity")) +
  geom_smooth(method='auto', aes(x=as.numeric(Depth_m), y=Shannon)) +
  labs(title="Alpha-diversity across depth", y="Shannon's diversity index", x="Depth (m)") +
  geom_line(aes(x=Depth_m, y=O2_uM/15, colour="O2_uM")) +
  geom_point(aes(x=Depth_m, y=O2_uM/15, colour="O2_uM"))+
  scale_y_continuous(sec.axis = sec_axis(~.*(15), name = "O2 (uM)")) +
  scale_colour_manual(values = c("blue", "red"))+
  labs(title="Alpha-diversity and Oxygen across depth", y = "Shannon’s diversity index" , x = "Depth (m)" , colour = "Parameter") +
  theme(legend.position = c(0.8, 0.9))


library(magrittr)
m.meta.alpha %>%
  ggplot() +
  geom_point(aes(x=O2_uM, y=Shannon)) +
  geom_smooth(method='lm', aes(x=as.numeric(O2_uM), y=Shannon)) +
  labs(title="Alpha diversity by Oxygen concentration")

m.norm %>% 
  psmelt() %>% 
  filter(OTU=="Otu2319") %>% 
  
  lm(Abundance ~ Depth_m, .) %>% 
  summary()

#Linear model: Abundance Against NH4
m.perc %>%
  subset_taxa(Genus=="Candidatus_Scalindua") %>%
  psmelt() %>%
  group_by(Sample) %>%
  summarize(Abundance_sum=sum(Abundance), NH4_uM=mean(NH4_uM)) %>%
  #lm(Abundance_sum ~ NH4_uM, .) %>% 
  #summary()
  ggplot() +
  geom_point(aes(x=NH4_uM, y=Abundance_sum)) +
  geom_smooth(method='lm', aes(x=as.numeric(NH4_uM), y=Abundance_sum)) +
  labs(title="Abundance Candidatus Scalindua across ammonium.")
  

m.norm %>% 
  psmelt() %>% 
  filter(OTU=="Otu1755") %>% 
  
  lm(Abundance ~ NH4_uM, .) %>% 
  summary()


#OTus across NH4
m.perc %>%
  subset_taxa(Genus == "Candidatus_Scalindua") %>%
  psmelt() %>%
  

  ggplot() +
  geom_point(aes(x=NH4_uM, y=Abundance)) +
  geom_smooth(method='lm', aes(x=NH4_uM, y=Abundance)) +
  facet_wrap(~OTU, scales="free_y") +
  labs(title="Abundance of OTUs within Genus Candidatus Scalindua domain across NH4 conc.")

#Example: Abundance of OTU1755 across Oxygen conc.

m.perc %>%
  subset_taxa(Genus == "Candidatus_Scalindua") %>%
  psmelt() %>%
  filter(OTU=="Otu1755") %>%
  
  ggplot() +
  geom_point(aes(x=NH4_uM, y=Abundance)) +
  geom_smooth(method='lm', aes(x=NH4_uM, y=Abundance)) +
  facet_wrap(~OTU, scales="free_y") +
  labs(title="Abundance of OTU1755 across NH4 conc..")

##NO2
#Linear model: Abundance Against NO2
m.perc %>%
  subset_taxa(Genus=="Candidatus_Scalindua") %>%
  psmelt() %>%
  group_by(Sample) %>%
  summarize(Abundance_sum=sum(Abundance), NO2_uM=mean(NO2_uM)) %>%
  

  ggplot() +
  geom_point(aes(x=NO2_uM, y=Abundance_sum)) +
  geom_smooth(method='lm', aes(x=as.numeric(NO2_uM), y=Abundance_sum)) +
  labs(title="Abundance Candidatus Scalindua across NO2.")

m.norm %>% 
  psmelt() %>% 
  filter(OTU=="Otu1755") %>% 
  
  lm(Abundance ~ NH4_uM, .) %>% 
  summary()


#OTus across NO2
m.perc %>%
  subset_taxa(Genus == "Candidatus_Scalindua") %>%
  psmelt() %>%
  
  
  
  ggplot() +
  geom_point(aes(x=NO2_uM, y=Abundance)) +
  geom_smooth(method='lm', aes(x=NO2_uM, y=Abundance)) +
  facet_wrap(~OTU, scales="free_y") +
  labs(title="Abundance of OTUs within Genus Candidatus Scalindua domain across NO2 conc.")

#Example: Abundance of OTU1755 across NO2 conc.

m.perc %>%
  subset_taxa(Genus == "Candidatus_Scalindua") %>%
  psmelt() %>%
  filter(OTU=="Otu1755") %>%
  
  ggplot() +
  geom_point(aes(x=NO2_uM, y=Abundance)) +
  geom_smooth(method='lm', aes(x=NO2_uM, y=Abundance)) +
  facet_wrap(~OTU, scales="free_y") +
  labs(title="Abundance of OTU1755 across NO2 conc..")

####ASV
load("~/MICB425_materials/Module_03/Project1/data/qiime2_phyloseq.RData")
