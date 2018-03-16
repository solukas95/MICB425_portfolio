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


####ASV
load("~/MICB425_materials/Module_03/Project1/data/qiime2_phyloseq.RData")
