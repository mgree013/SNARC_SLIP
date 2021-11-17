############################################################################################################################################
#Indicator Species analysis
okay<-env_abund%>%pivot_wider(names_from = Species_Name, values_from=Abundance)

species_ab<-okay[c(1:2,46:82)]
new_env<-okay[,1:45]

species_ab$lake_id<-as.integer(species_ab$lake_id)
species_abun<-species_ab%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

dune.rel <- decostand(species_abun, "total")

dune.ind = multipatt(dune.rel, new_env$actual_fish_presence, func="IndVal.g", control = how(nperm=999))
summary(dune.ind)
summary(dune.ind, alpha=1)

dune.ind2 = multipatt(dune.rel, new_env$actual_fish_presence, func="IndVal.g", duleg=TRUE, control = how(nperm=999))
summary(dune.ind2, alpha=1)