############################################################################################################################################

#Community turnover difference in fish and fishless lakes
#1)All
dune.rel <- decostand(species_abun, "total")
Beta.sp = vegdist(dune.rel, method="bray",  binary=FALSE)

gx = new_env$Lat
gy = new_env$Lon
SPdata = data.frame(gx,gy)
row.names(SPdata) = row.names(new_env)
head(SPdata)
dim(SPdata)
xyD = dist(SPdata)

plot(xyD,Beta.sp)

#2)Fish
okay_fish<-okay%>%filter(actual_fish_presence == "Yes")

species_ab_fish<-okay_fish[c(1:2,46:82)]
new_env_fish<-okay_fish[,1:45]

species_ab_fish$lake_id<-as.integer(species_ab_fish$lake_id)
species_abun_fish<-species_ab_fish%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

dune.rel_fish <- decostand(species_abun_fish, "total")
Beta.sp_fish = vegdist(dune.rel_fish, method="bray",  binary=FALSE)

gx = new_env_fish$Lat
gy = new_env_fish$Lon
SPdata = data.frame(gx,gy)
row.names(SPdata) = row.names(new_env_fish)
head(SPdata)
dim(SPdata)
xyD_fish = dist(SPdata)

plot(xyD_fish,Beta.sp_fish)
dog<-lm(Beta.sp_fish~xyD_fish)
summary(dog)

#3)FIshless
okay_no_fish<-okay%>%filter(actual_fish_presence == "No")

species_ab_no_fish<-okay_no_fish[c(1:2,46:82)]
new_env_no_fish<-okay_no_fish[,1:45]

species_ab_no_fish$lake_id<-as.integer(species_ab_no_fish$lake_id)
species_abun_no_fish<-species_ab_no_fish%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

dune.rel_no_fish <- decostand(species_abun_no_fish, "total")
Beta.sp_no_fish = vegdist(dune.rel_no_fish, method="bray",  binary=FALSE)

gx = new_env_no_fish$Lat
gy = new_env_no_fish$Lon
SPdata = data.frame(gx,gy)
row.names(SPdata) = row.names(new_env_no_fish)
head(SPdata)
dim(SPdata)
xyD_no_fish = dist(SPdata)

plot(xyD_no_fish,Beta.sp_no_fish)
dog<-lm(Beta.sp_no_fish~xyD_no_fish)
summary(dog)

############################################################################################################################################
#Turnove rby lake drainage
evolution<-env_abund%>%
  filter(lake_drainage_name=="OWENS_ROCK_CK")

evo_env<-new_env%>%
  filter(lake_drainage_name=="OWENS_ROCK_CK")


evo_pivot_abund<-evolution%>%
  dplyr::select(c(Species_Name,Abundance,survey_date,lake_id))%>%
  pivot_wider(names_from = "Species_Name",values_from="Abundance")%>%
  unite("id_date",lake_id:survey_date)%>%
  #dplyr::select(-c(Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))


#1)All
dune.rel <- decostand(evo_pivot_abund, "total")
Beta.sp = vegdist(dune.rel, method="bray",  binary=FALSE)

gx = evo_env$Lat
gy = evo_env$Lon
SPdata = data.frame(gx,gy)
row.names(SPdata) = row.names(evo_env)
head(SPdata)
dim(SPdata)
xyD = dist(SPdata)

plot(xyD,Beta.sp)
dog<-lm(Beta.sp~xyD)
summary(dog)
abline(dog)

#2)Fish
okay_fish<-okay%>%
  filter(lake_drainage_name=="OWENS_ROCK_CK")%>%
  filter(actual_fish_presence == "Yes")

species_ab_fish<-okay_fish[c(1:2,46:82)]
new_env_fish<-okay_fish[,1:45]

species_ab_fish$lake_id<-as.integer(species_ab_fish$lake_id)
species_abun_fish<-species_ab_fish%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

dune.rel_fish <- decostand(species_abun_fish, "total")
Beta.sp_fish = vegdist(dune.rel_fish, method="bray",  binary=FALSE)

gx = new_env_fish$Lat
gy = new_env_fish$Lon
SPdata = data.frame(gx,gy)
row.names(SPdata) = row.names(new_env_fish)
head(SPdata)
dim(SPdata)
xyD_fish = dist(SPdata)

plot(xyD_fish,Beta.sp_fish)
dog<-lm(Beta.sp_fish~xyD_fish)
summary(dog)
abline(dog)

#3)FIshless
okay_no_fish<-okay%>%
  filter(lake_drainage_name=="OWENS_ROCK_CK")%>%
  filter(actual_fish_presence == "No")

species_ab_no_fish<-okay_no_fish[c(1:2,46:82)]
new_env_no_fish<-okay_no_fish[,1:45]

species_ab_no_fish$lake_id<-as.integer(species_ab_no_fish$lake_id)
species_abun_no_fish<-species_ab_no_fish%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

dune.rel_no_fish <- decostand(species_abun_no_fish, "total")
Beta.sp_no_fish = vegdist(dune.rel_no_fish, method="bray",  binary=FALSE)

gx = new_env_no_fish$Lat
gy = new_env_no_fish$Lon
SPdata = data.frame(gx,gy)
row.names(SPdata) = row.names(new_env_no_fish)
head(SPdata)
dim(SPdata)
xyD_no_fish = dist(SPdata)

plot(xyD_no_fish,Beta.sp_no_fish)
dog<-lm(Beta.sp_no_fish~xyD_no_fish)
summary(dog)
abline(dog)