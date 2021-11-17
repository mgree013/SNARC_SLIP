##This script is for downloading and data exploition inital of SLIP
#Link to EDI data portal: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=577&revision=2
# Package ID: edi.577.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: The Sierra Lakes Inventory Project: Non-Native fish and community composition of lakes and ponds in the Sierra Nevada, California.

#Matthew Green 
#31 Dec 2020

library(tidyverse)
library(ggplot2)
library(vegan)
library(reshape2)
library(reshape)
library(dplyr)
library(adespatial)
library(indicspecies)
library(FD)
library(leaflet)
library(viridis)
library(mapview)
library(sf)

#Zoop INitial Data exploration
#1) Format data and get abundances
#dt10,dt11,dt12,dt13

#Change from Species ID to Name

zoopz<-dt10%>%
  left_join(dt12, by = "SpeciesID")

dt8.1<-dt8%>%
  dplyr::select(c(lake_id,survey_date,lake_max_depth,zoo_tow_number,zoo_tow_type,zoo_tow_depth))

zoopz<-zoopz%>%
  left_join(dt8.1,by=c("lake_id", "survey_date"))%>%
  group_by(survey_date,lake_id)%>%
  mutate(volume=(3.14*0.295*zoo_tow_depth)/4,zoop_count=Number, zoop_abund=zoop_count/volume)%>%
  mutate(Species_Name = str_replace(Species_Name, " ", "_"))%>%
  mutate(Species_Name = str_replace(Species_Name, "/", "_"))%>%
  filter(Species_Name != "standard_measurement")

clean_zoop<-zoopz%>%
  dplyr::select(c(lake_id,survey_date,Subsample,Species_Name,zoop_abund))

#Fix subsample
clean_zoopzz<-clean_zoop%>%
  group_by(lake_id,survey_date)%>%
  mutate(Max.Subsample=max(Subsample))%>%
  ungroup()%>%
  group_by(lake_id,survey_date,Species_Name)%>%
  transmute(Abundance=sum(zoop_abund)/Max.Subsample)%>%
  distinct(lake_id,survey_date,Species_Name,Abundance)

#Species abundance Matrix
site.sp.quad <- cast(clean_zoopzz, lake_id+survey_date ~ Species_Name, value='Abundance')
site.sp.quad <- as.data.frame(site.sp.quad)
site.sp.quad[is.na(site.sp.quad)] <- 0

#Species Diversity as Response Varible

species<-as.data.frame(site.sp.quad[,3:39])

diversity<-species%>%
  transmute(N0=rowSums(species > 0),H= diversity(species),N1 =exp(H),N2 =diversity(species, "inv"),J= H/log(N0),E10= (N1/N0),E20= (N2/N0),Com.Size=rowSums(species),betas.LCBD=beta.div(species, method="hellinger",sqrt.D=TRUE)$LCBD ,betas.LCBD.p=beta.div(species, method="chord",sqrt.D=TRUE)$p.LCBD )

#Add site info and treatments to diversity data
local_diversity<-cbind(diversity, site.sp.quad[,1:2])



###################################################################################################################################################################################################################################################################################################################################
#Environmental DataSet

#Add in GPS coords from LAKEID
#Using sieera_lakes arc GIS export to excel
gps<-read.csv("sierra_lakes.csv")

dt5.1<-dt5%>%
  pivot_wider(names_from = littoral_type, names_glue = "{littoral_type}_{.value}",values_from=littoral_amount)
  
dt6.1<-dt6%>% 
  pivot_wider(names_from = shoreline_type,names_glue = "{shoreline_type}_{.value}", values_from=shoreline_amount)


env<-dt5.1%>%
  left_join(dt6.1, by= c("lake_id", "survey_date"))%>%
  left_join(dt4, by= "lake_id")%>%
  left_join(dt8, by= c("lake_id", "survey_date"))%>%
  filter(zoo_sample_ind=="Yes")%>%
  dplyr::select(-c(zoo_sample_ind,zoo_sample_time,zoo_tow_number,zoo_tow_type,zoo_tow_number,zoo_tow_depth,
                   benthic_sample_ind,benthic_sample_percent,nbr_benthic_sweeps,
                   lake_fairy_shrimp_ind,lake_shrimp_collection,pool_fairy_shrimp_ind,pool_shrimp_collection,
                   amphib_survey_starttime,amphib_survey_endtime,amphib_survey_duration,amphib_survey_desc,amphib_survey_fish_presence,
                   fish_survey_type, fish_net_location_type,fish_net_set_datetime,fish_net_pull_datetime))%>%
  left_join(gps, by="lake_id")

#SUbsamples? couts?
###################################################################################################################################################################################################################################################################################################################################

env_div<-left_join(env,local_diversity, by=c("lake_id", "survey_date"))%>%filter(lake_id !="11505" & lake_id !="42219" &lake_id !="71257" &lake_id !="71282" )

env_div%>%
  gather(N0,H,N2,N1,betas.LCBD,E10,key = "var", value = "value")%>% 
  ggplot(aes(x=lake_elevation_nbr, y=value, colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_grid(var~actual_fish_presence, scales = "free")

env_div%>%
  gather(N0,H,N2,N1,betas.LCBD,E10,key = "var", value = "value")%>% 
  ggplot(aes(x=Lat, y=value, colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_grid(var~actual_fish_presence, scales = "free")

env_div%>%
  #filter(lake_elevation_nbr>3500)%>%
  gather(N0,N1,H,betas.LCBD,E10,key = "var", value = "value") %>% 
  ggplot(aes(x=actual_fish_presence, y=value, fill=actual_fish_presence))+
  geom_boxplot()+
  facet_wrap(~var, scales = "free")

env_div%>%
  filter(lake_perimeter_nbr<5000)%>%
  #filter(lake_elevation_nbr>3500)%>%
  gather(N0,N1,H,betas.LCBD,E10,key = "var", value = "value") %>% 
  ggplot(aes(x=actual_fish_presence, y=value, fill=actual_fish_presence))+
  geom_boxplot()+
  facet_wrap(~var, scales = "free")

env_div%>%
  gather(N0,H,N2,N1,betas.LCBD,E10,key = "var", value = "value") %>% 
  ggplot(aes(x=water_temp, y=value, colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_grid(var~actual_fish_presence, scales = "free")

#Fish presence effect abundance individuals species
pivot_clean_zoopzz<-clean_zoopzz%>%pivot_wider(names_from = "Species_Name",values_from="Abundance")

env_abund<-left_join(clean_zoopzz,env, by=c("lake_id","survey_date"))%>%filter(actual_fish_presence=="Yes" |actual_fish_presence=="No")

env_abund%>%
  filter(lake_drainage_name=="SN_JQN_SF_EVOLUTION_CK")%>%
  ggplot(aes(x=actual_fish_presence,y=log(Abundance+1),fill=actual_fish_presence))+
  geom_boxplot()+
  facet_wrap(~Species_Name, scales="free")
  
env_abund%>%
  ggplot(aes(x=actual_fish_presence,y=(Abundance),fill=actual_fish_presence))+
  geom_bar(stat = "identity")+
  facet_wrap(~Species_Name, scales="free")

env_abund%>%
  ggplot(aes(x=Species_Name,y=(Abundance),fill=actual_fish_presence))+
  geom_bar(stat = "identity")+
  facet_wrap(~actual_fish_presence, scales="free")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##############################################################################################################################
#BODy size effects of fish-CWM
#Biomass instead of density
#dt11/dt12
zoop_body_size<-dt11%>%
  left_join(dt12, by = "SpeciesID")%>%
  mutate(Species_Name = str_replace(Species_Name, " ", "_"))%>%
  mutate(Species_Name = str_replace(Species_Name, "/", "_"))

#average body size per species
av_zoop_body_size<-zoop_body_size%>%
  group_by(Species_Name)%>%
  summarize(mean_body_size=mean(Length))

#CWM
av_zoop_body_size_new<-av_zoop_body_size%>%
  filter(Species_Name !="standard_measurement")%>%
  remove_rownames()%>%
  column_to_rownames('Species_Name')

pivot_clean_zoopzz<-clean_zoopzz%>%pivot_wider(names_from = "Species_Name",values_from="Abundance")%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

if(dim(pivot_clean_zoopzz)[2]!=dim(av_zoop_body_size_new)[1])stop("error:differentnumberofspeciesin'traits'and'abundances'matrices")

tres_bm = dbFD(av_zoop_body_size_new,pivot_clean_zoopzz, corr = ("lingoes"),
               stand.FRic = TRUE, calc.FDiv = TRUE)

cwm=tres_bm$CWM
cwm<-cwm%>%
  rownames_to_column("id_date")%>%
  separate("id_date",sep="_" ,into=c("lake_id", "survey_date"))%>%
  dplyr::rename(CWM=mean_body_size)

#combine CWM with env
cwm$lake_id<-as.integer(cwm$lake_id)
env_cwm<-left_join(env,cwm, by=c("lake_id", "survey_date"))%>%filter(lake_id !="11505" & lake_id !="42219" &lake_id !="71257" &lake_id !="71282" )

env_cwm%>%
  filter(CWM < 4000)%>%
  ggplot(aes(x=actual_fish_presence,y=CWM, fill=actual_fish_presence))+
  geom_boxplot()


env_cwm%>%
  #filter(lake_perimeter_nbr < 2000)%>%
  filter(CWM < 4000)%>%
  ggplot(aes(x=lake_elevation_nbr,y=CWM,colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~actual_fish_presence)

dog<-lm(CWM~lake_elevation_nbr*actual_fish_presence, data=env_cwm)
summary(dog)

env_cwm%>%
  filter(CWM < 4000)%>%
  ggplot(aes(x=lake_elevation_nbr,y=log(CWM+1),colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~actual_fish_presence)
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
#MAP species diversity

mapviewOptions(fgb = FALSE)

new_env_div<-env_div%>%filter(N0>0)
  
snw_sf <- st_as_sf(new_env_div, coords = c("Lon", "Lat"), crs=4326, remove = FALSE)
mapview(snw_sf, zcol="betas.LCBD", layer.name="LCBD")
mapview(snw_sf, zcol="N0", layer.name="Species Richness")
mapview(snw_sf, zcol="N1", layer.name="Species Diversity")

env_sf <- st_as_sf(env, coords = c("Lon", "Lat"), crs=4326, remove = FALSE)

mapview(env_sf, zcol="actual_fish_presence", layer.name="Fish Presence")


############################################################################################################################################
#Convert body length to body mass
#CWM Biomass
length_mas_conver<-read.csv("length_mass_regress_zoop.csv")

biomass<-length_mas_conver%>%
  dplyr::select(-c(Mean_body_size_mm))%>%
  remove_rownames()%>%
  column_to_rownames('Species_Name')


pivot_clean_zoopzz<-clean_zoopzz%>%pivot_wider(names_from = "Species_Name",values_from="Abundance")%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

if(dim(pivot_clean_zoopzz)[2]!=dim(biomass)[1])stop("error:differentnumberofspeciesin'traits'and'abundances'matrices")

tres_bm = dbFD(biomass,pivot_clean_zoopzz, corr = ("lingoes"),
               stand.FRic = TRUE, calc.FDiv = TRUE)

cwm=tres_bm$CWM
cwm<-cwm%>%
  rownames_to_column("id_date")%>%
  separate("id_date",sep="_" ,into=c("lake_id", "survey_date"))%>%
  dplyr::rename(CWM=Body_mass_ug)

#combine CWM with env
cwm$lake_id<-as.integer(cwm$lake_id)
env_cwm<-left_join(env,cwm, by=c("lake_id", "survey_date"))%>%filter(lake_id !="11505" & lake_id !="42219" &lake_id !="71257" &lake_id !="71282" )

env_cwm%>%
  filter(lake_elevation_nbr > 2500)%>%
  ggplot(aes(x=actual_fish_presence,y=CWM, fill=actual_fish_presence))+
  geom_boxplot()

env_cwm%>%
  filter(lake_perimeter_nbr < 2500)%>%
  ggplot(aes(x=lake_elevation_nbr,y=CWM,colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~actual_fish_presence)

env_cwm%>%
  filter(CWM < 4000)%>%
  ggplot(aes(x=lake_elevation_nbr,y=log(CWM+1),colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm")
 # facet_grid(~actual_fish_presence)
############################################################################################################################################
#Map fish body size
fish_env<-left_join(env,dt3, by=c("lake_id","survey_date"))


fish_sf <- st_as_sf(fish_env, coords = c("Lon", "Lat"), crs=4326, remove = FALSE)
mapview(fish_sf, zcol="fish_length", layer.name="Fish.Size")

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

############################################################################################################################################
#Spatial Connectivity metrics




