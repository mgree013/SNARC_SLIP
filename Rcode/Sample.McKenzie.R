##This script is for downloading and data exploition inital of SLIP
#Link to EDI data portal: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=577&revision=2
#Package ID: edi.577.2 Cataloging System:https://pasta.edirepository.org.
#Data set title: The Sierra Lakes Inventory Project: Non-Native fish and community composition of lakes and ponds in the Sierra Nevada, California.

#Matthew Green 
#16 June 2021 


#Load Packages
Packages <- c("tidyverse","betareg" ,"ggplot2", "vegan", "reshape2","reshape", "adespatial", "sf", "mapview", "viridis", "FD","multcomp","semPlot","lavaan", "performance")
lapply(Packages, library, character.only = TRUE)

#Load all data: Run Script "Download_Slip_Data.R"
source("Rcode/Download_Slip_Data.R", echo = TRUE)
########################################################################################################################################################################
#Part 1) Organize Species and Environmental Data Set: There are a lot of separate files that need to be cleaned and then merged together

#########################################################################
#1A) Correct Sub-sample and uneven sampling data issues
zoopzz<-dt10%>%
  left_join(dt12, by = "SpeciesID") # link species names with unique ID's

dt8.1<-dt8%>%
  dplyr::select(c(lake_id,survey_date,lake_max_depth,zoo_tow_number,zoo_tow_type,zoo_tow_depth)) #remove unnecessary info

#Correct sub sample issue to make equal comparison among sites
clean_zoopzz<-zoopzz%>%
  left_join(dt8.1,by=c("lake_id", "survey_date"))%>%
  group_by(lake_id,survey_date)%>%
  mutate(Max.Subsample=max(Subsample))%>%
  ungroup()%>%
  group_by(lake_id,survey_date,Species_Name)%>%
  mutate(Counts=sum(Number)/Max.Subsample,
         sample_volume=(3.14*0.295*zoo_tow_depth)/4,zoop_density=Counts/(sample_volume/zoo_tow_depth)*zoo_tow_number*33.02,
         Species_Name = str_replace(Species_Name, " ", "_"), #replace spaces in names with "_"
         Species_Name = str_replace(Species_Name, "/", "_"))%>% #replace / in names with "_"
  filter(Species_Name != "standard_measurement")%>%
  dplyr::select(c(lake_id,survey_date,Species_Name,zoop_density))%>% 
  distinct(lake_id,survey_date,Species_Name,zoop_density) #remove duplicates in data

#########################################################################
#1B) Pivot Data Set long to wide format for Species abundance Matrix
site.sp.quad <- cast(clean_zoopzz, lake_id+survey_date ~ Species_Name, value='zoop_density') 
site.sp.quad <- as.data.frame(site.sp.quad)  #set data set as data frame
site.sp.quad[is.na(site.sp.quad)] <- 0       #Replace NA's with 0's


#Species Diversity as Response Varible: calculate multiple species diversity metrics
species<-as.data.frame(site.sp.quad[,3:39])   #Select species and abundances only
diversity<-species%>%
  transmute(N0=rowSums(species > 0),
            H= diversity(species),
            N1 =exp(H),
            N2 =diversity(species, "inv"),
            J= H/log(N0),
            E10= (N1/N0),
            E20= (N2/N0),
            Com.Size=rowSums(species), #Total number individuals per site
            betas.LCBD=beta.div(species, method="hellinger",sqrt.D=TRUE)$LCBD) #LCBD (Beta-diversity or variability among sites)

#Add back site info and treatments to diversity data
local_diversity<-cbind(diversity, site.sp.quad[,1:2])

#########################################################################
#Part 1C) Organize Environmental Data Set and merge with Species Data

#Add in GPS coordinates from LAKEID variable: Using sieera_lakes arc GIS export to excel
gps<-read.csv("Data/sierra_lakes.csv")

dt5.1<-dt5%>%
  pivot_wider(names_from = littoral_type, names_glue = "{littoral_type}_{.value}",values_from=littoral_amount)

dt6.1<-dt6%>% 
  pivot_wider(names_from = shoreline_type,names_glue = "{shoreline_type}_{.value}", values_from=shoreline_amount)

#Join multiple envrionmental data sets with species data: Link by unique site id and date sampled
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

sp_abund_env<-left_join(site.sp.quad,env, by=c("lake_id", "survey_date"))%>%filter(lake_id!="70534")
summary(sp_abund_env)

#########################################################################
#1D) Isolated individual Species Abundance
#Fish presence  and environmental effects on abundance individuals species
pivot_clean_zoopzz<-clean_zoopzz%>%pivot_wider(names_from = "Species_Name",values_from="zoop_density")

env_abund<-left_join(clean_zoopzz,env, by=c("lake_id","survey_date"))%>%filter(actual_fish_presence=="Yes" |actual_fish_presence=="No")


###################################################################################################################################################################################################################################################################################################################################
#Part 2) Analysis and Visualization


##########################################################################
#2A)MAP Spatial Visualization of species diversity metrics

mapviewOptions(fgb = FALSE)

new_env_div<-env_div%>%filter(N0>0)

#Diversity Metrics
snw_sf <- st_as_sf(new_env_div, coords = c("Lon", "Lat"), crs=4326, remove = FALSE)
mapview(snw_sf, zcol="betas.LCBD", layer.name="LCBD")
mapview(snw_sf, zcol="N0", layer.name="Species Richness")
mapview(snw_sf, zcol="N1", layer.name="Species Diversity")

#fish Presence
mapview(snw_sf, zcol="actual_fish_presence", layer.name="Fish Presence")
#########################################################################

#2B) Explore Relationships among Diversity as a function of environmental variables: Visualization and Stats

env_div<-left_join(env,local_diversity, by=c("lake_id", "survey_date"))%>%filter(lake_id !="11505" & lake_id !="42219" &lake_id !="71257" &lake_id !="71282" )%>%
  mutate(Com.Size=log(Com.Size+1))%>%
  filter(HA>=0.5)%>%
  filter(lake_elevation_nbr>=2800,lake_elevation_nbr<=3600 )

env_div%>%
  gather(N0,  N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=lake_elevation_nbr, y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  xlab("Elevation (m)")+
  facet_wrap(~var, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank(),legend.position = "none")

env_div%>%
  gather(N0, N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=lake_elevation_nbr, y=value, colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  xlab("Elevation (m)")+
  facet_wrap(~var, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env_div%>%
  gather(N0, N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=lake_elevation_nbr, y=value, colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  xlab("Elevation (m)")+
  facet_grid(var~actual_fish_presence, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

dog<-lm(N0~lake_elevation_nbr, data=env_div)
dog<-glm(N0~lake_elevation_nbr,family = poisson(link = "log"), data=env_div)
summary(dog)
r2(dog)

dog<-lm(N1~lake_elevation_nbr, data=env_div)
summary(dog)
dog<-glm(N1~lake_elevation_nbr, family = gaussian(link="identity"),data=env_div)
summary(dog)

dog<-lm(betas.LCBD~lake_elevation_nbr, data=env_div)
summary(dog)

dog<-lm(Com.Size~lake_elevation_nbr, data=env_div)
summary(dog)

dog<-glm(betas.LCBD~actual_fish_presence, family = gaussian(link="identity"),data=env_div)
dog<-glm(N0~actual_fish_presence, family = poisson(link = "log"),data=env_div)
summary(dog)
r2(dog)

#Analysis:heres a looping linear model
regional.names<-env_div %>%
  dplyr::select(N0, H, N1, N2, E10, E20, J, Com.Size, betas.LCBD)
varlist<-names(regional.names)

models <- lapply(varlist, function(x) {
  lm(substitute(i~lake_elevation_nbr 
                , list(i = as.name(x))), data = env_div)
})
dog<-lapply(models, summary)
dog #this give you output of all models
dog[[1]] #or you can call on individual ones by replacing the number with 1:8

#Significant: betas.LCBD,J,E20,N0, H, N1, N2, E10
#Insignificant: Com.Size

#########################################################################
#Diversity as a function of fish presence/absence
env_div%>%
  gather(N0, N1,  E10, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=actual_fish_presence, y=value, fill=actual_fish_presence))+
  geom_boxplot()+
  xlab("Fish Presence")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  facet_wrap(~var, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                          panel.border = element_blank(),panel.background = element_blank())

env_div_av<-env_div%>%
  filter(!is.na(N0))%>%
  group_by(actual_fish_presence)%>%
  summarise(mean_n1=mean(N1), meanN0=mean(N0), mean.beta=mean(betas.LCBD), mean.Com=mean(Com.Size))

mod<-glm(N0~actual_fish_presence, family=poisson(link="log"),env_div)
summary(mod)

mod<-glm(N1~actual_fish_presence, family=gaussian(link="identity"),env_div)
summary(mod)

mod<-glm(Com.Size~actual_fish_presence, family=gaussian(link="identity"),env_div)
summary(mod)

mod<-betareg(betas.LCBD~actual_fish_presence,env_div)
summary(mod)

env_div%>%
  gather( N1, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=actual_fish_presence, y=value, fill=as.factor(actual_fish_presence)))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  xlab("Fish Presence")+
  facet_wrap(~var, scales = "free")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

#remove unwanted columns for analysis due to missing data
env_divz<-env_div%>%
  dplyr::select(c(N0, H, N1, N2, E10, E20, J, Com.Size, betas.LCBD,actual_fish_presence, lake_drainage_name,Jurisdicti))

env_divz<-as.data.frame(env_divz)

for(i in 1:(ncol(regional.names))){
  columns <- names(env_divz[i])
  anovaresult<- summary(aov(env_divz[,i]~actual_fish_presence,data=env_divz))
  print(columns)
  print(anovaresult)
}
#Beta diversity and Community size are different in fish and fishless lakes. All others are insignificant

#########################################################################
#Diversity as a function of Government Jurisdiction
env_div%>%
  gather(N0, H, N1, N2, E10, E20, J, Com.Size, betas.LCBD,key = "var", value = "value")%>% 
  ggplot(aes(x=Jurisdicti, y=value, fill=Jurisdicti))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(~var, scales = "free")+
  xlab("Government Jurisidction")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

#TUKEY and ANOVA Loop
for(i in 1:(ncol(regional.names))){
  columns <- names(env_divz[i])
  anovaresult<- summary(aov(env_divz[,i]~Jurisdicti,data=env_divz))
  posthocresult <- TukeyHSD(aov(env_divz[,i]~Jurisdicti,data=env_divz))
  
  print(columns)
  print(anovaresult)
  print(posthocresult)
}

#ANOVA: Sig:N0, H, N1, N2, J, Com.Size, betas.LCBD
        #Non-Sig: E10, E20
#TUKEY: N0:Dif: YOSE-SEKI,YOSE-USFS;  Not Dif:USFS-SEKI
#       H: Dif: YOSE-SEKI,YOSE-USFS;  Not Dif: USFS-SEKI
#       N1:Dif: YOSE-SEKI,YOSE-USFS;  Not Dif: USFS-SEKI
#       N2:Dif: YOSE-SEKI,YOSE-USFS;  Not Dif: USFS-SEKI
#       J:Dif: YOSE-SEKI,YOSE-USFS;  Not Dif: USFS-SEKI
#       Com.Size:Dif: USFS-SEKI;Not Dif: YOSE-SEKI,YOSE-USFS
#       betas.LCBD:Dif: YOSE-SEKI,YOSE-USFS;  Not Dif: USFS-SEKI

#Conclusion: Diversity metrics vary among Gov Jurisdictions, except evenness does not  change.

#########################################################################
#2C) Effects on individual species: Analysis and Viz

env_abund%>%
  ggplot(aes(x=actual_fish_presence,y=log(zoop_density+1),fill=actual_fish_presence))+
  geom_boxplot()+
  xlab("Fish Presence")+ylab("Log Density")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  facet_wrap(~Species_Name, scales="free")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                  panel.border = element_blank(),panel.background = element_blank())

env_abund%>%
  ggplot(aes(x=Species_Name,y=(zoop_density),fill=actual_fish_presence))+
  geom_bar(stat = "identity")+
  #facet_wrap(~actual_fish_presence, scales="free")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  ylab("Zooplankton Density")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())

env_abund%>%
  ggplot(aes(x=reorder(Species_Name, zoop_density, FUN = median),y=log(zoop_density+1),fill=actual_fish_presence))+
  geom_boxplot()+
  #facet_wrap(~actual_fish_presence, scales="free")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  ylab("Zooplankton Density")+xlab("Zooplankton Species")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())

env_abund%>%
  ggplot(aes(x=reorder(Species_Name, zoop_density, FUN = median),y=log(zoop_density+1),fill=actual_fish_presence))+
  geom_boxplot()+
  #facet_wrap(~actual_fish_presence, scales="free")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  ylab("Zooplankton Density")+xlab("Zooplankton Species")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())

env_abund%>%
  ggplot(aes(x=lake_elevation_nbr,y=log(zoop_density+1),color=Species_Name))+
  geom_point()+
  geom_smooth(method = "lm")+
  #facet_wrap(~actual_fish_presence, scales="free")+
  scale_color_viridis_d()+
  ylab("Zooplankton Density")+
  facet_wrap(~Species_Name, scales = "free")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank(), legend.position = "none")


env_abund$Species_Name<-as.factor(env_abund$Species_Name)
clean_zoopzz$lake_id<-as.integer(clean_zoopzz$lake_id)
env$lake_id<-as.integer(env$lake_id)

env_abundz<-clean_zoopzz%>%
  pivot_wider(names_from = "Species_Name",values_from="zoop_density")%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  left_join(env, by=c("lake_id", "survey_date"))


env_abundzzz<-env_abund%>%dplyr::rename(Taxon=Species_Name)%>%left_join(av_zoop_body_size_new, by="Taxon")

env_abundzzz%>%
  filter()%>%
  ggplot(aes(x=reorder(Taxon, Mean_body_size_mm, FUN = median),y=log(zoop_density+1),fill=actual_fish_presence))+
  geom_boxplot()+
  #facet_wrap(~actual_fish_presence, scales="free")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  ylab("Zooplankton Density")+xlab("Zooplankton Species")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())


##########
#Just Select Species that occur in fish and fishless sites
env_abundz_filter<-env_abundzzz%>%
  group_by(Taxon,actual_fish_presence,Mean_body_size_mm)%>%
  summarise(Mean_density=mean(zoop_density))%>%
  pivot_wider(names_from=actual_fish_presence,values_from =Mean_density )%>%
  mutate(change_density=No-Yes, change_1_density=Yes-No)

env_abundz_filter%>%
  filter(change_density>0)%>%
  ggplot(aes(x=log(Mean_body_size_mm+1),y=log(change_density+1)))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Cahnge Zooplankton Density")+xlab("Zooplankton Body Size")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())

env_abundz_filter%>%
  filter(change_1_density>0)%>%
  ggplot(aes(x=log(Mean_body_size_mm+1),y=log(change_1_density+1)))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Cahnge Zooplankton Density")+xlab("Zooplankton Body Size")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())


env_abundz_filtered<-env_abundzzz%>%
  filter(Taxon !="Collotheca_spp." ,Taxon !="Eurycercus_lamellatus" ,Taxon !="Lecane_spp." ,Taxon !="Monostyla_spp." ,Taxon !="Polyarthra_vulgaris" ,Taxon !="Simocephalus_serrulatus" )

env_abundz_filtered%>%
 ggplot(aes(x=reorder(Taxon, Mean_body_size_mm, FUN = median),y=log(zoop_density+1),fill=actual_fish_presence))+
  geom_boxplot()+
  #facet_wrap(~actual_fish_presence, scales="free")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  ylab("Zooplankton Density")+xlab("Zooplankton Species")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())


#Analysis: 
#remove unwanted columns for analysis due to missing data
env_abundzz<-env_abundz%>%
  unite("id_date",lake_id:survey_date)%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>%
  dplyr::select(c(Leptodiaptomus_signicauda,nauplii,Daphnia_melanica,Daphnia_dentifera,Conochilus_unicornis,Keratella_spp.,                       
  Keratella_quadrata,Hesperodiaptomus_shoshone,Polyarthra_dolichoptera,Cyclopoida,Lepadella_spp.,Chydorus_sphaericus,
  Holopedium_gibberum,Bosmina_longirostris,Trichocerca_capucina,Harpacticoida,Kellicottia_spp.,Polyarthra_vulgaris,Asplanchna_spp.,
  Trichotria_spp.,Hesperodiaptomus_eiseni,Alona_spp.,Ceriodaphnia_laticaudata,Alonella_excisa,Notholca_spp.,Synchaeta_spp.,Collotheca_spp.,
  Ascomorpha_spp.,Scapholeberis_mucronata,Diaphanosoma_brachyurum, Chaoborus_trivitattus,Polyphemus_pediculus, Daphnia_pulex_pulicaria,
  Eurycercus_lamellatus, actual_fish_presence))

env_abundzz<-as.data.frame(env_abundzz)

regional.names<-env_abundzz %>%
  dplyr::select(Leptodiaptomus_signicauda,nauplii,Daphnia_melanica,Daphnia_dentifera,Conochilus_unicornis,Keratella_spp.,                       
                Keratella_quadrata,Hesperodiaptomus_shoshone,Polyarthra_dolichoptera,Cyclopoida,Lepadella_spp.,Chydorus_sphaericus,
                Holopedium_gibberum,Bosmina_longirostris,Trichocerca_capucina,Harpacticoida,Kellicottia_spp.,Polyarthra_vulgaris,Asplanchna_spp.,
                Trichotria_spp.,Hesperodiaptomus_eiseni,Alona_spp.,Ceriodaphnia_laticaudata,Alonella_excisa,Notholca_spp.,Synchaeta_spp.,Collotheca_spp.,
                Ascomorpha_spp.,Scapholeberis_mucronata,Diaphanosoma_brachyurum, Chaoborus_trivitattus,Polyphemus_pediculus, Daphnia_pulex_pulicaria,
                Eurycercus_lamellatus)

for(i in 1:(ncol(regional.names))){
  columns <- names(env_abundzz[i])
  anovaresult<- summary(aov(env_abundzz[,i]~actual_fish_presence,data=env_abundzz))
  
  print(columns)
  print(anovaresult)
}

#Mostly large bodied taxa are missing from fish present lakes...

#########################################################################
#2D) Body size effects of fish: Community Weighted Mean (CWM)
#dt11/dt12
av_zoop_body_size_new<-dt11%>%
  left_join(dt12, by = "SpeciesID")%>%
  mutate(Species_Name = str_replace(Species_Name, " ", "_"),
         Species_Name = str_replace(Species_Name, "/", "_"))%>%
  group_by(Species_Name)%>%
  summarize(mean_body_size=mean(Length))%>%
  filter(Species_Name !="standard_measurement")%>%
  remove_rownames()%>%
  column_to_rownames('Species_Name')

av_zoop_body_size_new<-read.csv("data/length_mass_regress_zoop.csv",row.names = 1)
av_zoop_body_size_new<-av_zoop_body_size_new%>%dplyr::select(-c(Mean_body_size_mm))

pivot_clean_zoopzz<-clean_zoopzz%>%pivot_wider(names_from = "Species_Name",values_from="zoop_density")%>%
  unite("id_date",lake_id:survey_date)%>%
  dplyr::select(-c(Lecane_spp.,Monostyla_spp.,Simocephalus_serrulatus))%>%
  remove_rownames()%>%
  column_to_rownames('id_date')%>% 
  replace(is.na(.), 0)%>%
  dplyr::select(sort(names(.)))

#Calculate CWM
if(dim(pivot_clean_zoopzz)[2]!=dim(av_zoop_body_size_new)[1])stop("error:differentnumberofspeciesin'traits'and'abundances'matrices")

tres_bm = dbFD(av_zoop_body_size_new,pivot_clean_zoopzz, corr = ("lingoes"),
               stand.FRic = TRUE, calc.FDiv = TRUE)

#Combine CWM with env and organize data
cwm=tres_bm$CWM
cwm<-cwm%>%
  rownames_to_column("id_date")%>%
  separate("id_date",sep="_" ,into=c("lake_id", "survey_date"))%>%
  dplyr::rename(CWM=Body_mass_ug) #Body_mass_ug or mean_body_size

cwm$lake_id<-as.integer(cwm$lake_id)
env_cwm<-left_join(env,cwm, by=c("lake_id", "survey_date"))%>%filter(lake_id !="11505" & lake_id !="42219" &lake_id !="71257" &lake_id !="71282" )%>%filter(CWM < 4000)


#########################################################################
#Visualize influence of fish and elevation

env_cwm%>%
  ggplot(aes(x=lake_drainage_name,y=CWM, fill=actual_fish_presence))+
  geom_boxplot()+
  xlab("Fish Presence")+
  scale_fill_viridis(discrete = TRUE,name = "Lake Network")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env_cwm%>%
  ggplot(aes(x=actual_fish_presence,y=CWM, fill=actual_fish_presence))+
  geom_boxplot()+
  xlab("Fish Presence")+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())


dog<-glm(CWM~actual_fish_presence*lake_elevation_nbr,family=gaussian(link="identity"),env_cwm)
summary(dog)
r2(dog)

env_cwm%>%
  ggplot(aes(x=lake_elevation_nbr,y=CWM,colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d(name = "Fish Presence")+
  facet_grid(~actual_fish_presence)+
  xlab("Elevation (m)")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

env_cwm%>%
  ggplot(aes(x=log(lake_area_nbr+1),y=CWM,colour=actual_fish_presence))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d(name = "Fish Presence")+
  facet_grid(~actual_fish_presence)+
  xlab("Elevation (m)")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

fishless<-env_cwm%>%
  filter(actual_fish_presence=="No")

fish<-env_cwm%>%
  filter(actual_fish_presence=="Yes")

mod1<-lm(CWM~lake_elevation_nbr,  data=fishless)
summary(mod1)

mod1<-lm(CWM~lake_elevation_nbr,  data=fish)
summary(mod1)

mod0<-glm(N0~actual_fish_presence*lake_elevation_nbr*lake_max_depth,family=poisson(link ="log" ),env_div)
summary(mod0)
r2(mod0)

#Analaysis: GLM's
mod1<-glm(CWM~lake_elevation_nbr*actual_fish_presence,family=gaussian(link = "identity"),  data=env_cwm)
mod2<-glm(CWM~lake_elevation_nbr,family=gaussian(link = "identity"),  data=env_cwm)
mod3<-glm(CWM~actual_fish_presence,family=gaussian(link = "identity"),  data=env_cwm)
null<-glm(CWM~1,family=gaussian(link = "identity"),  data=env_cwm)

reported.table2 <- bbmle::AICtab(mod1,mod2,mod3, null,weights = TRUE, sort = FALSE)
reported.table2
r2(mod3)

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR1

#Conclusion: Lake Elevation and fish presence interactively explain variation in zooplankton community body size

#########################################################################
#2E) Multivariate Ordinations (NMDS)

set.seed(29)
species<-sp_abund_env[,3:39]
dune.rel<-decostand(species,"total") #standardize community data
dune.bray<-vegdist(dune.rel) #calculate dissimilarity among sites (i.e. dissimilarity matrix)
dune.nmds=metaMDS(dune.rel, k=2, try=10) #NMDS code
dune.nmds
stressplot(dune.nmds) #this tells us if our plot is going to work, and it looks good

plot(dune.nmds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
#text(dune.nmds$species[,1], dune.nmds$species[,2], rownames(dune.nmds$species), cex=0.7, col ="black")
points(dune.nmds$points[,1], dune.nmds$points[,2],  pch = 1) 
ordihull(dune.nmds, groups=sp_abund_env$actual_fish_presence, draw="polygon", label=T)
#ordihull(dune.nmds, groups=sp_abund_env$lake_drainage_name, draw="polygon", label=T)
ordisurf(dune.nmds, sp_abund_env$lake_elevation_nbr, prioirty=,labcex=0.9, add = T,col="forestgreen")

#PERMANOVA analysis-Whats driving variation we see above?
adonis2(dune.bray ~ sp_abund_env$actual_fish_presence+sp_abund_env$lake_elevation_nbr+sp_abund_env$lake_drainage_name, permutations = 99, method = "bray")
betad <- betadiver(dune.rel, "z")
adonis(betad ~ sp_abund_env$actual_fish_presence+sp_abund_env$lake_elevation_nbr, data=species, perm=200)

adonis2(dune.bray ~ sp_abund_env$actual_fish_presence, permutations = 99, method = "bray")
betad <- betadiver(dune.rel, "z")
adonis(betad ~ sp_abund_env$actual_fish_presence, data=species, perm=200)

dune.envfit <- envfit(dune.nmds, env = sp_abund_env$lake_elevation_nbr, perm = 999) #standard envfit
dune.envfit
env.scores.dune <- as.data.frame(scores(dune.envfit, display = "vectors")) #extracts relevant scores from envifit
env.data = cbind(sp_abund_env$lake_elevation_nbr)
mds.data.envfit = envfit(dune.nmds, env.data)

plot(mds.data.envfit, col = "black", labels = c( "Elevation"), lwd = 2)

mod <- betadisper(dune.bray, sp_abund_env$actual_fish_presence)
anova(mod)
print(mod)
permutest(mod)
boxplot(mod)
#adonis2(species ~ sp_abund_env$lake_elevation_nbr+sp_abund_env$actual_fish_presence+sp_abund_env$lake_drainage_name, permutations = 999, method = "bray")

#The Drainage Basins explain most of the variation (19%), then elevation (7%), and then fish presence (1%)...still a lot fo unexplained variation
#fish presence has larger hull indicating more community combinations, potentially confounded by elevation

#########################################################################
#2F) Structural Equation Models (SEM'S)-Causal Analysis


smod1 = ' CWM ~ lake_elevation_nbr+actual_fish_presence+lake_max_depth+lake_area_nbr'

smod1.fit <- sem(smod1,data=env_cwm)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

#Conc: Body Size negatively influenced by  elevation,area, fish, but positively influenced by lake depth
#fish presnece also negatively influenced by elevation, indirectly
#########################################################################
smod2 = ' N0 ~ lake_elevation_nbr+actual_fish_presence+lake_max_depth'

smod2.fit <- sem(smod2,data=env_div)
summary(smod2.fit,standardized=TRUE,rsq=T)
fitMeasures(smod2.fit)
modindices(smod2.fit)

#quick plot of path analysis
semPaths(smod2.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

#Conc: Species richness negatively influenced by  elevation, but positively influenced by lake depth

#########################################################################
smod3 = ' lake_elevation_nbr~water_temp +air_temp
          N0~water_temp +air_temp'

smod3.fit <- sem(smod3,data=env_div)
summary(smod3.fit,standardized=TRUE,rsq=T)
fitMeasures(smod3.fit)
modindices(smod3.fit)

#quick plot of path analysis
semPaths(smod3.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

#Conc:Elevation effects water and air temp, which have  direct negative effects on species richness

####################################################################################################################################################################################################################################################################################################
#Fish by Elevation
env_div%>%
  ggplot(aes(x = actual_fish_presence, y = lake_elevation_nbr, fill=actual_fish_presence))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE,name = "Fish Presence", labels = c("No", "Yes"))+
  xlab("Fish Presence")+
  labs(fill='Elevation (m)') +
  #scale_fill_discrete(name = "Fish Presence", labels = c("no", "yes"))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())
####################################################################################################################################################################################################################################################################################################

