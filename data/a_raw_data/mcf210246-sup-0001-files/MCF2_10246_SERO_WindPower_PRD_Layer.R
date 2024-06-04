###########################################
#
# SERO-PRD WindPower Analysis
# Reads in and assigns scores to
# SERO-PRD protected species data
# layers for Gulf of Mexico BOEM WEA
#
# Evaluates overlap of SERO-PRD
# protected species data layers
# with Product, Arithmetic Mean,
# Geometric Mean, and Lowest Score
# approaches.
#
# POC: Nick Farmer, Ph.D.
# Email: nick.farmer@noaa.gov
#
# Last Update: 1/25/2022
#
###########################################

#### 0.0 Import Libraries ####

library(sp)
library(rgdal)
library(adehabitatLT)
library(adehabitatMA)
library(adehabitatHR)
library(MASS)
library(raster)
library(ggplot2)
library(raster)
library(sf)
library(sp)
library(gridExtra)
library(marmap)
library(lubridate)
library(ggpubr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
library(maps)
library(ggsn)
library(ggspatial)
library(psych)
library(reshape2)

#### 1.0 Define Scores for each species ####
SAWFISH=0.3 #E, increasing
RICES=0.1 #E, declining/small #s
MANTA=0.4 #T, declining
GREEN=0.5 #T, increasing
KEMPS=0.2 #E, unknown
LOGGERHEAD=0.4 #T, unknown/possibly stable
LEATHERBACK=0.1 #E, declining
HAWKSBILL=0.2 #E, unknown
SPERM<-0.2 #E, unknown
STURGEON<-0.5 #T, increasing
OWT<-0.4 #T, unknown/declining
BEAKED<-0.7 #MMPA unknown but not strategic
PILOT<-0.7 #MMPA unknown but not strategic
BLACKFISH<-0.7 #MMPA unknown but not strategic (false killer whales, pygmy killer whales, and melon-headed whales)
CLYMENE<-0.6 #MMPA Strategic
KOGIA<-0.7 #MMPA unknown but not strategic
O_BND<-0.7 #MMPA unknown but not strategic
RISSOS<-0.7 #MMPA unknown but not strategic
PANTROPICAL<-0.7 #MMPA unknown but not strategic ("satt" Stenella attenuatta attenuatta)
O_ATLSD<-0.7 #MMPA unknown but not strategic ("sf" Stenella frontalis)
S_BND<-0.8 #MMPA large
S_ATLSD<-0.7 #MMPA large unknown
SPINNER<-0.6 #MMPA strategic
STRIPED<-0.6 #MMPA strategic
DEFAULT_ESA<-0.5
DEFAULT_MMPA<-0.9

#default projection: <your_file>_utm<-spTransform(<your_file>,CRS("+init=epsg:26917"))

#### FUNCTIONS ####
plot_N<-function(d,species) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=max_N),color=NA)+scale_fill_distiller(palette ="Spectral", name="N")+
    geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
    labs(title=species,x="",y="",tag="A")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
    ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                      height=unit(0.5, "cm"),
                                      width = unit(0.5, "cm")) +
    ggspatial::annotation_scale(location = 'bl')+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(legend.position = c(0.9,0.85),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=6)) #change legend text font size)
}

plot_N_nolabels<-function(d,species) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=max_N),color=NA)+scale_fill_distiller(palette ="Spectral", name="N",na.value = NA)+
    geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
    labs(title=species,x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
    #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
    ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(title = element_text(size=4),
          text = element_text(size=4),
          legend.position = c(.5,-0.5),
          legend.direction = "horizontal",
          plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=4), #change legend title font size
          legend.text = element_text(size=4)) #change legend text font size
}

plot_P<-function(d,species) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=prs_mx_),color=NA)+scale_fill_distiller(palette ="Spectral", name="P")+
    geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
    labs(title=species,x="",y="",tag="A")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
    ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                      height=unit(0.5, "cm"),
                                      width = unit(0.5, "cm")) +
    ggspatial::annotation_scale(location = 'bl')+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(legend.position = c(0.9,0.85),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=6)) #change legend text font size)
}

plot_P_nolabels<-function(d,species) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=prs_mx_),color=NA)+scale_fill_distiller(palette ="Spectral", name="P",na.value=NA)+
    geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
    labs(title=species,x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
    #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
    ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(title = element_text(size=4),
          text = element_text(size=4),
          legend.position = c(0.5,-0.5),
          legend.direction = "horizontal",
          plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=4), #change legend title font size
          legend.text = element_text(size=4)) #change legend text font size
}

plot_Score<-function(d,species,scorename) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=as.factor(d[,scorename][[1]])),color=NA)+
    geom_sf(worldmap[which(worldmap$region_un=="Americas"),],mapping=aes(),fill="light gray",col="gray")+
    geom_sf(us_geo[which(us_geo$GEOID<=48),],mapping=aes(),fill="light gray",col="gray")+
    labs(fill="Score",x="",y="",tag="B")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
    ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                      height=unit(0.5, "cm"),
                                      width = unit(0.5, "cm")) +
    ggspatial::annotation_scale(location = 'bl')+
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(legend.position = c(0.9,0.85),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=6)) #change legend text font size
}

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(9,"Spectral")
#myColors[10]<-"#00000000"
names(myColors) <- c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9")
colScale <- scale_colour_manual(name = "grp",values = myColors)

plot_Score_final<-function(d,species,scorename,tagLetter) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=as.factor(d[,scorename][[1]])),color=NA)+
    scale_fill_manual(breaks=names(myColors), values=myColors,na.value=NA)+
    geom_sf(worldmap[which(worldmap$region_un=="Americas"),],mapping=aes(),fill="light gray",col="gray")+
    geom_sf(us_geo[which(us_geo$GEOID<=48),],mapping=aes(),fill="light gray",col="gray")+
    labs(title=species,fill="Score",x="",y="",tag=tagLetter)+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
    #coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")+
    ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                      height=unit(0.25, "cm"),
                                      width = unit(0.25, "cm")) +
    ggspatial::annotation_scale(location = 'bl',height=unit(0.10,'cm'))+
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(title = element_text(size=6),
          text = element_text(size=6),
          legend.position = c(0.92,0.8),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=6)) #change legend text font size
}

plot_Score_final2<-function(d,species,scorename,tagLetter) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=as.factor(d[,scorename][[1]])),color=NA)+
    scale_fill_manual(breaks=names(myColors), values=myColors,na.value=NA)+
    geom_sf(worldmap[which(worldmap$region_un=="Americas"),],mapping=aes(),fill="light gray",col="gray")+
    geom_sf(us_geo[which(us_geo$GEOID<=48),],mapping=aes(),fill="light gray",col="gray")+
    labs(title=paste0(tagLetter,") ",species),fill="Score",x="",y="")+
    geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
    #coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")+
    #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.25, "cm"),width = unit(0.25, "cm")) +
    ggspatial::annotation_scale(location = 'bl',height=unit(0.075,'cm'),text_cex = 0.25)+
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(title = element_text(size=4),
          text = element_text(size=4),
          plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
          legend.position = "none",#c(0.92,0.8),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=4), #change legend title font size
          legend.text = element_text(size=4)) #change legend text font size
}

plot_Score_final_bigtext<-function(d,species,scorename,tagLetter) {
  ggplot()+ geom_sf(data=d,mapping=aes(fill=as.factor(d[,scorename][[1]])),color=NA)+
    scale_fill_manual(breaks=names(myColors), values=myColors)+
    geom_sf(worldmap[which(worldmap$region_un=="Americas"),],mapping=aes(),fill="light gray",col=NA)+
    geom_sf(us_geo[which(us_geo$GEOID<=48),],mapping=aes(),fill="light gray",col="gray")+
    labs(title=species,fill="Score",x="",y="",tag=tagLetter)+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
    coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+
    #  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")+
    ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                      height=unit(0.5, "cm"),
                                      width = unit(0.5, "cm")) +
    ggspatial::annotation_scale(location = 'bl')+
    guides(shape = guide_legend(override.aes = list(size = 1)))+
    theme(legend.position = c(0.8,1.15),
          legend.direction = "horizontal",
          legend.key.size = unit(0.4, 'cm'), #change legend key size
          legend.key.height = unit(0.4, 'cm'), #change legend key height
          legend.key.width = unit(0.4, 'cm'), #change legend key width
          legend.title = element_text(size=5), #change legend title font size
          legend.text = element_text(size=5)) #change legend text font size
}

#### 2.0 Input layers for All Maps ####
setwd("D:/Documents/GIS/Data/Protected Species/Wind Power")
# PAOA_E_GRID<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/GOM_PAOA_NAD83/GOM_PAOA_NAD83",layer="PAOA_E_GRID")
# PAOA_C_GRID<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/GOM_PAOA_NAD83/GOM_PAOA_NAD83",layer="PAOA_C_GRID")
# PAOA_W_GRID<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/GOM_PAOA_NAD83/GOM_PAOA_NAD83",layer="PAOA_W_GRID")
# PAOA_SE_GRID<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/GOM_PAOA_NAD83/GOM_PAOA_NAD83",layer="PAOA_SE_GRID")

WEA_CALL<-readOGR(dsn="./GOM ReN CALL RFI.gdb",layer="G0M_ReN_WEA_CallArea")
WEA_CALL<-spTransform(WEA_CALL,CRS("+init=epsg:26917"))
WEA_CALL<-st_as_sf(WEA_CALL)
US_MAR_LIMS<-readOGR(dsn="D:/Documents/GIS/Base/USA/USMaritimeLimitsAndBoundariesSHP",layer="USMaritimeLimitsNBoundaries")
us_geo <- tigris::states(class = "sf", cb = TRUE) %>% 
  shift_geometry()
worldmap <- ne_countries(returnclass = 'sf')


#### 2.1 Manta Ray ####
# use species distribution model (Farmer et al. (2022) Scientific Reports)
# manta_SDM_consult<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Manta Ray/ENSEMBLE/Pres",layer="GMR_consult")
# nyserda_grid<-read.csv("D:/Documents/GIS/Data/Protected Species/Manta Ray/ENSEMBLE/Pres/nyserda_grid_20201216.csv",stringsAsFactors = F)
# sefsc_grid<-read.csv("D:/Documents/GIS/Data/Protected Species/Manta Ray/SEFSC Aerial Surveys/sefsc_grid_FINAL.csv",stringsAsFactors = F)
# narwc_grid<-read.csv("D:/Documents/GIS/Data/Protected Species/Manta Ray/NARWC/narwc_grid_clean_skymaster.csv",stringsAsFactors = F)
# narwc_grid$Source<-"NARWC"
# sefsc_grid$Source<-"SEFSC"
# nyserda_grid$Source<-"NYSERDA"
# combo_grid2<-rbind(sefsc_grid[,c("Source","Latitude","Longitude","striparea","N","Depth_m","SST","Front_Z","pp","ChlA","DfromShore","Slope_deg")],
#                    narwc_grid[,c("Source","Latitude","Longitude","striparea","N","Depth_m","SST","Front_Z","pp","ChlA","DfromShore","Slope_deg")],
#                    nyserda_grid[,c("Source","Latitude","Longitude","striparea","N","Depth_m","SST","Front_Z","pp","ChlA","DfromShore","Slope_deg")])
# combo_grid2<-combo_grid2[which(combo_grid2$Source=="NYSERDA" | (combo_grid2$Source=="NARWC" & combo_grid2$Latitude < 35) | (combo_grid2$Source=="SEFSC" & combo_grid2$Latitude < 35)),]
# combo_grid2$pres<-ifelse(combo_grid2$N>0,1,0)
# combo_grid2<-combo_grid2[which(combo_grid2$striparea>0),]
# min_depth<-min(combo_grid2$Depth_m)
# manta_SDM_consult$pres_mx_depth<-ifelse(manta_SDM_consult$Depth_m>min_depth,manta_SDM_consult$pres_mx,NA) #maximum probability of detection by cell from 1-2003 to 12-2019; don't predict to depths beyond model inputs
# manta_sf<-st_as_sf(manta_SDM_consult)
# manta_sf$pres_mx_depth<-ifelse(manta_sf$pres_mx_depth==-Inf,NA,manta_sf$pres_mx_depth)
# 
# quantile(manta_sf$pres_mx_depth,probs=seq(0,1,0.05),na.rm=T)
# median(manta_sf$pres_mx_depth,na.rm=T)
# manta_sf$consult_1pct<-ifelse(manta_sf$pres_mx_depth>0.01,1,0)
# manta_sf$consult_5pct<-ifelse(manta_sf$pres_mx_depth>0.05,1,0)
# manta_sf$consult_10pct<-ifelse(manta_sf$pres_mx_depth>0.1,1,0)
# manta_sf$consult_15pct<-ifelse(manta_sf$pres_mx_depth>0.15,1,0)
# manta_sf$consult_20pct<-ifelse(manta_sf$pres_mx_depth>0.20,1,0)
# manta_sf$consult_25pct<-ifelse(manta_sf$pres_mx_depth>0.25,1,0)
# manta_sf$consult_30pct<-ifelse(manta_sf$pres_mx_depth>0.30,1,0)
# manta_sf$consult_35pct<-ifelse(manta_sf$pres_mx_depth>0.35,1,0)
# manta_sf$consult_45pct<-ifelse(manta_sf$pres_mx_depth>0.45,1,0)
# manta_sf$consult_50pct<-ifelse(manta_sf$pres_mx_depth>0.50,1,0)
# manta_sf$consult_Median<-ifelse(manta_sf$pres_mx_depth>median(manta_sf$pres_mx_depth,na.rm=T),1,0)
# 
# fgbnms<-readOGR(dsn="D:/Documents/GIS/Base/Gulf/Management Areas/FGBNMS Boundary",layer="fgbnms_pl_ed")
# fgbnms_sf<-st_as_sf(fgbnms)
# 
# windows()
# p1<-ggplot(manta_sf) +
#   geom_sf(aes(fill=pres_mx_depth),colour = NA) + 
#   scale_fill_distiller(palette ="Spectral", name="P(detect)",limits=c(0,1))+
#   geom_sf(fgbnms_sf,mapping=aes())+
#   coord_sf(xlim = c(min(manta_sf$x), max(manta_sf$x)), ylim = c(min(manta_sf$y), max(manta_sf$y)), expand = FALSE)
# p2<-ggplot(manta_sf) +
#   geom_sf(aes(fill=consult_Median),colour = NA) + 
#   scale_fill_distiller(palette ="Spectral", name="P(detect)",limits=c(0,1))+
#   geom_sf(fgbnms_sf,mapping=aes(),colour="white")+
#   coord_sf(xlim = c(min(manta_sf$x), max(manta_sf$x)), ylim = c(min(manta_sf$y), max(manta_sf$y)), expand = FALSE)
# grid.arrange(p1,p2)
# 
# manta_SDM_consult$pres_mx_depth<-ifelse(manta_SDM_consult$pres_mx_depth==-Inf,NA,manta_SDM_consult$pres_mx_depth)
# manta_SDM_consult$consult_Median<-ifelse(manta_SDM_consult$pres_mx_depth>median(manta_SDM_consult$pres_mx_depth,na.rm=T),1,0)
# 
# #assign score
# manta_SDM_consult$AOA_Score<-ifelse(manta_SDM_consult$consult_Median==1,MANTA,ifelse(is.na(manta_SDM_consult$consult_Median),NA,0.5)) #0.4 for threatened/declining; 0.5 for general range
# manta_SDM_consult$MANTA_RAY<-manta_SDM_consult$AOA_Score
# summary(manta_SDM_consult$MANTA_RAY)
# manta_utm<-spTransform(manta_SDM_consult,CRS("+init=epsg:26917"))
# writeOGR(manta_utm,dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="manta_utm",driver="ESRI Shapefile")

manta_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="manta_utm")
manta_utm<-st_as_sf(spTransform(manta_utm,CRS("+init=epsg:26917")))
p1<-plot_P(manta_utm,"Giant Manta Ray")
p2<-plot_Score(manta_utm,"Giant Manta Ray","MANTA_R")
png("./GiantMantaRay.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

# 
# manta_SDM_consult_NAD<-spTransform(manta_SDM_consult,crs(PAOA_C_GRID))
# summary(PAOA_C_GRID@data)
# PAOA_C_GRID_manta<-over(PAOA_C_GRID,manta_SDM_consult_NAD[,"MANTA_RAY"]) #memory error
# manta_sf<-st_as_sf(manta_SDM_consult_NAD)
# 
# #join manta scores to aquaculture grid
# PAOA_C_GRID_manta_sf<-st_join(PAOA_C_GRID_sf,manta_sf[,"MANTA_RAY"])
# PAOA_E_GRID_manta_sf<-st_join(PAOA_E_GRID_sf,manta_sf[,"MANTA_RAY"])
# PAOA_W_GRID_manta_sf<-st_join(PAOA_W_GRID_sf,manta_sf[,"MANTA_RAY"])
# PAOA_SE_GRID_manta_sf<-st_join(PAOA_SE_GRID_sf,manta_sf[,"MANTA_RAY"])
# 
# #clean up duplicates (multiple intersections) by selecting most conservative score
# test<-PAOA_E_GRID_manta_sf
# test$dup<-ifelse(duplicated(test$GRID_ID) | duplicated(test$GRID_ID, fromLast=TRUE),1,0)
# min_dups<-test[order(test$GRID_ID,test$MANTA_RAY,decreasing=F),]
# PAOA_E_GRID_manta_sf<-min_dups[!duplicated(min_dups$GRID_ID),]
# 
# test<-PAOA_W_GRID_manta_sf
# test$dup<-ifelse(duplicated(test$GRID_ID) | duplicated(test$GRID_ID, fromLast=TRUE),1,0)
# min_dups<-test[order(test$GRID_ID,test$MANTA_RAY,decreasing=F),]
# PAOA_W_GRID_manta_sf<-min_dups[!duplicated(min_dups$GRID_ID),]
# 
# test<-PAOA_SE_GRID_manta_sf
# test$dup<-ifelse(duplicated(test$GRID_ID) | duplicated(test$GRID_ID, fromLast=TRUE),1,0)
# min_dups<-test[order(test$GRID_ID,test$MANTA_RAY,decreasing=F),]
# PAOA_SE_GRID_manta_sf<-min_dups[!duplicated(min_dups$GRID_ID),]
# 
# test<-PAOA_C_GRID_manta_sf
# test$dup<-ifelse(duplicated(test$GRID_ID) | duplicated(test$GRID_ID, fromLast=TRUE),1,0)
# min_dups<-test[order(test$GRID_ID,test$MANTA_RAY,decreasing=F),]
# PAOA_C_GRID_manta_sf<-min_dups[!duplicated(min_dups$GRID_ID),]
# 
# #check<-min_dups[min_dups$dup==1,]; View(check)
# 
# 
# #verify scoring worked by plotting
# windows()
# ggplot()+
#   #coord_sf(xlim = c(min(manta_sf$x), max(manta_sf$x)), ylim = c(min(manta_sf$y), max(manta_sf$y)), expand = FALSE)+
#   geom_sf(data=PAOA_E_GRID_manta_sf,aes(fill=factor(MANTA_RAY)),lwd=0,colour=NA)+
#   geom_sf(data=PAOA_C_GRID_manta_sf,aes(fill=factor(MANTA_RAY)),lwd=0,colour=NA)+
#   geom_sf(data=PAOA_W_GRID_manta_sf,aes(fill=factor(MANTA_RAY)),lwd=0,colour=NA)+
#   geom_sf(data=PAOA_SE_GRID_manta_sf,aes(fill=factor(MANTA_RAY)),lwd=0,colour=NA)+
#   geom_sf(data=manta_sf,aes(fill=factor(consult_Median)),lwd=0,colour=NA,alpha=0.2)+  
#   coord_sf(xlim = c(-97, -81), ylim = c(24, 32), expand = FALSE)

#### 2.2 Rice's Whale ####
rices_CORE<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/BE_CORE_AREA_v2_06Jun19",layer="BE_CORE_AREA_v2_06Jun19")
rices_SUITABLE<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/Suitable_Brydes_hab_v1",layer="BRY_Gulf_of_Mexico_Suitable_Habitat_polygon_template_20210129")
rices_CORE_utm<-spTransform(rices_CORE,CRS("+init=epsg:26917"))
rices_SUITABLE_utm<-spTransform(rices_SUITABLE,CRS("+init=epsg:26917"))

#these layers overlap but will receive the same score (0.1), so must eliminate overlap by doing Union
rices_CORE_utm_sf<-st_as_sf(rices_CORE_utm)
rices_SUITABLE_utm_sf<-st_as_sf(rices_SUITABLE_utm)

rices_utm<-st_union(rices_CORE_utm_sf,rices_SUITABLE_utm_sf)

#assign score
rices_utm$RICES<-RICES #score based on table
writeOGR(as(rices_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="rices_utm",driver="ESRI Shapefile")

#rices_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="rices_utm")
#rices_utm<-st_as_sf(rices_utm)

p1<-ggplot()+ geom_sf(data=rices_CORE_utm_sf,mapping=aes(),fill="red",alpha=0.5,color="red")+
  geom_sf(data=rices_SUITABLE_utm_sf,mapping=aes(),fill="blue",alpha=0.5,color="blue")+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Rice's Whale",x="",y="",tag="A")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = 'bl')+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  #coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")+
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(legend.position = c(0.9,0.15),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)
p2<-plot_Score(rices_utm,"Rice's Whale","RICES")
png("./RicesWhale.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.3.1 Green Sea Turtle ####
shelf_green_turtle_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="shelf_greens_max_n")
shelf_green_turtle_utm<-st_as_sf(shelf_green_turtle_utm)
shelf_green_turtle_utm$aboveMedian<-ifelse(shelf_green_turtle_utm$max_N>median(shelf_green_turtle_utm$max_N,na.rm=T),1,0)
shelf_green_turtle_utm$GREEN<-ifelse(shelf_green_turtle_utm$aboveMedian==1,GREEN,ifelse(is.na(shelf_green_turtle_utm$aboveMedian),NA,DEFAULT_ESA))
p1<-plot_N(shelf_green_turtle_utm,"Green Sea Turtle (Shelf)")
p2<-plot_Score(shelf_green_turtle_utm,"Green Sea Turtle (Shelf)","GREEN")
png("./GreenSeaTurtle.png",height=9,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1)
dev.off()

#juveniles
juv_green_turtle_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/Richards Turtle Grids",layer="CM_Grid_1deg_GoMex_utm17")
juv_green_turtle_utm<-st_as_sf(spTransform(juv_green_turtle_utm,CRS("+init=epsg:26917")))
juv_green_turtle_utm$aboveMedian<-ifelse(juv_green_turtle_utm$CM817max>median(juv_green_turtle_utm$CM817max,na.rm=T),1,0) #if grid above median
juv_green_turtle_utm$GREEN<-ifelse(juv_green_turtle_utm$aboveMedian==1,GREEN,ifelse(is.na(juv_green_turtle_utm$aboveMedian),NA,DEFAULT_ESA))
juv_green_turtle_utm$max_N<-juv_green_turtle_utm$CM817max
p3<-plot_N(juv_green_turtle_utm,"Green Sea Turtle (Juvenile)")
p4<-plot_Score(juv_green_turtle_utm,"Green Sea Turtle (Juvenile)","GREEN")
png("./JuvGreenSeaTurtle.png",height=9,width=6.5,res=300,units="in")
egg::ggarrange(p3,p4,ncol=1)
dev.off()

png("./GreenSeaTurtle_all.png",height=6.5,width=9,res=300,units="in")
egg::ggarrange(p1,p3,p2,p4,ncol=2,nrow=2,labels=c("A","B","C","D"),label.args=list(gp=gpar(font=4)))
dev.off()

#spatial join these two layers in ArcGIS>Analysis>Overlay>SpatialJoin>MergeRule=Minimum



#### 2.3.2 Kemps Ridley Sea Turtle ####
shelf_kemps_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="shelf_kemps_max_n")
shelf_kemps_utm<-st_as_sf(shelf_kemps_utm)
shelf_kemps_utm$aboveMedian<-ifelse(shelf_kemps_utm$max_N>median(shelf_kemps_utm$max_N,na.rm=T),1,0)
shelf_kemps_utm$KEMPS<-ifelse(shelf_kemps_utm$aboveMedian==1,KEMPS,ifelse(is.na(shelf_kemps_utm$aboveMedian),NA,DEFAULT_ESA))
p1<-plot_N(shelf_kemps_utm,"Kemp's Ridley Sea Turtle (Shelf)")
p2<-plot_Score(shelf_kemps_utm,"Kemp's Ridley Sea Turtle (Shelf)","KEMPS")
png("./KempsRidleySeaTurtle.png",height=9,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1)
dev.off()

#juveniles
juv_kemps_turtle_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/Richards Turtle Grids",layer="LK_Grid_1deg_GoMex_utm17")
juv_kemps_turtle_utm<-st_as_sf(spTransform(juv_kemps_turtle_utm,CRS("+init=epsg:26917")))
juv_kemps_turtle_utm$aboveMedian<-ifelse(juv_kemps_turtle_utm$LK817max>median(juv_kemps_turtle_utm$LK817max,na.rm=T),1,0) #if grid above median
juv_kemps_turtle_utm$KEMPS<-ifelse(juv_kemps_turtle_utm$aboveMedian==1,KEMPS,ifelse(is.na(juv_kemps_turtle_utm$aboveMedian),NA,DEFAULT_ESA))
juv_kemps_turtle_utm$max_N<-juv_kemps_turtle_utm$LK817max
p3<-plot_N(juv_kemps_turtle_utm,"Kemp's Ridley Sea Turtle (Juvenile)")
p4<-plot_Score(juv_kemps_turtle_utm,"Kemp's Ridley Sea Turtle (Juvenile)","KEMPS")
png("./JuvKempsRidleySeaTurtle.png",height=9,width=6.5,res=300,units="in")
egg::ggarrange(p3,p4,ncol=1)
dev.off()

png("./KempsRidleySeaTurtle_all.png",height=6.5,width=9,res=300,units="in")
egg::ggarrange(p1,p3,p2,p4,ncol=2,nrow=2,labels=c("A","B","C","D"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.3.3 Leatherback Sea Turtle ####
shelf_leatherback_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="shelf_leatherback_max_n")
shelf_leatherback_utm<-st_as_sf(shelf_leatherback_utm)
shelf_leatherback_utm$aboveMedian<-ifelse(shelf_leatherback_utm$max_N>median(shelf_leatherback_utm$max_N,na.rm=T),1,0)
shelf_leatherback_utm$LEATHERBACK<-ifelse(shelf_leatherback_utm$aboveMedian==1,LEATHERBACK,ifelse(is.na(shelf_leatherback_utm$aboveMedian),NA,DEFAULT_ESA))
p1<-plot_N(shelf_leatherback_utm,"Leatherback Sea Turtle (Shelf)")
p2<-plot_Score(shelf_leatherback_utm,"Leatherback Sea Turtle (Shelf)","LEATHERBACK")
png("./LeatherbackSeaTurtle.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.3.4 Loggerhead Sea Turtle ####
shelf_loggerhead_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="shelf_loggerhead_max_n")
shelf_loggerhead_utm<-st_as_sf(shelf_loggerhead_utm)
shelf_loggerhead_utm$aboveMedian<-ifelse(shelf_loggerhead_utm$max_N>median(shelf_loggerhead_utm$max_N,na.rm=T),1,0)
shelf_loggerhead_utm$LOGGERHEAD<-ifelse(shelf_loggerhead_utm$aboveMedian==1,LOGGERHEAD,ifelse(is.na(shelf_loggerhead_utm$aboveMedian),NA,DEFAULT_ESA))
p1<-plot_N(shelf_loggerhead_utm,"Loggerhead Sea Turtle (Shelf)")
p2<-plot_Score(shelf_loggerhead_utm,"Loggerhead Sea Turtle (Shelf)","LOGGERHEAD")
png("./LoggerheadSeaTurtle.png",height=9,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1)
dev.off()

#juveniles
juv_loggerhead_turtle_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/Richards Turtle Grids",layer="CC_Grid_1deg_GoMex_utm17")
juv_loggerhead_turtle_utm<-st_as_sf(spTransform(juv_loggerhead_turtle_utm,CRS("+init=epsg:26917")))
juv_loggerhead_turtle_utm$aboveMedian<-ifelse(juv_loggerhead_turtle_utm$CC817max>median(juv_loggerhead_turtle_utm$CC817max,na.rm=T),1,0) #if grid above median
juv_loggerhead_turtle_utm$LOGGERHEAD<-ifelse(juv_loggerhead_turtle_utm$aboveMedian==1,LOGGERHEAD,ifelse(is.na(juv_loggerhead_turtle_utm$aboveMedian),NA,DEFAULT_ESA))
juv_loggerhead_turtle_utm$max_N<-juv_loggerhead_turtle_utm$CC817max
p3<-plot_N(juv_loggerhead_turtle_utm,"Loggerhead Sea Turtle (Juvenile)")
p4<-plot_Score(juv_loggerhead_turtle_utm,"Loggerhead Sea Turtle (Juvenile)","LOGGERHEAD")
png("./JuvLoggerheadSeaTurtle.png",height=9,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1)
dev.off()

png("./LoggerheadSeaTurtle_all.png",height=6.5,width=9,res=300,units="in")
egg::ggarrange(p1,p3,p2,p4,ncol=2,nrow=2,labels=c("A","B","C","D"),label.args=list(gp=gpar(font=4)))
dev.off()


green_HUA<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/finalSeaTurtleAoaFiles",layer="highUseArea_greenTurtle")
green_MC<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/finalSeaTurtleAoaFiles",layer="migCor_greenTurtle")
kemps_HUA<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/finalSeaTurtleAoaFiles",layer="highUseArea_kempsRidleyTurtle")
leatherback_HUA<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/finalSeaTurtleAoaFiles",layer="highUseArea_leatherbackTurtle")
loggerhead_HUA<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/finalSeaTurtleAoaFiles",layer="highUseArea_loggerheadTurtle")
loggerhead_MC<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/finalSeaTurtleAoaFiles",layer="migCor_loggerheadTurtle")
hawksbill_MC<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture/finalSeaTurtleAoaFiles",layer="migCor_hawksbillTurtle")

#only include the identified areas
kemps<-st_as_sf(spTransform(kemps_HUA,CRS("+init=epsg:26917")))
leatherback<-st_as_sf(spTransform(leatherback_HUA,CRS("+init=epsg:26917")))
hawksbill<-st_as_sf(spTransform(hawksbill_MC,CRS("+init=epsg:26917")))

temp<-spTransform(green_HUA,CRS("+init=epsg:26917"))
temp2<-spTransform(green_MC,CRS("+init=epsg:26917"))
temp<-st_buffer(st_as_sf(temp[which(temp@data$areaType=="green turtle high-use area"),]),dist=0)
temp2<-st_as_sf(temp2[which(temp2@data$areaType=="green turtle high-use migratory corridor"),])
green<-st_union(temp,temp2)

temp<-spTransform(loggerhead_HUA,CRS("+init=epsg:26917"))
temp2<-spTransform(loggerhead_MC,CRS("+init=epsg:26917"))
temp<-st_buffer(st_as_sf(temp[which(temp@data$areaType=="loggerhead high-use area"),]),dist=0)
temp2<-st_as_sf(temp2[which(temp2@data$areaType=="loggerhead high-use migratory corridor"),])
loggerhead<-st_union(temp,temp2)
kemps<-kemps[which(kemps$hua==1),];summary(kemps)
leatherback<-leatherback[which(leatherback$hua==1),];summary(leatherback)
hawksbill<-hawksbill[which(hawksbill$areaType=="hawksbill high-use migratory corridor"),];summary(hawksbill)

plot_N(shelf_loggerhead_utm,"Loggerhead Sea Turtle (Shelf)")+
  geom_sf(loggerhead,mapping=aes(),fill=NA,col="red")+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")
plot_N(shelf_green_turtle_utm,"Green Sea Turtle (Shelf)")+
  geom_sf(green,mapping=aes(),fill=NA,col="red")+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")
plot_N(shelf_leatherback_utm,"Leatherback Sea Turtle (Shelf)")+
  geom_sf(leatherback,mapping=aes(),fill=NA,col="red")+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")
plot_N(shelf_kemps_utm,"Kemp's Ridley Sea Turtle (Shelf)")+
  geom_sf(kemps,mapping=aes(),fill=NA,col="red")+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")



#### 2.4 Sawfish ####
# sawfish_encounter<-read.csv("./sawfish_encounters.csv",stringsAsFactors = F)
# sawfish_sattag<-read.csv("./Jasmin_Graham_Sawfish_Pts/For Adam/Allsatdata_uniquePTT_sawfish.csv",stringsAsFactors = F)
# sawfish_acoustictag<-read.csv("./Jasmin_Graham_Sawfish_Pts/For Adam/fullsawfishdataset(final).csv",stringsAsFactors = F)
# sawfish_encounter$Source<-"Encounter"
# sawfish_sattag$Source<-"Sat_tag"
# sawfish_acoustictag$Source<-"AT_tag"
# 
# names(sawfish_encounter)
# names(sawfish_sattag)
# names(sawfish_acoustictag)
# 
# sawfish_sattag$Lat<-sawfish_sattag$latitude
# sawfish_sattag$Long<-sawfish_sattag$longitude
# sawfish_sattag$Year<-year(as.POSIXlt(sawfish_sattag$time,tz="UTC"))
# sawfish_acoustictag$Lat<-sawfish_acoustictag$LatDD
# sawfish_acoustictag$Long<-sawfish_acoustictag$LonDD
# sawfish_acoustictag$datetime<-paste0("20",sawfish_acoustictag$Date.and.Time..UTC.)
# sawfish_acoustictag$Year<-year(as.POSIXlt(sawfish_acoustictag$datetime,format="%Y-%m-%d %H:%M:%S",tz="UTC"))
# sawfish_pts<-rbind(sawfish_encounter[,c("Year","Lat","Long","Source")],sawfish_sattag[,c("Year","Lat","Long","Source")],sawfish_acoustictag[,c("Year","Lat","Long","Source")])
# sawfish_pts<-sawfish_pts[which(sawfish_pts$Lat != 0 & sawfish_pts$Long !=0 & sawfish_pts$Long < -80),] #eliminate bad points and also retain only Gulf of Mexico
# 
# coordinates(sawfish_pts)<-c("Long","Lat")
# proj4string(sawfish_pts)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# gulf_eez<-readOGR("G:/PRODUCTS/person/africk/eez/marinecadastre_gov", "Gulf_of_Mexico_EEZ_po")
# 
# utm_crs<-CRS("+init=epsg:26917") #NAD83 Zone 17N
# sawfish_NAD83_17N<-spTransform(sawfish_pts,utm_crs)
# gulf_eez_NAD83_17N<-spTransform(gulf_eez,utm_crs)
# 
# sawfish_gulf<-sawfish_NAD83_17N[gulf_eez_NAD83_17N,]
# plot(sawfish_gulf)
# crs(sawfish_gulf)
# 
# # sawfish_KUD_input<-as(sawfish_NAD83_17N,"SpatialPoints")
# # str(sawfish_KUD_input)
# # plot(sawfish_KUD_input)
# sawfish_gulf$id<-1
# 
# #sawfish.kde<-kernelUD(as(sawfish_gulf[,"id"],"SpatialPoints"),h="LSCV",grid=1000)
# sawfish.kde<-kernelUD(as(sawfish_gulf[,"id"],"SpatialPoints"),grid=1000)
# sawfish.range50 <- getverticeshr(sawfish.kde, percent = 50)
# sawfish.range95 <- getverticeshr(sawfish.kde, percent = 95)
# sawfish.range99 <- getverticeshr(sawfish.kde, percent = 99)
# 
# windows()
# plot(sawfish_gulf)
# plot(sawfish.range50,border="green",add=T)
# plot(sawfish.range95,border="blue",add=T)
# plot(sawfish.range99,border="red",add=T)
# 
# sawfish_UTM<-st_as_sf(spTransform(sawfish.range95,crs(PAOA_C_GRID_hawksbill_sf)))
# 
# writeOGR(sawfish.range95,dsn="C:/Users/nick.farmer/Documents/GIS/Data/Protected Species/Aquaculture",layer="sawfish_range95_ALL_data",driver="ESRI Shapefile",overwrite_layer = T)

sawfish_hua<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Aquaculture",layer="sawfish_range95_ALL_data")
sawfish_hua<-st_as_sf(sawfish_hua)

#Critical Habitat
sawfish_CH<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="sawfish_CH")
sawfish_CH_utm<-spTransform(sawfish_CH,CRS("+init=epsg:26917"))
sawfish_CH_utm<-st_as_sf(sawfish_CH_utm)

#Section 7 Consultation Layers
sawfish_S7<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="sawfish_S7_dissolve")
sawfish_S7_utm<-spTransform(sawfish_S7,CRS("+init=epsg:26917"))
sawfish_S7_utm<-st_as_sf(sawfish_S7_utm)
windows(); ggplot()+ geom_sf(data=sawfish_S7_utm,fill="blue")

#assign sawfish scores
sawfish_utm1<-st_union(sawfish_hua,sawfish_CH_utm)
sawfish_utm1$SAWFISH<-SAWFISH #score based on table

windows(); ggplot()+ geom_sf(data=sawfish_utm1,fill="red")

sawfish_hua1 <- sawfish_utm1 %>%
  st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(centrum = T) # return back the data value 
windows(); ggplot()+ geom_sf(data=sawfish_hua1,fill="red")

sawfish_S7_LUA<-sf::st_difference(sawfish_S7_utm,sawfish_hua1)
sawfish_S7_LUA<-st_as_sf(sawfish_S7_LUA,crs="epsg:26917")
windows(); ggplot()+ geom_sf(data=sawfish_hua1,aes(),lwd=0,colour=NA,fill="red")+ geom_sf(data=sawfish_S7_LUA,aes(),lwd=0,colour=NA,fill="blue")
sawfish_S7_LUA$SAWFISH<-0.5
sawfish_hua1$SAWFISH<-SAWFISH

writeOGR(as(sawfish_hua1,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="sawfish_hua1",driver="ESRI Shapefile",overwrite_layer = T)
writeOGR(as(sawfish_S7_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="sawfish_S7_utm",driver="ESRI Shapefile",overwrite_layer = T)

sawfish_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="sawfish_utm")
sawfish_utm<-st_as_sf(sawfish_utm)

p1<-ggplot()+ geom_sf(data=sawfish_S7_utm,mapping=aes(),fill="yellow",alpha=0.25,color="yellow")+
  geom_sf(data=sawfish_hua,mapping=aes(),fill="red",alpha=0.5,color="red")+
  geom_sf(data=sawfish_CH_utm,mapping=aes(),fill="blue",alpha=0.8,color="blue")+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Smalltooth Sawfish (US DPS)",x="",y="",tag="A")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = 'bl')+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(legend.position = c(0.9,0.15),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)
p2<-plot_Score(sawfish_utm,"Smalltooth Sawfish (US DPS)","SAWFISH")
png("./Sawfish.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.6 Gulf Sturgeon ####
gulfsturgeon_S7<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="gulfsturgeon_S7_dissolve")
gulfsturgeon_CH<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="gulfsturgeon_CH")
gulfsturgeon_S7_utm<-spTransform(gulfsturgeon_S7,CRS("+init=epsg:26917"))
gulfsturgeon_S7_utm<-st_as_sf(gulfsturgeon_S7_utm)
gulfsturgeon_CH_utm<-st_as_sf(spTransform(gulfsturgeon_CH,CRS("+init=epsg:26917")))

ggplot(gulfsturgeon_S7_utm)+geom_sf()
#note Gulf sturgeon score is 0.5 so no need to differentiate between layers unless we decide S7 ==1
gulfsturgeon_S7_utm$STURGEON=STURGEON
writeOGR(as(gulfsturgeon_S7_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="gulfsturgeon_utm",driver="ESRI Shapefile",overwrite_layer = T)

p1<-ggplot()+ geom_sf(data=gulfsturgeon_S7_utm,mapping=aes(),fill="yellow",alpha=0.25,color="yellow")+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Gulf Sturgeon",x="",y="",tag="A")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  geom_sf(data=gulfsturgeon_CH_utm,mapping=aes(),fill="blue",alpha=0.8,color="blue")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = 'bl')+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(legend.position = c(0.9,0.15),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)
p2<-plot_Score(gulfsturgeon_S7_utm,"Gulf Sturgeon","STURGEON")
png("./GulfSturgeon.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.7 Oceanic Whitetip Shark ####
owt_S7<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="owt_S7_dissolve")
owt_S7_utm<-st_as_sf(spTransform(owt_S7,CRS("+init=epsg:26917")))
owt_EFH<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="owt_EFH")
owt_EFH_utm<-st_as_sf(spTransform(owt_EFH,CRS("+init=epsg:26917")))

#the following layer was developed in ArcMap using Analysis>Overlap>Erase to erase EFH from S7, then
#Analysis>Overlap>Union to combine EFH with S7, then Attribute Table>Add Field>OWT
#then select record non-EFH, Field Calculator: OWT=0.5; reverse selection OWT=0.4
owt_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="owt_utm")
owt_utm<-spTransform(owt_utm,CRS("+init=epsg:26917"))
owt_utm<-st_as_sf(owt_utm)

p1<-ggplot()+ geom_sf(data=owt_S7_utm,mapping=aes(),fill="yellow",alpha=0.25,color="yellow")+
  geom_sf(data=owt_EFH_utm,mapping=aes(),fill="blue",alpha=0.8,color="blue")+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Oceanic Whitetip Shark",x="",y="",tag="A")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = 'bl')+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(legend.position = c(0.9,0.15),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)
p2<-plot_Score(owt_utm,"Oceanic Whitetip Shark","OWT")
png("./OceanicWhitetip.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.8 Sperm Whales ####
sperm_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="sperm_whale_max_n")
sperm_utm<-st_as_sf(sperm_utm)
sperm_utm$aboveMedian<-ifelse(sperm_utm$max_N>median(sperm_utm$max_N,na.rm=T),1,0)
sperm_utm$SPERM<-ifelse(sperm_utm$aboveMedian==1,SPERM,ifelse(is.na(sperm_utm$aboveMedian),NA,DEFAULT_ESA)) #0.2 for endangered/unknown; 0.5 for general range
writeOGR(as(sperm_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power",layer="sperm_utm",driver="ESRI Shapefile",overwrite_layer = T)
p1<-plot_N(sperm_utm,"Sperm Whale")
p2<-plot_Score(sperm_utm,"Sperm Whale","SPERM")
png("./SpermWhale.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.9 Beaked Whales ####
beaked_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="beaked_whale_max_n")
beaked_utm<-st_as_sf(beaked_utm)
beaked_utm$aboveMedian<-ifelse(beaked_utm$max_N>median(beaked_utm$max_N,na.rm=T),1,0)
beaked_utm$BEAKED<-ifelse(beaked_utm$aboveMedian==1,BEAKED,ifelse(is.na(beaked_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(beaked_utm,"Beaked Whale")
p2<-plot_Score(beaked_utm,"Beaked Whale","BEAKED")
png("./BeakedWhale.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.10 Blackfish (false killer whales, pygmy killer whales, and melon-headed whales) ####
blackfish_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="blackfish_max_n")
blackfish_utm<-st_as_sf(blackfish_utm)
blackfish_utm$aboveMedian<-ifelse(blackfish_utm$max_N>median(blackfish_utm$max_N,na.rm=T),1,0)
blackfish_utm$BLACKFISH<-ifelse(blackfish_utm$aboveMedian==1,BLACKFISH,ifelse(is.na(blackfish_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(blackfish_utm,"False Killer, Pygmy Killer, & Melon-Headed Whales")
p2<-plot_Score(blackfish_utm,"False Killer, Pygmy Killer, & Melon-Headed Whales","BLACKFISH")
png("./BlackfishWhale.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.11 Clymene Dolphin ####
clymene_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="clymene_max_n")
clymene_utm<-st_as_sf(clymene_utm)
clymene_utm$aboveMedian<-ifelse(clymene_utm$max_N>median(clymene_utm$max_N,na.rm=T),1,0)
clymene_utm$CLYMENE<-ifelse(clymene_utm$aboveMedian==1,CLYMENE,ifelse(is.na(clymene_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(clymene_utm,"Clymene Dolphin")
p2<-plot_Score(clymene_utm,"Clymene Dolphin","CLYMENE")
png("./ClymeneDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.12 Kogia ####
kogia_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="kogia_max_n")
kogia_utm<-st_as_sf(kogia_utm)
kogia_utm$aboveMedian<-ifelse(kogia_utm$max_N>median(kogia_utm$max_N,na.rm=T),1,0)
kogia_utm$KOGIA<-ifelse(kogia_utm$aboveMedian==1,KOGIA,ifelse(is.na(kogia_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(kogia_utm,"Kogia")
p2<-plot_Score(kogia_utm,"Kogia","KOGIA")
png("./Kogia.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.13 Oceanic Bottlenose Dolphin ####
oceanic_bnd_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="oceanic_bnd_max_n")
oceanic_bnd_utm<-st_as_sf(oceanic_bnd_utm)
oceanic_bnd_utm$aboveMedian<-ifelse(oceanic_bnd_utm$max_N>median(oceanic_bnd_utm$max_N,na.rm=T),1,0)
oceanic_bnd_utm$O_BND<-ifelse(oceanic_bnd_utm$aboveMedian==1,O_BND,ifelse(is.na(oceanic_bnd_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(oceanic_bnd_utm,"Bottlenose Dolphin (Oceanic)")
p2<-plot_Score(oceanic_bnd_utm,"Bottlenose Dolphin (Oceanic)","O_BND")
png("./OceanicBottlenoseDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.14 Pilot Whale ####
oceanic_pilot_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="oceanic_pilot_max_n")
oceanic_pilot_utm<-st_as_sf(oceanic_pilot_utm)
oceanic_pilot_utm$aboveMedian<-ifelse(oceanic_pilot_utm$max_N>median(oceanic_pilot_utm$max_N,na.rm=T),1,0)
oceanic_pilot_utm$PILOT<-ifelse(oceanic_pilot_utm$aboveMedian==1,PILOT,ifelse(is.na(oceanic_pilot_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(oceanic_pilot_utm,"Pilot Whale (Oceanic)")
p2<-plot_Score(oceanic_pilot_utm,"Pilot Whale (Oceanic)","PILOT")
png("./OceanicPilotWhale.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()


#### 2.15 Risso's Dolphin ####
oceanic_rissos_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="oceanic_rissos_max_n")
oceanic_rissos_utm<-st_as_sf(oceanic_rissos_utm)
oceanic_rissos_utm$aboveMedian<-ifelse(oceanic_rissos_utm$max_N>median(oceanic_rissos_utm$max_N,na.rm=T),1,0)
oceanic_rissos_utm$RISSOS<-ifelse(oceanic_rissos_utm$aboveMedian==1,RISSOS,ifelse(is.na(oceanic_rissos_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(oceanic_rissos_utm,"Risso's Dolphin (Oceanic)")
p2<-plot_Score(oceanic_rissos_utm,"Risso's Dolphin (Oceanic)","RISSOS")
png("./OceanicRissosDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.16 Pantropical Spotted Dolphin ####
oceanic_satt_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="oceanic_satt_max_n")
oceanic_satt_utm<-st_as_sf(oceanic_satt_utm)
oceanic_satt_utm$aboveMedian<-ifelse(oceanic_satt_utm$max_N>median(oceanic_satt_utm$max_N,na.rm=T),1,0)
oceanic_satt_utm$PANTROPICAL<-ifelse(oceanic_satt_utm$aboveMedian==1,PANTROPICAL,ifelse(is.na(oceanic_satt_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(oceanic_satt_utm,"Pantropical Spotted Dolphin (Oceanic)")
p2<-plot_Score(oceanic_satt_utm,"Pantropical Spotted Dolphin (Oceanic)","PANTROPICAL")
png("./OceanicPantropicalSpottedDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.17 Atlantic Spotted Dolphin ####
oceanic_sf_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="oceanic_sf_max_n")
oceanic_sf_utm<-st_as_sf(oceanic_sf_utm)
oceanic_sf_utm$aboveMedian<-ifelse(oceanic_sf_utm$max_N>median(oceanic_sf_utm$max_N,na.rm=T),1,0)
oceanic_sf_utm$O_ATLSD<-ifelse(oceanic_sf_utm$aboveMedian==1,O_ATLSD,ifelse(is.na(oceanic_sf_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(oceanic_sf_utm,"Atlantic Spotted Dolphin (Oceanic)")
p2<-plot_Score(oceanic_sf_utm,"Atlantic Spotted Dolphin (Oceanic)","O_ATLSD")
png("./OceanicAtlanticSpottedDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.18 Shelf Bottlenose Dolphin ####
shelf_bnd_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="shelf_bnd_max_n")
shelf_bnd_utm<-st_as_sf(shelf_bnd_utm)
shelf_bnd_utm$aboveMedian<-ifelse(shelf_bnd_utm$max_N>median(shelf_bnd_utm$max_N,na.rm=T),1,0)
shelf_bnd_utm$S_BND<-ifelse(shelf_bnd_utm$aboveMedian==1,S_BND,ifelse(is.na(shelf_bnd_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(shelf_bnd_utm,"Bottlenose Dolphin (Shelf)")
p2<-plot_Score(shelf_bnd_utm,"Bottlenose Dolphin (Shelf)","S_BND")
png("./ShelfBottlenoseDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.18 Shelf Atlantic Spotted Dolphin ####
shelf_frontalis_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="shelf_frontalis_max_n")
shelf_frontalis_utm<-st_as_sf(shelf_frontalis_utm)
shelf_frontalis_utm$aboveMedian<-ifelse(shelf_frontalis_utm$max_N>median(shelf_frontalis_utm$max_N,na.rm=T),1,0)
shelf_frontalis_utm$S_ATLSD<-ifelse(shelf_frontalis_utm$aboveMedian==1,S_ATLSD,ifelse(is.na(shelf_frontalis_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(shelf_frontalis_utm,"Atlantic Spotted Dolphin (Shelf)")
p2<-plot_Score(shelf_frontalis_utm,"Atlantic Spotted Dolphin (Shelf)","S_ATLSD")
png("./ShelfAtlanticSpottedDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.19 Spinner Dolphin ####
spinner_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="spinner_max_n")
spinner_utm<-st_as_sf(spinner_utm)
spinner_utm$aboveMedian<-ifelse(spinner_utm$max_N>median(spinner_utm$max_N,na.rm=T),1,0)
spinner_utm$SPINNER<-ifelse(spinner_utm$aboveMedian==1,SPINNER,ifelse(is.na(spinner_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(spinner_utm,"Spinner Dolphin")
p2<-plot_Score(spinner_utm,"Spinner Dolphin","SPINNER")
png("./SpinnerDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 2.20 Striped Dolphin ####
striped_utm<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density 20220513/GoMMAPPS Maximum Density",layer="striped_max_n")
striped_utm<-st_as_sf(striped_utm)
striped_utm$aboveMedian<-ifelse(striped_utm$max_N>median(striped_utm$max_N,na.rm=T),1,0)
striped_utm$STRIPED<-ifelse(striped_utm$aboveMedian==1,STRIPED,ifelse(is.na(striped_utm$aboveMedian),NA,DEFAULT_MMPA))
p1<-plot_N(striped_utm,"Striped Dolphin")
p2<-plot_Score(striped_utm,"Striped Dolphin","STRIPED")
png("./StripedDolphin.png",height=6.5,width=6.5,res=300,units="in")
egg::ggarrange(p1,p2,ncol=1,labels=c("A","B"),label.args=list(gp=gpar(font=4)))
dev.off()

#### 3.0 Output Final Layers ####
#final layer names
writeOGR(as(manta_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="manta_utm",driver="ESRI Shapefile")
writeOGR(as(rices_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="rices_utm",driver="ESRI Shapefile")
writeOGR(as(shelf_green_turtle_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="shelf_green_turtle_utm",driver="ESRI Shapefile")
writeOGR(as(shelf_kemps_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="shelf_kemps_utm",driver="ESRI Shapefile")
writeOGR(as(shelf_leatherback_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="shelf_leatherback_utm",driver="ESRI Shapefile")
writeOGR(as(shelf_loggerhead_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="shelf_loggerhead_utm",driver="ESRI Shapefile")
writeOGR(as(juv_green_turtle_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="juv_green_turtle_utm",driver="ESRI Shapefile")
writeOGR(as(juv_kemps_turtle_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="juv_kemps_turtle_utm",driver="ESRI Shapefile")
writeOGR(as(juv_loggerhead_turtle_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="juv_loggerhead_turtle_utm",driver="ESRI Shapefile")
writeOGR(as(sawfish_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="sawfish_utm",driver="ESRI Shapefile")
writeOGR(as(gulfsturgeon_S7_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="gulfsturgeon_S7_utm",driver="ESRI Shapefile")
writeOGR(as(owt_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="owt_utm",driver="ESRI Shapefile")
writeOGR(as(sperm_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="sperm_utm",driver="ESRI Shapefile")
writeOGR(as(beaked_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="beaked_utm",driver="ESRI Shapefile")
writeOGR(as(blackfish_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="blackfish_utm",driver="ESRI Shapefile")
writeOGR(as(clymene_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="clymene_utm",driver="ESRI Shapefile")
writeOGR(as(kogia_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="kogia_utm",driver="ESRI Shapefile")
writeOGR(as(oceanic_bnd_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="oceanic_bnd_utm",driver="ESRI Shapefile")
writeOGR(as(oceanic_rissos_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="oceanic_rissos_utm",driver="ESRI Shapefile")
writeOGR(as(oceanic_pilot_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="oceanic_pilot_utm",driver="ESRI Shapefile")
writeOGR(as(oceanic_satt_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="oceanic_satt_utm",driver="ESRI Shapefile")
writeOGR(as(oceanic_sf_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="oceanic_sf_utm",driver="ESRI Shapefile",overwrite_layer = T)
writeOGR(as(shelf_bnd_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="shelf_bnd_utm",driver="ESRI Shapefile")
writeOGR(as(shelf_frontalis_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="shelf_frontalis_utm",driver="ESRI Shapefile",overwrite_layer = T)
writeOGR(as(spinner_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="spinner_utm",driver="ESRI Shapefile")
writeOGR(as(striped_utm,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="striped_utm",driver="ESRI Shapefile")

#merged all layers in sequence in ArcGIS>Analysis>Overlay>SpatialJoin>MergeRule=Minimum

#### 3.0 FINAL PRD GRID SCORES ####

final_PRD<-readOGR("D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers/Joins",layer="all_spp_joined")
final_PRD<-st_as_sf(spTransform(final_PRD,CRS("+init=epsg:26917")))
colnames(final_PRD)

names(final_PRD)[names(final_PRD)=="GREEN_2"] <- "GREEN"
names(final_PRD)[names(final_PRD)=="KEMPS_Join"] <- "KEMPS"

temp<-final_PRD[,c("HEXID","GoM","BEAKED","BLACKFI","CLYMENE","STURGEON","KOGIA","MANTA_R","O_BND","PILOT","RISSOS",
                   "PANTROP","O_ATLSD","OWT","RICES","SAWFISH","S_BND","S_ATLSD",
                   "SPERM","SPINNER","STRIPED","GREEN","KEMPS","LOGGERHEAD","LEATHER","geometry")]
score_names<-c("BEAKED","BLACKFI","CLYMENE","STURGEON","KOGIA","MANTA_R","O_BND","PILOT","RISSOS",
            "PANTROP","O_ATLSD","OWT","RICES","SAWFISH","S_BND","S_ATLSD",
            "SPERM","SPINNER","STRIPED","GREEN","KEMPS","LOGGERHEAD","LEATHER")
spp_names<-c("Beaked Whale","False Killer, Pygmy Killer, & Melon-Headed Whales","Clymene Dolphin","Gulf Sturgeon",
             "Kogia","Giant Manta Ray","Bottlenose Dolphin (Oceanic)","Pilot Whale","Risso's Dolphin",
              "Pantropical Spotted Dolphin","Atlantic Spotted Dolphin (Oceanic)","Oceanic Whitetip Shark",
              "Rice's Whale","Smalltooth Sawfish (US DPS)","Bottlenose Dolphin (Shelf)","Atlantic Spotted Dolphin (Shelf)",
               "Sperm Whale","Spinner Dolphin","Striped Dolphin",
             "Green Sea Turtle","Kemp's Ridley Sea Turtle","Loggerhead Sea Turtle","Leatherback Sea Turtle")

#these must be in the same order for the code to work
score_names2<-c("STURGEON","OWT","SAWFISH","RICES","MANTA_R",
               "S_ATLSD","O_ATLSD",
               "BEAKED","BLACKFI",
               "S_BND","O_BND",
               "CLYMENE","KOGIA","PANTROP",
               "PILOT","RISSOS",
               "SPERM","SPINNER","STRIPED",
               "GREEN","KEMPS","LOGGERHEAD","LEATHER")
spp_names2<-c("Gulf Sturgeon","Oceanic Whitetip Shark","Smalltooth Sawfish (US DPS)","Rice's Whale","Giant Manta Ray",
             "Atlantic Spotted Dolphin (Shelf)","Atlantic Spotted Dolphin (Oceanic)",
             "Beaked Whale","Blackfish",#: False & Pygmy Killer, & Melon-Headed Whale"
             "Bottlenose Dolphin (Shelf)","Bottlenose Dolphin (Oceanic)",
             "Clymene Dolphin","Kogia","Pantropical Spotted Dolphin",
             "Pilot Whale","Risso's Dolphin",
             "Sperm Whale","Spinner Dolphin","Striped Dolphin",
             "Green Sea Turtle","Kemp's Ridley Sea Turtle","Loggerhead Sea Turtle","Leatherback Sea Turtle")

summary(temp)
for(c in 3:(ncol(temp)-1)){
  for(r in 1:nrow(temp)){
    if(temp[r,c][[1]]==0){
      temp[r,c][[1]]<-1
    }
  }
}

writeOGR(as(temp,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="final_Scored_PRD_WP_Layer",driver="ESRI Shapefile")

summary(temp)
temp$S_ATLSD<-ifelse(temp$S_ATLSD==0.8,0.7,temp$S_ATLSD) #fix scoring error

score_plots2<-lapply(seq_len(length(score_names2)),FUN=function(x) {
  plot_Score_final2(temp,spp_names2[x],score_names2[x],LETTERS[x])
  })


commonLegend<-ggplot()+ geom_sf(data=d,mapping=aes(fill=as.factor(d[,scorename][[1]])),color=NA,show.legend = T,alpha=0)+
  scale_fill_manual(breaks=names(myColors), values=myColors,na.value=NA)+
  geom_blank(worldmap[which(worldmap$region_un=="Americas"),],mapping=aes(),fill="light gray",col="gray")+
  geom_blank(us_geo[which(us_geo$GEOID<=48),],mapping=aes(),fill="light gray",col="gray")+
  labs(fill="Score",x="",y="")+
  geom_blank(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  #labs(title="Legend")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  #ggspatial::annotation_scale(location = 'bl')+
  guides(shape = guide_legend(override.aes = list(size = 1)),fill = guide_legend(override.aes = list(alpha=1)))+
  theme_void()+
  theme(legend.position = c(0.55,0.25),
        legend.direction = "horizontal",
        legend.key.size = unit(0.35, 'cm'), #change legend key size
        legend.key.height = unit(0.35, 'cm'), #change legend key height
        legend.key.width = unit(0.35, 'cm'), #change legend key width
        legend.title = element_text(size=5), #change legend title font size
        legend.text = element_text(size=5)) #change legend text font size

score_plots2[[24]]<-commonLegend

png("./AllScores_commonlegend.png",width=9,height=5.5,res=300,units="in")
ggarrange(plotlist=score_plots2)#,legend="none")#common.legend=T)
dev.off()

summary(temp)

temp$PRODUCT<-apply(st_drop_geometry(temp[,3:25]),1,prod)
temp$ARI_MEAN<-apply(st_drop_geometry(temp[,3:25]),1,mean)
temp$GEO_MEAN<-apply(st_drop_geometry(temp[,3:25]),1,FUN=psych::geometric.mean)
temp$LOWEST<-apply(st_drop_geometry(temp[,3:25]),1,min)

writeOGR(as(temp,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="final_Scored_PRD_WP_Layer",driver="ESRI Shapefile",overwrite=T)

temp<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="final_Scored_PRD_WP_Layer")
temp<-st_as_sf(temp)

#limit final output to US EEZ area
unique(US_MAR_LIMS@data$REGION)
GULF_ATL_EEZ_utm<-spTransform(US_MAR_LIMS[which(US_MAR_LIMS@data$REGION=="Atlantic Coast and Gulf of Mexico"),],CRS("+init=epsg:26917"))
bbox(GULF_ATL_EEZ_utm)
bb<-st_bbox(c(xmin=-1254112.2,xmax=567085.6,ymin=2634320,ymax=3425706.7),crs="epsg:26917")
final_layer<-st_crop(temp,bb)
writeOGR(as(final_layer,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="final_Scored_PRD_WP_Layer_USEEZ",driver="ESRI Shapefile",overwrite=T)

sefsc_layers<-kogia_utm
sefsc_layers$domain<-ifelse((!is.na(kogia_utm$max_N) | !is.na(shelf_bnd_utm$max_N)),1,0)
ggplot()+geom_sf(data=sefsc_layers,aes(fill=domain))
sefsc_layers<-sefsc_layers[which(sefsc_layers$domain==1),]
ggplot()+geom_sf(data=sefsc_layers,aes())
sefsc_layers_utm<-st_transform(sefsc_layers,CRS("+init=epsg:26917"))

sefsc_layers_utm<-st_transform(sefsc_layers_utm,st_crs(final_layer))
sefsc_layers_domain<-st_union(sefsc_layers_utm)
final_layer_domain<-sf::st_intersection(final_layer,sefsc_layers_domain)
final_layer_domain_sf<-st_as_sf(final_layer_domain)

summary(final_layer)

st_write(sefsc_layers_domain,dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="SEFSC_model_domain",driver="ESRI Shapefile",overwrite=T)
#st_write(final_layer_domain_sf,dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="final_Scored_PRD_WP_Layer_domain",driver="ESRI Shapefile",overwrite=T,append=F)
writeOGR(as(final_layer,"Spatial"),dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/Final Layers",layer="final_Scored_PRD_WP_Layer_USEEZ",driver="ESRI Shapefile",overwrite=T)

windows()
brks<-quantile(final_layer_domain$PRODUCT,seq(0,1,0.1))
max_prod<-max(final_layer_domain$PRODUCT,na.rm=T)

p_finalProduct<-ggplot()+ 
  #  geom_sf(data=temp,mapping=aes(fill=PRODUCT),color=NA)+
  #geom_sf(data=st_crop(final_layer,bb),mapping=aes(fill=PRODUCT),color=NA)+
  geom_sf(data=final_layer_domain,mapping=aes(fill=PRODUCT),color=NA)+
  #scale_fill_distiller(palette ="Spectral",direction=1, name="Score",values=brks,limits=c(0,max_prod))+
  scale_fill_distiller(palette ="Spectral",direction=1, name="Score (log10)",trans="log10")+
  geom_sf(worldmap[which(worldmap$region_un=="Americas"),],mapping=aes(),fill="light gray",col=NA)+
  geom_sf(us_geo[which(us_geo$GEOID<=48),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Combined Data Layer: Product",fill="Score (log10)",x="",y="")+ #subtitle="Colors assigned by quantiles",
  geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+
  #  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2057006.8,3425706.7),crs="epsg:26917")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = 'bl')+
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(legend.position = c(0.8,1.1),
        legend.direction = "horizontal",
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.height = unit(0.4, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), #change legend key width
        legend.title = element_text(size=5), #change legend title font size
        legend.text = element_text(size=5,angle=90)) #change legend text font size

png("./CombinedLayer_Product.png",width=9,height=6.5,res=300,units="in")
p_finalProduct
dev.off()

p_finalLowest<-plot_Score_final_bigtext(final_layer_domain,"Combined Data Layer: Lowest","LOWEST","B")

png("./CombinedLayer_Lowest.png",width=9,height=6.5,res=300,units="in")
p_finalLowest
dev.off()

png("./CombinedLayers_Final.png",width=6.5,height=9,res=300,units="in")
egg::ggarrange(p_finalProduct,p_finalLowest,nrow=2,labels = c("A","B"))
dev.off()

final_layer_domain_melt<-melt(st_drop_geometry(final_layer_domain[,c(1:2,26:30)]),id.vars=c("HEXID","GoM"),variable.name="Method")
head(final_layer_domain_melt)

png("./CombinedLayer_Boxplot_Methods.png",height=6.5,width=9,res=300,units="in")
boxplot(final_layer_domain$PRODUCT,final_layer_domain$GEO_MEAN,final_layer_domain$ARI_MEAN,final_layer_domain$LOWEST,
        horizontal=T,
        names=c("Product","Geo_Mean","Ari_Mean","Lowest"))
dev.off()


png("./CombinedLayer_ViolinPlot_Methods.png",height=6.5,width=9,res=300,units="in")
ggplot(final_layer_domain_melt,aes(x=factor(Method),y=value))+geom_violin()+labs(x="Method",y="Score")
dev.off()

#ranks
colnames(final_layer_domain)
final_layer_domain$Rank_Product<-rank(final_layer_domain$PRODUCT,ties.method = "min")
final_layer_domain$Rank_GeoMean<-rank(final_layer_domain$GEO_MEAN,ties.method = "min")
final_layer_domain$Rank_AriMean<-rank(final_layer_domain$ARI_MEAN,ties.method = "min")
final_layer_domain$Rank_Lowest<-rank(final_layer_domain$LOWEST,ties.method = "min")

png("./CombinedLayer_Boxplot_Methods_ranks.png",height=6.5,width=9,res=300,units="in")
boxplot(final_layer_domain$Rank_Product,final_layer_domain$Rank_GeoMean,final_layer_domain$Rank_AriMean,final_layer_domain$Rank_Lowest,
        horizontal=T,
        names=c("Product","Geo_Mean","Ari_Mean","Lowest"))
dev.off()

final_layer_domain_melt2<-melt(st_drop_geometry(final_layer_domain[,c("HEXID","GoM","Rank_Product","Rank_GeoMean","Rank_AriMean","Rank_Lowest")]),id.vars=c("HEXID","GoM"),variable.name="Method")
head(final_layer_domain_melt2)

png("./CombinedLayer_ViolinPlot_Methods_ranks.png",height=6.5,width=9,res=300,units="in")
ggplot(final_layer_domain_melt,aes(x=factor(Method),y=value))+geom_violin()+labs(x="Method",y="Score")
dev.off()

png("./CombinedLayer_Boxplot_Methods_scoresandranks.png",height=9,width=6.5,res=300,units="in")
par(mfrow=c(2,1),mar=c(4,6,2,1))
boxplot(final_layer_domain$PRODUCT,final_layer_domain$GEO_MEAN,final_layer_domain$ARI_MEAN,final_layer_domain$LOWEST,
        horizontal=T,xlab="Suitability Score",yaxt="n")
        title(main="A) Scores",adj=0)
        axis(side=2,las=2,at=c(1,2,3,4),labels=c("Product","Geo_Mean","Ari_Mean","Lowest"))
boxplot(final_layer_domain$Rank_Product,final_layer_domain$Rank_GeoMean,final_layer_domain$Rank_AriMean,final_layer_domain$Rank_Lowest,
        horizontal=T,xlab="Suitability Rank",yaxt="n")
        title(main="B) Ranks",adj=0)
        axis(side=2,las=2,at=c(1,2,3,4),labels=c("Product","Geo_Mean","Ari_Mean","Lowest"))
        
dev.off()

#combo plot of all N scores

manta<-plot_P_nolabels(manta_utm,"Giant Manta Ray")

rices<-ggplot()+ geom_sf(data=rices_CORE_utm_sf,mapping=aes(fill=as.factor(1)),alpha=0.5,color="red",show.legend=TRUE)+
  geom_sf(data=rices_SUITABLE_utm_sf,mapping=aes(fill=as.factor(2)),alpha=0.5,color="blue",show.legend=TRUE)+
  scale_fill_manual(values=c("red","blue"),labels=c("Core Habitat","Suitable Habitat"),guide=guide_legend(override.aes=list(color=NA)))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Rice's Whale",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

sawfish<-ggplot()+ geom_sf(data=sawfish_S7_utm,mapping=aes(fill=as.factor(1)),alpha=0.25,color="yellow")+
  geom_sf(data=sawfish_hua,mapping=aes(fill=as.factor(2)),alpha=0.5,color="red")+
  geom_sf(data=sawfish_CH_utm,mapping=aes(fill=as.factor(3)),alpha=0.8,color="blue")+
  scale_fill_manual(values=c("yellow","red","blue"),labels=c("S7","HUA","Critical Habitat"),guide=guide_legend(override.aes=list(color=NA)))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Smalltooth Sawfish (US DPS)",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

green_adult<-plot_N_nolabels(shelf_green_turtle_utm,"Green Sea Turtle (Shelf)")
green_juv<-plot_N_nolabels(juv_green_turtle_utm,"Green Sea Turtle (Juvenile)")
kemps_adult<-plot_N_nolabels(shelf_kemps_utm,"Kemp's Ridley Sea Turtle (Shelf)")
kemps_juv<-plot_N_nolabels(juv_kemps_turtle_utm,"Kemp's Ridley Sea Turtle (Juvenile)")
leatherback<-plot_N_nolabels(shelf_leatherback_utm,"Leatherback Sea Turtle (Shelf)")
loggerhead_adult<-plot_N_nolabels(shelf_loggerhead_utm,"Loggerhead Sea Turtle (Shelf)")
loggerhead_juv<-plot_N_nolabels(juv_loggerhead_turtle_utm,"Loggerhead Sea Turtle (Juvenile)")
owt<-ggplot()+ geom_sf(data=owt_S7_utm,mapping=aes(fill=as.factor(1)),alpha=0.25,color="yellow")+
  geom_sf(data=owt_EFH_utm,mapping=aes(fill=as.factor(2)),alpha=0.5,color="blue")+
  scale_fill_manual(values=c("yellow","blue"),labels=c("S7","EFH"),guide=guide_legend(override.aes=list(color=NA)))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Oceanic Whitetip Shark",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

sperm<-plot_N_nolabels(sperm_utm,"Sperm Whale")
beaked<-plot_N_nolabels(beaked_utm,"Beaked Whale")
blackfish<-plot_N_nolabels(blackfish_utm,"Blackfish")
clymene<-plot_N_nolabels(clymene_utm,"Clymene Dolphin")
kogia<-plot_N_nolabels(kogia_utm,"Kogia")
obnd<-plot_N_nolabels(oceanic_bnd_utm,"Bottlenose Dolphin (Oceanic)")
opw<-plot_N_nolabels(oceanic_pilot_utm,"Pilot Whale (Oceanic)")
rissos<-plot_N_nolabels(oceanic_rissos_utm,"Risso's Dolphin (Oceanic)")
ptsd<-plot_N_nolabels(oceanic_satt_utm,"Pantropical Spotted Dolphin (Oceanic)")
oasd<-plot_N_nolabels(oceanic_sf_utm,"Atlantic Spotted Dolphin (Oceanic)")
sbnd<-plot_N_nolabels(shelf_bnd_utm,"Bottlenose Dolphin (Shelf)")
sasd<-plot_N_nolabels(shelf_frontalis_utm,"Atlantic Spotted Dolphin (Shelf)")
spinner<-plot_N_nolabels(spinner_utm,"Spinner Dolphin")
striped<-plot_N_nolabels(striped_utm,"Striped Dolphin")
sturgeon<-ggplot()+ 
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  geom_sf(data=gulfsturgeon_S7_utm,mapping=aes(fill=as.factor(1)),alpha=0.25,color="yellow")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  geom_sf(data=gulfsturgeon_CH_utm,mapping=aes(fill=as.factor(2)),alpha=0.8,color="blue")+
  scale_fill_manual(values=c("yellow","blue"),labels=c("S7","Critical Habitat"),guide=guide_legend(override.aes=list(color=NA)))+
  labs(title="Gulf Sturgeon",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

plotlist_N<-list(sturgeon,owt,sawfish,rices,manta,sasd,oasd,beaked,blackfish,sbnd,obnd,clymene,
                 kogia,ptsd,opw,rissos,sperm,spinner,striped,green_juv,green_adult,kemps_juv,kemps_adult,
                 loggerhead_juv,loggerhead_adult,leatherback)

png("./All_Spp_N.png",width=9,height=6,res=300,units="in")
ggarrange(plotlist=plotlist_N)#,labels="AUTO",label.args=list(gp=gpar(font=4), x=unit(1,"line"), y=unit(-1,"line"),hjust=0))
dev.off()


#combo plot of all N scores w/ labels

manta<-plot_P_nolabels(manta_utm,"E) Giant Manta Ray")

rices<-ggplot()+ geom_sf(data=rices_CORE_utm_sf,mapping=aes(fill=as.factor(1)),alpha=0.5,color="red",show.legend=TRUE)+
  geom_sf(data=rices_SUITABLE_utm_sf,mapping=aes(fill=as.factor(2)),alpha=0.5,color="blue",show.legend=TRUE)+
  scale_fill_manual(values=c("red","blue"),labels=c("Core Habitat","Suitable Habitat"),guide=guide_legend(override.aes=list(color=NA)))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="D) Rice's Whale",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

sawfish<-ggplot()+ geom_sf(data=sawfish_S7_utm,mapping=aes(fill=as.factor(1)),alpha=0.25,color="yellow")+
  geom_sf(data=sawfish_hua,mapping=aes(fill=as.factor(2)),alpha=0.5,color="red")+
  geom_sf(data=sawfish_CH_utm,mapping=aes(fill=as.factor(3)),alpha=0.8,color="blue")+
  scale_fill_manual(values=c("yellow","red","blue"),labels=c("S7","HUA","Critical Habitat"),guide=guide_legend(override.aes=list(color=NA)))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="C) Smalltooth Sawfish (US DPS)",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

green_adult<-plot_N_nolabels(shelf_green_turtle_utm,"U) Green Sea Turtle (Shelf)")
green_juv<-plot_N_nolabels(juv_green_turtle_utm,"T) Green Sea Turtle (Juvenile)")
kemps_adult<-plot_N_nolabels(shelf_kemps_utm,"W) Kemp's Ridley Sea Turtle (Shelf)")
kemps_juv<-plot_N_nolabels(juv_kemps_turtle_utm,"V) Kemp's Ridley Sea Turtle (Juvenile)")
leatherback<-plot_N_nolabels(shelf_leatherback_utm,"Z) Leatherback Sea Turtle (Shelf)")
loggerhead_adult<-plot_N_nolabels(shelf_loggerhead_utm,"Y) Loggerhead Sea Turtle (Shelf)")
loggerhead_juv<-plot_N_nolabels(juv_loggerhead_turtle_utm,"X) Loggerhead Sea Turtle (Juvenile)")
owt<-ggplot()+ geom_sf(data=owt_S7_utm,mapping=aes(fill=as.factor(1)),alpha=0.25,color="yellow")+
  geom_sf(data=owt_EFH_utm,mapping=aes(fill=as.factor(2)),alpha=0.5,color="blue")+
  scale_fill_manual(values=c("yellow","blue"),labels=c("S7","EFH"),guide=guide_legend(override.aes=list(color=NA)))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="B) Oceanic Whitetip Shark",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

sperm<-plot_N_nolabels(sperm_utm,"Q) Sperm Whale")
beaked<-plot_N_nolabels(beaked_utm,"H) Beaked Whale")
blackfish<-plot_N_nolabels(blackfish_utm,"I) Blackfish")
clymene<-plot_N_nolabels(clymene_utm,"L) Clymene Dolphin")
kogia<-plot_N_nolabels(kogia_utm,"M) Kogia")
obnd<-plot_N_nolabels(oceanic_bnd_utm,"K) Bottlenose Dolphin (Oceanic)")
opw<-plot_N_nolabels(oceanic_pilot_utm,"O) Pilot Whale (Oceanic)")
rissos<-plot_N_nolabels(oceanic_rissos_utm,"P) Risso's Dolphin (Oceanic)")
ptsd<-plot_N_nolabels(oceanic_satt_utm,"N) Pantropical Spotted Dolphin (Oceanic)")
oasd<-plot_N_nolabels(oceanic_sf_utm,"G) Atlantic Spotted Dolphin (Oceanic)")
sbnd<-plot_N_nolabels(shelf_bnd_utm,"J) Bottlenose Dolphin (Shelf)")
sasd<-plot_N_nolabels(shelf_frontalis_utm,"F) Atlantic Spotted Dolphin (Shelf)")
spinner<-plot_N_nolabels(spinner_utm,"R) Spinner Dolphin")
striped<-plot_N_nolabels(striped_utm,"S) Striped Dolphin")
sturgeon<-ggplot()+ 
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  geom_sf(data=gulfsturgeon_S7_utm,mapping=aes(fill=as.factor(1)),alpha=0.25,color="yellow")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  geom_sf(data=gulfsturgeon_CH_utm,mapping=aes(fill=as.factor(2)),alpha=0.8,color="blue")+
  scale_fill_manual(values=c("yellow","blue"),labels=c("S7","Critical Habitat"),guide=guide_legend(override.aes=list(color=NA)))+
  labs(title="A) Gulf Sturgeon",x="",y="")+geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  #ggspatial::annotation_north_arrow(which_north = "true",location="tl",height=unit(0.15, "cm"),width = unit(0.15, "cm")) +
  ggspatial::annotation_scale(location = 'bl',height=unit(0.05,'cm'),text_cex=0.25)+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(title = element_text(size=4),
        text = element_text(size=4),
        legend.position = c(.5,-0.5),
        legend.direction = "horizontal",
        plot.margin = unit(c(-0.5,0,-0.5,0),"cm"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size

plotlist_N_labs<-list(sturgeon,owt,sawfish,rices,manta,sasd,oasd,beaked,blackfish,sbnd,obnd,clymene,
                 kogia,ptsd,opw,rissos,sperm,spinner,striped,green_juv,green_adult,kemps_juv,kemps_adult,
                 loggerhead_juv,loggerhead_adult,leatherback)

png("./All_Spp_N_labs.png",width=9,height=6,res=300,units="in")
ggarrange(plotlist=plotlist_N_labs)#,labels="AUTO",label.args=list(gp=gpar(font=4), x=unit(1,"line"), y=unit(-1,"line"),hjust=0))
dev.off()

#### 4.0 Compare to Draft Layer ####
#import draft layer and compare
draftLayer<-readOGR(dsn="D:/Documents/GIS/Data/Protected Species/Wind Power/GoMMAPPS Maximum Density-20220201T153833Z-001/Draft final layers",layer="final_Scored_PRD_WP_Layer")
draftLayer<-st_as_sf(draftLayer)
compare_final_layer<-st_intersection(final_layer_domain,draftLayer)
compare_final_layer$DELTA<-compare_final_layer$PRODUCT - compare_final_layer$PRODUCT.1 #new minus old
compare_final_layer_wea<-st_intersection(compare_final_layer,WEA_CALL)
compare_final_layer_wea$pDELTA<-compare_final_layer_wea$PRODUCT/compare_final_layer_wea$PRODUCT.1 #new minus old/old

png("./Final_minus_Draft_WEA.png",width=9,height=6,res=300,units="in")
ggplot()+ geom_sf(data=compare_final_layer_wea,mapping=aes(fill=DELTA),color=NA)+
  scale_fill_distiller(palette ="Spectral", direction=1,name=c("\u0394 Score"))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Final - Draft Score",x="",y="")+
  geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  #geom_sf(data=GULF_ATL_EEZ_utm_sf,mapping=aes(),fill=NA,col="red")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = 'bl')+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(legend.position = c(0.9,0.85),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)
dev.off()

png("./Final_vs_Draft_WEA_pDelta.png",width=9,height=6,res=300,units="in")
ggplot()+ geom_sf(data=compare_final_layer_wea,mapping=aes(fill=pDELTA),color=NA)+
  scale_fill_distiller(palette ="Spectral", direction=-1,name=c("\u0394 Score"))+
  geom_sf(worldmap[which(worldmap$region_un=="Americas" & worldmap$geounit != "United States of America"),],mapping=aes(),fill="light gray",col="gray")+
  labs(title="Final/Draft",x="",y="")+
  geom_sf(data=WEA_CALL,mapping=aes(),fill=NA,col="black")+
  #geom_sf(data=GULF_ATL_EEZ_utm_sf,mapping=aes(),fill=NA,col="red")+
  geom_sf(us_geo[which(us_geo$GEOID<=50),],mapping=aes(),fill="light gray",col="gray")+
  ggspatial::annotation_north_arrow(which_north = "true",location="tl",
                                    height=unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = 'bl')+
  coord_sf(xlim=c(-1254112.2,567085.6),ylim=c(2634320,3425706.7),crs="epsg:26917")+ #limit to US GoMex
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme(legend.position = c(0.9,0.85),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.25, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size)
dev.off()
boxplot(compare_final_layer_wea$pDELTA,ylab="\u0394 Score",horizontal=T)
