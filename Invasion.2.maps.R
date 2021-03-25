rm(list = ls())
gc()
Dir=c("~/SynologyDrive/FUNDIV - NFI - Europe/")

require(rgdal)
require(adegenet)
require(fields)
library(rworldmap)
library(lattice)
require(spatial.tools)
library(maptools)
require(rworldxtra)
library(rgeos)
library(RStoolbox)
library(ggplot2) 
library(rangeBuilder)
library(fields)
library(raster)
library(rasterVis)
library(cowplot)

CODE <- "QUERUB"
Species <- "Quercus_rubra"
CODE <- "ROBPSE"
Species <- "Robinia_pseudacacia"

df <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot2_",CODE,".rds")) ## Plotdatabase ! 
df_FR <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot2_FR_",CODE,".rds")) ## Plotdatabase ! 

## NA are probably monsospécifi stand where they were no tree at all at T1

df$expansion <- ifelse(df$sp.recruitment.plot.rate=="1",2,ifelse(df$sp.recruitment.plot.rate=="0",0,1)) ## three level for recruitment
df$dominProg <- ifelse(df$Domin.sp.T1%in%Species & df$Domin.sp.T2%in%Species,3, #twice dominant 
                       ifelse(!df$Domin.sp.T1%in%Species & df$Domin.sp.T2%in%Species,2,
                          ifelse(!df$Domin.sp.T1%in%Species & !df$Domin.sp.T2%in%Species,0,1)))

df$dominProg2 <- df$DOMIN.s.T2
df$dominProg2[!is.na(df$DOMIN.s.T1)] <- df$dominProg2[!is.na(df$DOMIN.s.T1)]-df$DOMIN.s.T1[!is.na(df$DOMIN.s.T1)]

df_FR$dominProg2 <- df_FR$DOMIN.s.T1
df_FR$DOMIN.s.T2 <- df_FR$DOMIN.s.T1

df_FR[,"sp.recruitment.plot.rate"] <- NA
df_FR$treeNbrJ.IR <- df_FR$treeNbrJ.A


df <- rbind(df[,c("plotcode","longitude.x","latitude.x",
                  "sp.recruitment.plot.rate","treeNbrJ.IR","dominProg2","DOMIN.s.T1","DOMIN.s.T2")],
            df_FR[,c("plotcode","longitude.x","latitude.x",
                  "sp.recruitment.plot.rate","treeNbrJ.IR","dominProg2","DOMIN.s.T1","DOMIN.s.T2")])
            
            

#|is.na(df$Domin.sp.T1) & df$Domin.sp.T2!="Quercus_rubra"
# Need to map three colors being recrut = 1 VS others VS 0 (comparaison new areas (range expansion) and others) or continous to check 
# first colors = new area = recruted indiv only => Size of the point is number of new indiv = treenbr.R = T2 only 
# second colors = 2 options = somme of new indiv and old or just the number of recrut check with treenbr.R or with treenbrJ + I (alive at T2)
#third color = tree ingrowth trees (without the deat then)
# But also the number of tree ? t1 or t2 ? 

par(mfrow=c(1,1))
  worldmap <- getMap(resolution = "high")
  #europe <- worldmap[which(worldmap$REGION=="Europe"),]
  #europe <- worldmap[which(grepl("Europe", worldmap$GEO3) & as.character(worldmap$NAME) != "Russia"),] 
  europe <- worldmap[which(grepl("Europe", worldmap$GEO3)|grepl("North Africa", worldmap$GEO3)),] 
  europe <-spTransform(europe,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") # Convert to the right system
  europe <- gSimplify(europe, tol = 0.00001)
  europe1 = crop(europe,c(-15, 45, 35, 70))
  europe1 = crop(europe,c(min(df$longitude.x), max(df$longitude.x), min(df$latitude.x), max(df$latitude.x)))

  
  ## percentage of recruitment gvradient !! ADD FRENCH ALSO 
  pobs <- ggplot() + theme(panel.background = element_rect(fill="white", colour="black", size=3, 
                                                           linetype=1, color="black"),legend.key.size = unit(3, "cm"),legend.position=c(0.5,0.5)) +
    theme(panel.grid.major = element_line(colour="blue", size=4, linetype=0,lineend="square", color="red"),
          panel.grid.minor = element_line(colour="blue", size=1, linetype=0,lineend="round", color="blue"))+
    theme(legend.background = element_rect(fill="white",size=1, linetype="solid", 
                                           colour ="black"))+
    #scale_y_continuous(expand = c(0.02,0),limits = c(36,70)) +
    #scale_x_continuous(expand = c(0.02,0),limits = c(-10,32)) +
    coord_fixed(1.3,expand=T) +  
    geom_polygon(data = europe1, aes(x=long, y = lat,group=group),fill=NA,col="black") + #gray 60 & gray 60
    geom_point(data = df, aes(x = longitude.x, y = latitude.x,fill=sp.recruitment.plot.rate, size = treeNbrJ.IR),shape=21,col="black")+ # scale on the fill 
    #geom_point(data = df_FR, aes(x = longitude.x, y = latitude.x,col=dominProg2,size=treeNbrJ.IR))+ # French points 
    #geom_point(data = df_FR, aes(x = longitude.x, y = latitude.x),col="black",fill="black",size=0.5)+
    #scale_colour_manual(values = c("0"="blue","2"="red","1"="green","3"="black"),name="",labels = c("0"="No dominance","1"="Lost dominance at T2","2"="Won dominance at T2","3"="Kept dominance at T2")) +
    #scale_shape_manual(values = c(20,20,20),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)")) +
    #scale_size_manual(values= c(2.5,2.5,2.5),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)"))+ # 0.7 0.5 0.7 de base
    scale_fill_gradient(name="Recruitment rate (%)",low = "yellow", high = "red4", na.value = NA)+
    scale_color_gradient(name="Basal area occupation\n in France (%)", low = "white", high = "black", na.value = NA)+
    guides(size = guide_legend(guide_legend(title="Number of individual")))+    #xlim(-10,32)+
    labs(title=paste0(CODE), y=paste0("Latitude"), x="Longitude")+
    theme(text = element_text(face="bold"),legend.direction ="vertical",
          legend.position = c("right"),
          legend.justification = c("right"),
          axis.text.x = element_text(size=18,color="black"),axis.text.y = element_text(size=18,color="black"),
          axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
          legend.key = element_rect(fill = "white", colour = "black",size = 1),
          legend.background=element_rect(fill="white",colour="black",size=0),
          legend.key.heigh = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.key.size = unit(5,"line"),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          panel.border = element_rect(colour = "black", fill=NA, size=0),
          axis.line = element_line(colour="black"),
          plot.margin = margin(0,0,0,0, "cm"),
          plot.title = element_text(size=12,hjust = 0.02,vjust = -1),
          plot.caption = element_text(face="bold.italic"))
  pobs
  save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/",CODE,"Recruitment.Quanti.map.png"),
            plot = pobs,base_width = 10, base_height = 10, dpi = 300 ,units = "in",nrow=1,ncol=1)
  
  
  
  ## Map with the percentage of cover !
  pobs <- ggplot() + theme(panel.background = element_rect(fill="white", colour="black", size=3, 
                                                           linetype=1, color="black"),legend.key.size = unit(3, "cm"),legend.position=c(0.5,0.5)) +
    theme(panel.grid.major = element_line(colour="blue", size=4, linetype=0,lineend="square", color="red"),
          panel.grid.minor = element_line(colour="blue", size=1, linetype=0,lineend="round", color="blue"))+
    theme(legend.background = element_rect(fill="white",size=1, linetype="solid", 
                                           colour ="black"))+
    #scale_y_continuous(expand = c(0.02,0),limits = c(36,70)) +
    #scale_x_continuous(expand = c(0.02,0),limits = c(-10,32)) +
    coord_fixed(1.3,expand=T) +  
    geom_polygon(data = europe1, aes(x=long, y = lat,group=group),fill=NA,col="black") + #gray 60 & gray 60
    geom_point(data = df, aes(x = longitude.x, y = latitude.x,fill=dominProg2, size = treeNbrJ.IR),shape=21,col="black")+
    #geom_point(data = df_FR, aes(x = longitude.x, y = latitude.x,col=dominProg2,size=treeNbrJ.IR))+ # French points 
    
    #scale_colour_manual(values = c("0"="blue","2"="red","1"="green","3"="black"),name="",labels = c("0"="No dominance","1"="Lost dominance at T2","2"="Won dominance at T2","3"="Kept dominance at T2")) +
    #scale_shape_manual(values = c(20,20,20),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)")) +
    #scale_size_manual(values= c(2.5,2.5,2.5),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)"))+ # 0.7 0.5 0.7 de base
    scale_fill_gradient(name="Change in Basal\n area occupation (%)",low = "yellow", high = "red4", na.value = NA)+
    scale_color_gradient(name="Basal area occupation\n in France (%)", low = "white", high = "black", na.value = NA)+
    guides(size = guide_legend(guide_legend(title="Number of individual")))+    #xlim(-10,32)
    labs(title=paste0(CODE), y=paste0("Latitude"), x="Longitude")+
    theme(text = element_text(face="bold"),legend.direction ="vertical",
          legend.position = c("right"),
          legend.justification = c("right"),
          axis.text.x = element_text(size=18,color="black"),axis.text.y = element_text(size=18,color="black"),
          axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
          legend.key = element_rect(fill = "white", colour = "black",size = 1),
          legend.background=element_rect(fill="white",colour="black",size=0),
          legend.key.heigh = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.key.size = unit(5,"line"),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          panel.border = element_rect(colour = "black", fill=NA, size=0),
          axis.line = element_line(colour="black"),
          plot.margin = margin(0,0,0,0, "cm"),
          plot.title = element_text(size=12,hjust = 0.02,vjust = -1),
          plot.caption = element_text(face="bold.italic"))
  pobs
  save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/",CODE,"Dominance.Quanti.map.png"),
            plot = pobs,base_width = 10, base_height = 10, dpi = 300 ,units = "in",nrow=1,ncol=1)
  
  
  
  
  ## Now do the same thing with T1 and T2 (cover only)
  pobs <- ggplot() + theme(panel.background = element_rect(fill="white", colour="black", size=3, 
                                                           linetype=1, color="black"),legend.key.size = unit(3, "cm"),legend.position=c(0.5,0.5)) +
    theme(panel.grid.major = element_line(colour="blue", size=4, linetype=0,lineend="square", color="red"),
          panel.grid.minor = element_line(colour="blue", size=1, linetype=0,lineend="round", color="blue"))+
    theme(legend.background = element_rect(fill="white",size=1, linetype="solid", 
                                           colour ="black"))+
    #scale_y_continuous(expand = c(0.02,0),limits = c(36,70)) +
    #scale_x_continuous(expand = c(0.02,0),limits = c(-10,32)) +
    coord_fixed(1.3,expand=T) +  
    geom_polygon(data = europe1, aes(x=long, y = lat,group=group),fill=NA,col="black") + #gray 60 & gray 60
    geom_point(data = df[!is.na(df$DOMIN.s.T1),], aes(x = longitude.x, y = latitude.x,fill=DOMIN.s.T1,size = treeNbrJ.IR),shape=21,col="black")+ #important to specify we don't want the NA otherwise they appears white on the map
    #geom_point(data = df_FR, aes(x = longitude.x, y = latitude.x,col=DOMIN.s.T1,size=treeNbrJ.IR))+ # French points 
    #scale_shape_manual(values = c(20,20,20),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)")) +
    #scale_size_manual(values= c(2.5,2.5,2.5),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)"))+ # 0.7 0.5 0.7 de base
    scale_fill_gradient(name="Basal area\n occupation (%)",low = "yellow", high = "red4", na.value = NA)+
    #scale_color_gradient(name="Basal area\n occupation (%)",low = "yellow", high = "red4", na.value = NA)+
    guides(size = guide_legend(guide_legend(title="Number of individual",fill="black")))+    #xlim(-10,32)
    labs(title=paste0(CODE," occupation at T1"), y=paste0("Latitude"), x="Longitude")+
    theme(text = element_text(face="bold"),legend.direction ="vertical",
          legend.position = c("right"),
          legend.justification = c("right"),
          axis.text.x = element_text(size=18,color="black"),axis.text.y = element_text(size=18,color="black"),
          axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
          legend.key = element_rect(fill = "white", colour = "black",size = 1),
          legend.background=element_rect(fill="white",colour="black",size=0),
          legend.key.heigh = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.key.size = unit(5,"line"),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          panel.border = element_rect(colour = "black", fill=NA, size=0),
          axis.line = element_line(colour="black"),
          plot.margin = margin(0,0,0,0, "cm"),
          plot.title = element_text(size=18,hjust = 0.02,vjust = -1),
          plot.caption = element_text(face="bold.italic"))
  pobs
  save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/",CODE,"Dominance.T1.map.png"),
            plot = pobs,base_width = 10, base_height = 10, dpi = 300 ,units = "in",nrow=1,ncol=1)
  
  #T2
  pobs <- ggplot() + theme(panel.background = element_rect(fill="white", colour="black", size=3, 
                                                           linetype=1, color="black"),legend.key.size = unit(3, "cm"),legend.position=c(0.5,0.5)) +
    theme(panel.grid.major = element_line(colour="blue", size=4, linetype=0,lineend="square", color="red"),
          panel.grid.minor = element_line(colour="blue", size=1, linetype=0,lineend="round", color="blue"))+
    theme(legend.background = element_rect(fill="white",size=1, linetype="solid", 
                                           colour ="black"))+
    #scale_y_continuous(expand = c(0.02,0),limits = c(36,70)) +
    #scale_x_continuous(expand = c(0.02,0),limits = c(-10,32)) +
    coord_fixed(1.3,expand=T) +  
    geom_polygon(data = europe1, aes(x=long, y = lat,group=group),fill=NA,col="black") + #gray 60 & gray 60
    geom_point(data = df[!is.na(df$DOMIN.s.T2),], aes(x = longitude.x, y = latitude.x,fill=DOMIN.s.T2,size = treeNbrJ.IR),shape=21,col="black")+ #important to specify we don't want the NA otherwise they appears white on the map
    #geom_point(data = df_FR, aes(x = longitude.x, y = latitude.x,col=DOMIN.s.T1,size=treeNbrJ.IR))+ # French points 
    #scale_shape_manual(values = c(20,20,20),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)")) +
    #scale_size_manual(values= c(2.5,2.5,2.5),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)"))+ # 0.7 0.5 0.7 de base
    scale_fill_gradient(name="Basal area\n occupation (%)",low = "yellow", high = "red4", na.value = NA)+
    #scale_color_gradient(name="Basal area\n occupation (%)",low = "yellow", high = "red4", na.value = NA)+
    guides(size = guide_legend(guide_legend(title="Number of individual")))+    #xlim(-10,32)
    labs(title=paste0(CODE," occupation at T2"), y=paste0("Latitude"), x="Longitude")+
    theme(text = element_text(face="bold"),legend.direction ="vertical",
          legend.position = c("right"),
          legend.justification = c("right"),
          axis.text.x = element_text(size=18,color="black"),axis.text.y = element_text(size=18,color="black"),
          axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
          legend.key = element_rect(fill = "white", colour = "black",size = 1),
          legend.background=element_rect(fill="white",colour="black",size=0),
          legend.key.heigh = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.key.size = unit(5,"line"),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          panel.border = element_rect(colour = "black", fill=NA, size=0),
          axis.line = element_line(colour="black"),
          plot.margin = margin(0,0,0,0, "cm"),
          plot.title = element_text(size=18,hjust = 0.02,vjust = -1),
          plot.caption = element_text(face="bold.italic"))
  pobs
  save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/",CODE,"Dominance.T2.map.png"),
            plot = pobs,base_width = 10, base_height = 10, dpi = 300 ,units = "in",nrow=1,ncol=1)
  
  
  
  
  
  
  
  
  
  ##### Similar maps with categorical data instead of continuous cover and recruitment. 
  
  
  
  
  pobs <- ggplot() + theme(panel.background = element_rect(fill="white", colour="black", size=3, 
                                                           linetype=1, color="black"),legend.key.size = unit(3, "cm"),legend.position=c(0.5,0.5)) +
    theme(panel.grid.major = element_line(colour="blue", size=4, linetype=0,lineend="square", color="red"),
          panel.grid.minor = element_line(colour="blue", size=1, linetype=0,lineend="round", color="blue"))+
    theme(legend.background = element_rect(fill="white",size=1, linetype="solid", 
                                           colour ="black"))+
    #scale_y_continuous(expand = c(0.02,0),limits = c(36,70)) +
    #scale_x_continuous(expand = c(0.02,0),limits = c(-10,32)) +
    coord_fixed(1.3,expand=T) +  
    geom_polygon(data = europe1, aes(x=long, y = lat,group=group),fill=NA,col="black") + #gray 60 & gray 60
    geom_point(data = df, aes(x = longitude.x, y = latitude.x, col=as.factor(expansion), size = treeNbrJ.IR),alpha=0.7)+
    scale_colour_manual(values = c("0"="blue","2"="red","1"="green"),name="",labels = c("0"="No new individual","1"="Some new individual","2"="Only new individual")) +
    #scale_shape_manual(values = c(20,20,20),name="Mortality occurence \n probability",labels = c("High (>Q3)", "Medium \n(Q1 - Q3)","Low (<Q1)")) +
    guides(colour = guide_legend(override.aes = list(size = 5)),
           size = guide_legend(guide_legend(title="Number of individual")))+    #xlim(-10,32)+
    labs(title=paste0(CODE), y=paste0("Latitude"), x="Longitude")+
    theme(text = element_text(face="bold"),legend.direction ="vertical",
          legend.position = c("right"),
          legend.justification = c("right"),
          axis.text.x = element_text(size=18,color="black"),axis.text.y = element_text(size=18,color="black"),
          axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
          legend.key = element_rect(fill = "white", colour = "black",size = 1),
          legend.background=element_rect(fill="white",colour="black",size=0),
          legend.key.heigh = unit(4,"line"),
          legend.key.width = unit(1,"line"),
          legend.key.size = unit(5,"line"),
          legend.text=element_text(size=16),
          legend.title=element_text(size=17),
          panel.border = element_rect(colour = "black", fill=NA, size=0),
          axis.line = element_line(colour="black"),
          plot.margin = margin(0,0,0,0, "cm"),
          plot.title = element_text(size=12,hjust = 0.02,vjust = -1),
          plot.caption = element_text(face="bold.italic"))

pobs    
save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/",CODE,"Expansion.map.png"),
          plot = pobs,base_width = 10, base_height = 10, dpi = 300 ,units = "in",nrow=1,ncol=1)





## same maps with dominance 
## Maybe do a maps of dominance by the species VS dominance by other or three colors =
## dominant at t1 and t2 
## not dominant at t1 also because not there but domin at t2. everything except QUERUB or NA 
## not at t1 also because not there not at t2. everything but QQUERUB and NA 
## at t1 but not at t2
par(mfrow=c(1,1))
pobs <- ggplot() + theme(panel.background = element_rect(fill="white", colour="black", size=3, 
                                                         linetype=1, color="black"),legend.key.size = unit(3, "cm"),legend.position=c(0.5,0.5)) +
  theme(panel.grid.major = element_line(colour="blue", size=4, linetype=0,lineend="square", color="red"),
        panel.grid.minor = element_line(colour="blue", size=1, linetype=0,lineend="round", color="blue"))+
  theme(legend.background = element_rect(fill="white",size=1, linetype="solid", 
                                         colour ="black"))+
  #scale_y_continuous(expand = c(0.02,0),limits = c(36,70)) +
  #scale_x_continuous(expand = c(0.02,0),limits = c(-10,32)) +
  coord_fixed(1.3,expand=T) +  
  geom_polygon(data = europe1, aes(x=long, y = lat,group=group),fill=NA,col="black") + #gray 60 & gray 60
  geom_point(data = df, aes(x = longitude.x, y = latitude.x, col=as.factor(dominProg), size = treeNbrJ.IR),alpha=0.7)+
  scale_colour_manual(values = c("0"="blue","2"="red","1"="green","3"="black"),name="",labels = c("0"="No dominance","1"="Lost dominance at T2","2"="Won dominance at T2","3"="Kept dominance at T2")) +
  guides(colour = guide_legend(override.aes = list(size = 5)),
         size = guide_legend(guide_legend(title="Number of individual")))+    #xlim(-10,32)+
  labs(title=paste0(CODE), y=paste0("Latitude"), x="Longitude")+
  theme(text = element_text(face="bold"),legend.direction ="vertical",
        legend.position = c("right"),
        legend.justification = c("right"),
        axis.text.x = element_text(size=18,color="black"),axis.text.y = element_text(size=18,color="black"),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        legend.key = element_rect(fill = "white", colour = "black",size = 1),
        legend.background=element_rect(fill="white",colour="black",size=0),
        legend.key.heigh = unit(4,"line"),
        legend.key.width = unit(1,"line"),
        legend.key.size = unit(5,"line"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=17),
        panel.border = element_rect(colour = "black", fill=NA, size=0),
        axis.line = element_line(colour="black"),
        plot.margin = margin(0,0,0,0, "cm"),
        plot.title = element_text(size=12,hjust = 0.02,vjust = -1),
        plot.caption = element_text(face="bold.italic"))
pobs
save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/",CODE,"Dominance.Quali.map.png"),
          plot = pobs,base_width = 10, base_height = 10, dpi = 300 ,units = "in",nrow=1,ncol=1)




