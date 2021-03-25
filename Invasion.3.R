rm(list = ls())
gc()
### Analyses invasion 
## Consider the dominant species of the stands 

## Nbr of plots containing the species / nbr total of plots Q1
## Instead of that we can check => Porportion at T1 and proportion at T2 (avant et après) and abosulte numnber)

# Q2 
## Regen on plot with or wothout adult (check at t1 and t2) => only 1 (ingrowth) VS 1 and 2 (ingrowth + recrut) VS only ingrowth) (at t2 only) because onlyt 2 at T1

# Q3
# regen under Himslef only of depends on the dominating species ? 
# Distributed under similar VS non similar species (already present or number of individual in the plots)


## This depedns on the country ?
## Spatially where it is ? all located at the same place or not ? 

# Q4 
# Graident of climate ? Can we associate these expansion , if any, with relative drought ? Or with climate ? ...With density ...


## Dominant species (mostly oak) but mostly conifeour in poland. To check ??? 
## Regen higher when dominant is the target species vs another species dominate

## Gradient of cliamte ? 

## should I retrieve the smaller trees in other inventories ???

# Chgeck if we have data such as cover (french and wallonia) or density scale (spain and germany)
# check this in the inventori data description of sophia check also the dominanat species !
#otherwise calculate which oine it is withthe most important basal area of the plot ! 

## Barplot of the dominance plot ! 
## Absolute number of dominants plot and also the proportion 

library(plyr)
library(ggplot2)
library(cowplot)
CODE <- "QUERUB"
Species <- "Quercus_rubra"
CODE <- "ROBPSE"
Species <- "Robinia_pseudacacia"
CONIF <- c("Abies_alba","Larix_decidua", 
           "Larix_kaempferi", "Larix_sp.","Picea_abies", 
           "Pinus_radiata", "Pinus_sylvestris","Pseudotsuga_menziesii",
           "Pinus_halepensis", "Pinus_nigra", "Pinus_pinaster", 
           "Pinus_pinea")
BLEAVED <- c("Acer_campestre", "Acer_platanoides", "Acer_pseudoplatanus", 
             "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", 
             "Alnus_sp.", "Betula_pendula", "Betula_sp.", "Carpinus_betulus", 
             "Castanea_sativa", "Fagus_sylvatica", "Fraxinus_excelsior","Other_broadleaved_sp.", 
             "Platanus_hispanica", "Populus_nigra", 
             "Prunus_avium", "Quercus_petraea", "Quercus_pyrenaica", 
             "Quercus_robur", "Quercus_rubra", "Quercus_sp.", "Robinia_pseudacacia", 
             "Salix_sp.", "Sorbus_torminalis", "Tilia_sp.",
             "Acacia_sp.","Celtis_australis", "Corylus_avellana","Eucalyptus_camaldulensis", 
             "Eucalyptus_globulus","Fraxinus_angustifolia", 
             "Laurus_nobilis","Populus_sp.","Quercus_ilex", 
             "Quercus_suber","Ulmus_minor")
BLEAVED <- BLEAVED[!BLEAVED%in%Species]
dfplot <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot2_",CODE,".rds")) ## Plotdatabase ! 
## think about the representation of barplots with the two cover beffore and after + the percentage and the dominant species 
df2 <- dfplot[,c("Domin.sp.T1",'DOMIN.s.T1',"Domin.sp.T2",'DOMIN.s.T2')]

df.T1 <- as.data.frame(table(as.character(df2$Domin.sp.T1)))
df.T2 <- as.data.frame(table(as.character(df2$Domin.sp.T2)))
df.T1$Var1 <- as.character(df.T1$Var1)
df.T2$Var1 <- as.character(df.T2$Var1)

df.T2.bis <- rbind(df.T2,df.T1[!df.T1$Var1%in%df.T2$Var1,]) ## Add the names that appears just in one survey (!!! This command also add the freq that goes with it)
df.T1.bis <- rbind(df.T1,df.T2[!df.T2$Var1%in%df.T1$Var1,]) # Check next lines to change the freq to zero 

df.T2.bis[df.T2.bis$Var1%in%df.T1[!df.T1$Var1%in%df.T2$Var1,"Var1"],"Freq"] <- 0 ## Put the value at the missing time to 0
df.T1.bis[df.T1.bis$Var1%in%df.T2[!df.T2$Var1%in%df.T1$Var1,"Var1"],"Freq"] <- 0
df.T1.bis <- arrange(df.T1.bis, Var1,desc(Freq)) 
df.T2.bis <- arrange(df.T2.bis, Var1,desc(Freq)) 

df.T2.bis$prop <- round(df.T2.bis$Freq/sum(df.T2.bis$Freq),2)
df.T1.bis$prop <- round(df.T1.bis$Freq/sum(df.T2.bis$Freq),2) # For the porportion at T1 => use the total number of plot where it is present (not the NA). We use the sum of plot at T2

df.T1.bis$Census <- 1
df.T2.bis$Census <- 2

df <- cbind(df.T1.bis,df.T2.bis)
df <- df[df[,2]>1|df[,6]>1,] ## keep only species with freq > 1 in ONE of the inventoy.(otherwise the bar in the barplot is larger) 
df <- rbind(df[,1:4],df[,5:8])

df_sorted <- arrange(df, Var1,desc(Census)) 
#df_cumsum <- ddply(df_sorted, "Var1",
#                     transform, label_ypos=cumsum(prop)-0.5*prop)
df_sorted$label_ypos <- df_sorted$prop-0.5*df_sorted$prop
p <- ggplot(data=df_sorted, aes(x=reorder(Var1,-prop), y=prop,fill=as.factor(Census))) +
  theme(panel.background = element_rect(fill="white", colour="black", size=1,
                                        linetype=1, color="black")) +
  theme(panel.grid.major = element_line(size=0, linetype=0,lineend="square", color="red"),
        panel.grid.minor = element_line(size=0, linetype=0,lineend="round", color="blue"))+
  geom_bar(stat="identity",position = position_dodge2(width = 0.85,preserve = c("total")),color="black",size=0.5)+
  #geom_bar(stat="identity",color="black",size=0.5)+
  geom_text(position = position_dodge2(width = 0.85,preserve = c("total")),aes(y=label_ypos, label=ifelse(prop<=0.01,NA,Freq)),color="black", vjust=0.15, size=3,fontface='bold')+
  labs(y=paste0("Frenquency"), x="Species",title=paste0('Regeneration of ',CODE," : Dominant species of the stand"))+
  scale_colour_manual(values = c("blue","red"),labels=c("1"="First census","2"="Second census")) +
  scale_fill_manual(name="Plot count",values = c("1"="steelblue","2"="indianred"),labels=c("1"="First census","2"="Second census"))+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.5), expand = c(0,0.01)) +
  #geom_text(aes(label = MARGIN,y=FREQsignif+0.03),colour="black",size=5,fontface = "bold")+
  theme(text = element_text(face="bold"),
        legend.direction ="vertical",
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(size=11,color="black",angle = 45,hjust = 1,vjust = 1),axis.text.y = element_text(size=18,color="black"),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        legend.key = element_rect(fill = "white", colour = "black",size = 1),
        legend.key.heigh = unit(3,"line"),
        legend.key.width = unit(1.5,"line"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        #legend.justification = "center",
        legend.margin = margin(0,0,0,0),
        #legend.background=element_rect(fill="white",colour="black",size=0.5,linetype="solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        axis.line = element_line(colour="black"),
        plot.title = element_text(size=17,hjust = 0,vjust = 0),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
        plot.caption = element_text(face="bold.italic"))
p
save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/Dominance.Barplot",CODE,".png"),plot = p,base_width = 10, base_height = 7, dpi = 500 ,units = "in",nrow=1,ncol=1)

## Do the same by regrouping the species Brodleaved VS coniferous, VS the target species 
## Maybe one with only the dominant species at T1 for plot where it became dominant at T2

df2 <- dfplot[,c("Domin.sp.T1",'DOMIN.s.T1',"Domin.sp.T2",'DOMIN.s.T2')]
df2$dominProg <- ifelse(df2$Domin.sp.T1%in%Species & df2$Domin.sp.T2%in%Species,3, #twice dominant 
                       ifelse(!df2$Domin.sp.T1%in%Species & df2$Domin.sp.T2%in%Species,2,
                              ifelse(!df2$Domin.sp.T1%in%Species & !df2$Domin.sp.T2%in%Species,0,1)))

df2 <- df2[df2$dominProg=="2",]
df.T1 <- as.data.frame(table(as.character(df2$Domin.sp.T1)))
df.T1$Var1 <- as.character(df.T1$Var1)
df.T1.bis <- arrange(df.T1, Var1,desc(Freq)) 
df.T1.bis$prop <- round(df.T1.bis$Freq/sum(df.T1.bis$Freq),2)
df <- df.T1.bis
df$label_ypos <- df$prop-0.5*df$prop
p <- ggplot(data=df, aes(x=reorder(Var1,-prop), y=prop)) +
  theme(panel.background = element_rect(fill="white", colour="black", size=1,
                                        linetype=1, color="black")) +
  theme(panel.grid.major = element_line(size=0, linetype=0,lineend="square", color="red"),
        panel.grid.minor = element_line(size=0, linetype=0,lineend="round", color="blue"))+
  #geom_bar(stat="identity",position = position_dodge2(width = 0.85,preserve = c("total")),color="black",size=0.5)+
  geom_bar(stat="identity",color="black",size=0.5,fill="steelblue")+
  geom_text(position = position_dodge2(width = 0.85,preserve = c("total")),aes(y=label_ypos, label=ifelse(prop<=0.01,NA,Freq)),color="black", vjust=0.15, size=3,fontface='bold')+
  labs(y=paste0("Frenquency"), x="Species",title=paste0('Dominant species at T1 where ',CODE," became dominant at T2"))+
  #scale_colour_manual(values = c("blue","red"),labels=c("1"="First census","2"="Second census")) +
  #scale_fill_manual(values = c("1"="steelblue","2"="indianred"),labels=c("1"="First census","2"="Second census"))+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.2), expand = c(0,0.01)) +
  #geom_text(aes(label = MARGIN,y=FREQsignif+0.03),colour="black",size=5,fontface = "bold")+
  theme(text = element_text(face="bold"),
        legend.direction ="vertical",
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(size=11,color="black",angle = 45,hjust = 1,vjust = 1),axis.text.y = element_text(size=18,color="black"),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        legend.key = element_rect(fill = "white", colour = "black",size = 1),
        legend.key.heigh = unit(3,"line"),
        legend.key.width = unit(1.5,"line"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        #legend.justification = "center",
        legend.margin = margin(0,0,0,0),
        #legend.background=element_rect(fill="white",colour="black",size=0.5,linetype="solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        axis.line = element_line(colour="black"),
        plot.title = element_text(size=17,hjust = 0,vjust = 0),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
        plot.caption = element_text(face="bold.italic"))
p
save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/Dominance.Barplot",CODE,"at.T2.png"),plot = p,base_width = 10, base_height = 7, dpi = 500 ,units = "in",nrow=1,ncol=1)

## Or maybe look at the dominance of the species in plot where it increases proportion between T1 and T2 !
dfplot$dominProg2 <- dfplot$DOMIN.s.T2
dfplot$dominProg2[!is.na(dfplot$DOMIN.s.T1)] <- dfplot$dominProg2[!is.na(dfplot$DOMIN.s.T1)]-dfplot$DOMIN.s.T1[!is.na(dfplot$DOMIN.s.T1)]
# here we calculate the progression index 
df2 <- dfplot[dfplot$dominProg2>=0,c("Domin.sp.T1",'DOMIN.s.T1',"Domin.sp.T2",'DOMIN.s.T2')] ## keep plot where increases QUERUB
df.T1 <- as.data.frame(table(as.character(df2$Domin.sp.T1)))
df.T2 <- as.data.frame(table(as.character(df2$Domin.sp.T2)))
df.T1$Var1 <- as.character(df.T1$Var1)
df.T2$Var1 <- as.character(df.T2$Var1)

df.T2.bis <- rbind(df.T2,df.T1[!df.T1$Var1%in%df.T2$Var1,]) ## Add the names that appears just in one survey (!!! This command also add the freq that goes with it)
df.T1.bis <- rbind(df.T1,df.T2[!df.T2$Var1%in%df.T1$Var1,]) # Check next lines to change the freq to zero 

df.T2.bis[df.T2.bis$Var1%in%df.T1[!df.T1$Var1%in%df.T2$Var1,"Var1"],"Freq"] <- 0 ## Put the value at the missing time to 0
df.T1.bis[df.T1.bis$Var1%in%df.T2[!df.T2$Var1%in%df.T1$Var1,"Var1"],"Freq"] <- 0
df.T1.bis <- arrange(df.T1.bis, Var1,desc(Freq)) 
df.T2.bis <- arrange(df.T2.bis, Var1,desc(Freq)) 

df.T2.bis$prop <- round(df.T2.bis$Freq/sum(df.T2.bis$Freq),2)
df.T1.bis$prop <- round(df.T1.bis$Freq/sum(df.T2.bis$Freq),2)

df.T1.bis$Census <- 1
df.T2.bis$Census <- 2

df <- cbind(df.T1.bis,df.T2.bis)
df <- df[df[,2]>1|df[,6]>1,] ## keep only species with freq > 1 in ONE of the inventoy.(otherwise the bar in the barplot is larger) 
df <- rbind(df[,1:4],df[,5:8])

df_sorted <- arrange(df, Var1,desc(Census)) 
#df_cumsum <- ddply(df_sorted, "Var1",
#                     transform, label_ypos=cumsum(prop)-0.5*prop)
df_sorted$label_ypos <- df_sorted$prop-0.5*df_sorted$prop
p <- ggplot(data=df_sorted, aes(x=reorder(Var1,-prop), y=prop,fill=as.factor(Census))) +
  theme(panel.background = element_rect(fill="white", colour="black", size=1,
                                        linetype=1, color="black")) +
  theme(panel.grid.major = element_line(size=0, linetype=0,lineend="square", color="red"),
        panel.grid.minor = element_line(size=0, linetype=0,lineend="round", color="blue"))+
  geom_bar(stat="identity",position = position_dodge2(width = 0.85,preserve = c("total")),color="black",size=0.5)+
  #geom_bar(stat="identity",color="black",size=0.5)+
  geom_text(position = position_dodge2(width = 0.85,preserve = c("total")),aes(y=label_ypos, label=ifelse(prop<=0.01,NA,Freq)),color="black", vjust=0.15, size=3,fontface='bold')+
  labs(y=paste0("Frenquency"), x="Species",title=paste0('Dominant species at T1 and T2 where ',CODE," cover increased"))+
  scale_colour_manual(values = c("blue","red"),labels=c("1"="First census","2"="Second census")) +
  scale_fill_manual(name=NULL,values = c("1"="steelblue","2"="indianred"),labels=c("1"="First census","2"="Second census"))+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.5), expand = c(0,0.01)) +
  #geom_text(aes(label = MARGIN,y=FREQsignif+0.03),colour="black",size=5,fontface = "bold")+
  theme(text = element_text(face="bold"),
        legend.direction ="vertical",
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(3, 3, 3, 3),
        axis.text.x = element_text(size=11,color="black",angle = 45,hjust = 1,vjust = 1),axis.text.y = element_text(size=18,color="black"),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        legend.key = element_rect(fill = "white", colour = "black",size = 1),
        legend.key.heigh = unit(3,"line"),
        legend.key.width = unit(1.5,"line"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        #legend.justification = "center",
        legend.margin = margin(0,0,0,0),
        #legend.background=element_rect(fill="white",colour="black",size=0.5,linetype="solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        axis.line = element_line(colour="black"),
        plot.title = element_text(size=17,hjust = 0,vjust = 0),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
        plot.caption = element_text(face="bold.italic"))
p
save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/Dominance.Increased.Barplot",CODE,".png"),plot = p,base_width = 10, base_height = 7, dpi = 500 ,units = "in",nrow=1,ncol=1)


### last barplot to show the dominance of species at T1 and T2 where we found the species. But group species broadlevade and conif 
df2 <- dfplot[,c("Domin.sp.T1",'DOMIN.s.T1',"Domin.sp.T2",'DOMIN.s.T2')]
df.T1 <- as.data.frame(table(as.character(df2$Domin.sp.T1)))
df.T2 <- as.data.frame(table(as.character(df2$Domin.sp.T2)))
df.T1$Var1 <- as.character(df.T1$Var1)
df.T2$Var1 <- as.character(df.T2$Var1)
df.T1.bis <- arrange(df.T1, Var1,desc(Freq)) 
df.T2.bis <- arrange(df.T2, Var1,desc(Freq)) 

df.T1.CONIF <- c("Coniferous",sum(df.T1.bis[df.T1.bis$Var1%in%CONIF,"Freq"]))
df.T1.BLEAVED <- c("Broadleaved",sum(df.T1.bis[df.T1.bis$Var1%in%BLEAVED,"Freq"]))
df.T1.Species <- df.T1.bis[df.T1.bis$Var1%in%Species,]
df.T1.bis <- rbind(df.T1.CONIF,df.T1.BLEAVED,df.T1.Species)
df.T1.bis$Freq <- as.numeric(df.T1.bis$Freq)

df.T2.CONIF <- c("Coniferous",sum(df.T2.bis[df.T2.bis$Var1%in%CONIF,"Freq"]))
df.T2.BLEAVED <- c("Broadleaved",sum(df.T2.bis[df.T2.bis$Var1%in%BLEAVED,"Freq"]))
df.T2.Species <- df.T2.bis[df.T2.bis$Var1%in%Species,]
df.T2.bis <- rbind(df.T2.CONIF,df.T2.BLEAVED,df.T2.Species)
df.T2.bis$Freq <- as.numeric(df.T2.bis$Freq)

df.T2.bis$prop <- round(df.T2.bis$Freq/sum(df.T2.bis$Freq),2)
df.T1.bis$prop <- round(df.T1.bis$Freq/sum(df.T2.bis$Freq),2) # For the porportion at T1 => use the total number of plot where it is present (not the NA). We use the sum of plot at T2

df.T1.bis$Census <- 1
df.T2.bis$Census <- 2

df <- rbind(df.T1.bis,df.T2.bis)

df_sorted <- arrange(df, desc(Var1)) 
df_sorted$label_ypos <- df_sorted$prop-0.5*df_sorted$prop
df_sorted$Var1 <- as.factor(df_sorted$Var1)
df_sorted$Var1 <- factor(df_sorted$Var1, levels = levels(df_sorted$Var1)[c(3,1,2)]) ## Reorder as we want !
p <- ggplot(data=df_sorted, aes(x=Var1, y=prop,fill=as.factor(Census))) +
  theme(panel.background = element_rect(fill="white", colour="black", size=1,
                                        linetype=1, color="black")) +
  theme(panel.grid.major = element_line(size=0, linetype=0,lineend="square", color="red"),
        panel.grid.minor = element_line(size=0, linetype=0,lineend="round", color="blue"))+
  geom_bar(stat="identity",position = position_dodge2(width = 0.85,preserve = c("total")),color="black",size=0.5)+
  #geom_bar(stat="identity",color="black",size=0.5)+
  geom_text(position = position_dodge2(width = 0.85,preserve = c("total")),aes(y=label_ypos, label=ifelse(prop<=0.01,NA,Freq)),color="black", vjust=0.15, size=3,fontface='bold')+
  labs(y=paste0("Frenquency"), x="Species",title=paste0('Regeneration of ',CODE," : Dominant species of the stand"))+
  scale_colour_manual(values = c("blue","red"),labels=c("1"="First census","2"="Second census")) +
  scale_fill_manual(name="Plot count",values = c("1"="steelblue","2"="indianred"),labels=c("1"="First census","2"="Second census"))+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.5), expand = c(0,0.01)) +
  #geom_text(aes(label = MARGIN,y=FREQsignif+0.03),colour="black",size=5,fontface = "bold")+
  theme(text = element_text(face="bold"),
        legend.direction ="vertical",
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(size=11,color="black",angle = 45,hjust = 1,vjust = 1),axis.text.y = element_text(size=18,color="black"),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        legend.key = element_rect(fill = "white", colour = "black",size = 1),
        legend.key.heigh = unit(3,"line"),
        legend.key.width = unit(1.5,"line"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        #legend.justification = "center",
        legend.margin = margin(0,0,0,0),
        #legend.background=element_rect(fill="white",colour="black",size=0.5,linetype="solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        axis.line = element_line(colour="black"),
        plot.title = element_text(size=17,hjust = 0,vjust = 0),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
        plot.caption = element_text(face="bold.italic"))
p
save_plot(filename = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/Redaction/paper3/Dominance.Group.Barplot",CODE,".png"),plot = p,base_width = 10, base_height = 7, dpi = 500 ,units = "in",nrow=1,ncol=1)


