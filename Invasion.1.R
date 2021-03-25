rm(list = ls())
gc()
Dir=c("~/SynologyDrive/FUNDIV - NFI - Europe/")
#load(paste0(Dir,"our-data/tfinal.biotic.Feb2019.RData")) #This is the database we want from which we want to analyse the metrics
tfinal <- readRDS(paste0(Dir,"our-data/tfinal.biotic.August2020.rds")) #no france 
tfinal[tfinal$treestatus_th==6,"treestatus_th"] <- 5  ### For PICABI and PINSYL some trees (42 and 142 were noted as 6 and are effectively dead which false the calculations later on)
## Start with splitting all plot 

QUERUB <- unique(tfinal[tfinal$code=="QUERUB","plotcode"])
Mydf <- tfinal[tfinal$plotcode%in%QUERUB,]
dat.fundiv <- split(Mydf,Mydf$plotcode)
library(parallel)
data <- do.call(rbind, lapply(dat.fundiv,function(DF){
  ## Add something for species Lvl !!! 
  i=DF[1,"plotcode"]
  dat=matrix(NA,nrow(Mydf[Mydf$plotcode==i,]),9)
  dat[,9]=c(1:nrow(Mydf))[Mydf$plotcode==i]
  if(nrow(DF)>0) {
    S=unique(DF$speciesid)
    S=S[!is.na(S)]
    if(length(S)>0)
      for(s in S) {
      df=DF[!is.na(DF$speciesid) & DF$speciesid==s,]
      DOMIN.s.T1=(df$BAj.plot.1/df$BA.ha.plot.1)
      DOMIN.s.T2=(df$BAj.plot/df$BA.plot.ha)
      #if(length(DOMIN.s)==0 | DOMIN.s==0) DOMIN.s=0 #rajout march 0 instead of na
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,7]=DOMIN.s.T1
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,8]=DOMIN.s.T2
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,1] <- as.numeric(nrow(df)) # Number of lines = total number of trees of the species
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,2] <- as.numeric(nrow(df[df$treestatus_th=="2" & !is.na(df$bachange_ha_yr.1),])) #ingrowth trees that have a values of growth (excluding some french trees without ir5 values for the calculation of mean BAI)
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,3] <- as.numeric(nrow(df[df$treestatus_th=="2" & !is.na(df$dbh1),])) #ingrowth trees that have a values of dbh (to include trees used to calculate the dbh sum of the plot). +6000 compare to the previous one
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,4] <- as.numeric(nrow(df[df$treestatus_th=="1",])) #recruited
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,5] <- as.numeric(nrow(df[df$treestatus_th%in%c("3","4","5")  & !is.na(df$dbh1),])) #dead trees with values of dbh (excluding some french trees for the caclulation of meandbh)
      dat[Mydf$speciesid[Mydf$plotcode==i]==s,6] <- as.numeric(nrow(df[df$treestatus_th=="4" & !is.na(df$dbh1),])) # Real dead (used in calculation of mortality rate)
    # The previous line should include only real dead trees (4) but in the calculation of the total dbh of the plot (script 10bis.meanDBH),
    # all trees with a value of dbh1 were included. (Except just recruited trees)
  }}
as.data.frame(dat)}))
treeNbrJ=as.numeric(data[order(data[,9]),1]) # total lines of the species
treeNbrJ.I.BAI=as.numeric(data[order(data[,9]),2]) #ingrowth with a value of bai
treeNbrJ.I.dbh=as.numeric(data[order(data[,9]),3]) #ingrowth with a value of dbh1
treeNbrJ.R=as.numeric(data[order(data[,9]),4]) # recruited
treeNbrJ.M=as.numeric(data[order(data[,9]),6]) # dead (three statuts)
treeNbrJ.Mall=as.numeric(data[order(data[,9]),5]) # dead (just the fourth statuts)
DOMIN.s.T1=as.numeric(data[order(data[,9]),7]) # dead (just the fourth statuts)
DOMIN.s.T2=as.numeric(data[order(data[,9]),8]) # dead (just the fourth statuts)

Mydf[,"treeNbrJ"]=treeNbrJ # number of lines = number of individuals (including the mistakes, see above)
Mydf[,"treeNbrJ.I.BAI"]=treeNbrJ.I.BAI # number of individuals ingrowth
Mydf[,"treeNbrJ.I.dbh"]=treeNbrJ.I.dbh # number of individuals ingrowth
Mydf[,"treeNbrJ.R"]=treeNbrJ.R # number of recruted
Mydf[,"treeNbrJ.M"]=treeNbrJ.M # number of real dead (statut 4)
Mydf[,"treeNbrJ.Mall"]=treeNbrJ.Mall # number of dead ( + statut 3 and statut 5 also). For check at the end  of the script
Mydf[,"treeNbrJ.IMall"]=treeNbrJ.Mall+treeNbrJ.I.dbh   ## Ingrowth + Dead and 3 and 5  = Trees accounted for in mortality rate and for dbhmean
Mydf[,"treeNbrJ.IR"]=treeNbrJ.R+treeNbrJ.I.BAI   ## Ingrowth + Recruted = Trees accounted for in rectuitment rate
Mydf[,"DOMIN.s.T1"]=DOMIN.s.T1 # number of dead ( + statut 3 and statut 5 also). For check at the end  of the script
Mydf[,"DOMIN.s.T2"]=DOMIN.s.T2 # number of dead ( + statut 3 and statut 5 also). For check at the end  of the script

QUERUB2 <- split(Mydf,Mydf$plotcode) # Resplit the table 
QUERUB2 <- lapply(QUERUB2, function(x){
  x[,"MaxDomin.T1"] <- max(x[,"DOMIN.s.T1"]) # What is the porportion of the dominant species at T1
  x[,"Domin.sp.T1"] <- x[x[,"DOMIN.s.T1"]==max(x[,"DOMIN.s.T1"]),"binomial"][1] #What is the dominant species at T1
  x[,"MaxDomin.T2"] <- max(x[,"DOMIN.s.T2"]) 
  x[,"Domin.sp.T2"] <- x[x[,"DOMIN.s.T2"]==max(x[,"DOMIN.s.T2"]),"binomial"][1]
  x[,"richness.T1"] <- length(unique(x[!is.na(x[,"dbh1"]),"code"])) # nbr of species in the plot at t1
  x[,"richness.T2"] <- length(unique(x[!is.na(x[,"dbh2"]),"code"])) # nbr of species in the plot at t2
  x[,"sp.recruitment.plot.rate.yr"] <-x[,"sp.recruitment.plot.rate"] / x[,"yearsbetweensurveys"]
  ;x[x[,"code"]=="QUERUB",]})

QUERUB2 <- do.call(rbind,QUERUB2)

## add climate and spei and  save it ! 
CODE <- "QUERUB"
Dir = c(paste0(Dir, "our-data/species/", CODE, "/"))
setwd(Dir)
saveRDS(QUERUB2,paste0("Mydf_",CODE,".rds")) ## Treedatabase ! 

## Add the climate and save the rds for both species 
############################################################
####    Extraction de la base de donnée climatique     #####
############################################################
Dir <- c("~/SynologyDrive/FUNDIV - NFI - Europe/")
setwd(Dir)
source(paste0(Dir,"Myscripts/Fundiv.project/6.climateSpecies.R"))
Allcode <- c("QUERUB")
for (code in Allcode){
  try(ExtractClimate(CODE=code,yearINI=yearI,yearFIN=yearF,Intervalle=Inter,files.wanted=files.w,save=T),silent=T) # Maybe not all the intervalle
}
Mydf <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,"_allVariable.rds"))
Mydf1 <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,".rds"))
# the new database has the climatic variable ! 
dfQUERUB <-Mydf[!duplicated(Mydf$plotcode), -c(4,9:13)] # Keep unique plot codes
dfQUERUB$ptreeNbrJ.R <- dfQUERUB$treeNbrJ.R/dfQUERUB$treeNbrJ # proportion of recruit on total nbr of trees
dfQUERUB$ptreeNbrJ.I.dbh <- dfQUERUB$treeNbrJ.I.dbh/dfQUERUB$treeNbrJ # proportion of ingrowth trees on the total of the plot 
saveRDS(dfQUERUB,paste0("dfplot_",CODE,".rds")) ## Plotdatabase ! 


# need to add spei 


## ROBPSE
ROBPSE <- unique(tfinal[tfinal$code=="ROBPSE","plotcode"])
Mydf <- tfinal[tfinal$plotcode%in%ROBPSE,]
dat.fundiv <- split(Mydf,Mydf$plotcode)
data <- do.call(rbind, mclapply(dat.fundiv,function(DF){
  ## Add something for species Lvl !!! 
  i=DF[1,"plotcode"]
  dat=matrix(NA,nrow(Mydf[Mydf$plotcode==i,]),9)
  dat[,9]=c(1:nrow(Mydf))[Mydf$plotcode==i]
  if(nrow(DF)>0) {
    S=unique(DF$speciesid)
    S=S[!is.na(S)]
    if(length(S)>0)
      for(s in S) {
        df=DF[!is.na(DF$speciesid) & DF$speciesid==s,]
        DOMIN.s.T1=(df$BAj.plot.1/df$BA.ha.plot.1)
        DOMIN.s.T2=(df$BAj.plot/df$BA.plot.ha)
        #if(length(DOMIN.s)==0 | DOMIN.s==0) DOMIN.s=0 #rajout march 0 instead of na
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,7]=DOMIN.s.T1
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,8]=DOMIN.s.T2
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,1] <- as.numeric(nrow(df)) # Number of lines = total number of trees of the species
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,2] <- as.numeric(nrow(df[df$treestatus_th=="2" & !is.na(df$bachange_ha_yr.1),])) #ingrowth trees that have a values of growth (excluding some french trees without ir5 values for the calculation of mean BAI)
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,3] <- as.numeric(nrow(df[df$treestatus_th=="2" & !is.na(df$dbh1),])) #ingrowth trees that have a values of dbh (to include trees used to calculate the dbh sum of the plot). +6000 compare to the previous one
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,4] <- as.numeric(nrow(df[df$treestatus_th=="1",])) #recruited
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,5] <- as.numeric(nrow(df[df$treestatus_th%in%c("3","4","5")  & !is.na(df$dbh1),])) #dead trees with values of dbh (excluding some french trees for the caclulation of meandbh)
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,6] <- as.numeric(nrow(df[df$treestatus_th=="4" & !is.na(df$dbh1),])) # Real dead (used in calculation of mortality rate)
        # The previous line should include only real dead trees (4) but in the calculation of the total dbh of the plot (script 10bis.meanDBH),
        # all trees with a value of dbh1 were included. (Except just recruited trees)
      }}
  as.data.frame(dat)},mc.cores=1,mc.silent=T))
treeNbrJ=as.numeric(data[order(data[,9]),1]) # total lines of the species
treeNbrJ.I.BAI=as.numeric(data[order(data[,9]),2]) #ingrowth with a value of bai
treeNbrJ.I.dbh=as.numeric(data[order(data[,9]),3]) #ingrowth with a value of dbh1
treeNbrJ.R=as.numeric(data[order(data[,9]),4]) # recruited
treeNbrJ.M=as.numeric(data[order(data[,9]),6]) # dead (three statuts)
treeNbrJ.Mall=as.numeric(data[order(data[,9]),5]) # dead (just the fourth statuts)
DOMIN.s.T1=as.numeric(data[order(data[,9]),7]) # dead (just the fourth statuts)
DOMIN.s.T2=as.numeric(data[order(data[,9]),8]) # dead (just the fourth statuts)

Mydf[,"treeNbrJ"]=treeNbrJ # number of lines = number of individuals (including the mistakes, see above)
Mydf[,"treeNbrJ.I.BAI"]=treeNbrJ.I.BAI # number of individuals ingrowth
Mydf[,"treeNbrJ.I.dbh"]=treeNbrJ.I.dbh # number of individuals ingrowth
Mydf[,"treeNbrJ.R"]=treeNbrJ.R # number of recruted
Mydf[,"treeNbrJ.M"]=treeNbrJ.M # number of real dead (statut 4)
Mydf[,"treeNbrJ.Mall"]=treeNbrJ.Mall # number of dead ( + statut 3 and statut 5 also). For check at the end  of the script
Mydf[,"treeNbrJ.IMall"]=treeNbrJ.Mall+treeNbrJ.I.dbh   ## Ingrowth + Dead and 3 and 5  = Trees accounted for in mortality rate and for dbhmean
Mydf[,"treeNbrJ.IR"]=treeNbrJ.R+treeNbrJ.I.BAI   ## Ingrowth + Recruted = Trees accounted for in rectuitment rate
Mydf[,"DOMIN.s.T1"]=DOMIN.s.T1 
Mydf[,"DOMIN.s.T2"]=DOMIN.s.T2 

ROBPSE2 <- split(Mydf,Mydf$plotcode) # Resplit the table 
ROBPSE2 <- lapply(ROBPSE2, function(x){
  x[,"MaxDomin.T1"] <- max(x[,"DOMIN.s.T1"]) # What is the porportion of the dominant species at T1
  x[,"Domin.sp.T1"] <- x[x[,"DOMIN.s.T1"]==max(x[,"DOMIN.s.T1"]),"binomial"][1] #What is the dominant species at T1
  x[,"MaxDomin.T2"] <- max(x[,"DOMIN.s.T2"]) 
  x[,"Domin.sp.T2"] <- x[x[,"DOMIN.s.T2"]==max(x[,"DOMIN.s.T2"]),"binomial"][1]
  x[,"richness.T1"] <- length(unique(x[!is.na(x[,"dbh1"]),"code"])) # nbr of species in the plot at t1
  x[,"richness.T2"] <- length(unique(x[!is.na(x[,"dbh2"]),"code"])) # nbr of species in the plot at t2
  x[,"sp.recruitment.plot.rate.yr"] <-x[,"sp.recruitment.plot.rate"] / x[,"yearsbetweensurveys"]
  ;x[x[,"code"]=="ROBPSE",]})

ROBPSE2 <- do.call(rbind,ROBPSE2)
## add climate and spei and  save it ! 
CODE <- "ROBPSE"
Dir = c(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/", CODE, "/"))
setwd(Dir)
saveRDS(ROBPSE2,paste0("Mydf_",CODE,".rds")) ## Treedatabase ! 

## Add the climate and save the rds for both species 
############################################################
####    Extraction de la base de donnée climatique     #####
############################################################
Dir <- c("~/SynologyDrive/FUNDIV - NFI - Europe/")
setwd(Dir)
source(paste0(Dir,"Myscripts/Fundiv.project/6.climateSpecies.R"))
Allcode <- c("ROBPSE")
for (code in Allcode){
  try(ExtractClimate(CODE=code,yearINI=yearI,yearFIN=yearF,Intervalle=Inter,files.wanted=files.w,save=T),silent=T) # Maybe not all the intervalle
}
Mydf <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,"_allVariable.rds"))
Mydf1 <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,".rds"))
# the new database has the climatic variable ! 
dfROBPSE <-Mydf[!duplicated(Mydf$plotcode), -c(4,9:13)] # Keep unique plot codes
dfROBPSE$ptreeNbrJ.R <- dfROBPSE$treeNbrJ.R/dfROBPSE$treeNbrJ # proportion of recruit on total nbr of trees
dfROBPSE$ptreeNbrJ.I.dbh <- dfROBPSE$treeNbrJ.I.dbh/dfROBPSE$treeNbrJ # proportion of ingrowth trees on the total of the plot 
saveRDS(dfROBPSE,paste0("dfplot_",CODE,".rds")) ## Plotdatabase ! 


Allcode <- c("ROBPSE","QUERUB")
for (CODE in Allcode){
dfplot <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot_",CODE,".rds")) ## Plotdatabase ! 
### add SPEI for both species 
Years <- c("spei01","spei03","spei06","spei12","spei18","spei24","spei36","spei48")
for (i in Years){
  load(file = paste0("/home/achangenet/Documents/FUNDIV - NFI - Europe/our-data/climate/SPEI/plots_",i,"_1981_2015.RData"))
  dfplot[,paste0("mean_",i)] <- spei_plots_all$mean_spei_survey_years[match(dfplot$plotcode, spei_plots_all$plotcode,incomparables = NA)]
}
saveRDS(get("dfplot"), paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot2_",CODE,".rds")) # Work at the plot scale NO SCALE
}
dfplot <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot2_",CODE,".rds")) ## Plotdatabase ! 



