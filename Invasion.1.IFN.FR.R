rm(list = ls())
gc()
Dir=c("~/SynologyDrive/FUNDIV - NFI - Europe/")
load(paste0(Dir,"our-data/tfinal.biotic.Feb2019.RData"))
tfinal <- tfinal.biotic 
## Start with splitting all plot 

QUERUB <- unique(tfinal[tfinal$code=="QUERUB" & tfinal$country=="FR","plotcode"])
Mydf <- tfinal[tfinal$plotcode%in%QUERUB & !is.na(tfinal$speciesid),] #remov unkown species
dat.fundiv <- split(Mydf,Mydf$plotcode)
data <- do.call(rbind, lapply(dat.fundiv,function(DF){
  ## Add something for species Lvl !!! 
  i=DF[1,"plotcode"]
  dat=matrix(NA,nrow(Mydf[Mydf$plotcode==i,]),5)
  dat[,5]=c(1:nrow(Mydf))[Mydf$plotcode==i]
  if(nrow(DF)>0) {
    S=unique(DF$speciesid)
    S=S[!is.na(S)]
    if(length(S)>0)
      for(s in S) {
        df=DF[!is.na(DF$speciesid) & DF$speciesid==s,]
        DOMIN.s.T1=(df$BAj.plot.1/df$BA.ha.plot.1)
        #DOMIN.s.T1[is.na(DOMIN.s.T1)] <- 0
        #DOMIN.s.T2=(df$BAj.plot/df$BA.plot.ha) # Pas de T2 ici. We checked and they are identical (both variable)
        #if(length(DOMIN.s.T1)==0 | DOMIN.s.T1==0) DOMIN.s.T1=0 #rajout march 0 instead of na
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,1] <- as.numeric(nrow(df)) # Number of lines = total number of trees of the species
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,2] <- as.numeric(nrow(df[df$treestatus_th=="1",])) # Alive
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,3] <- as.numeric(nrow(df[df$treestatus_th=="0",])) # Dead
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,4]=DOMIN.s.T1
        # The previous line should include only real dead trees (4) but in the calculation of the total dbh of the plot (script 10bis.meanDBH),
        # all trees with a value of dbh1 were included. (Except just recruited trees)
      }}
  as.data.frame(dat)}))
treeNbrJ=as.numeric(data[order(data[,5]),1]) # total lines of the species
treeNbrJ.A=as.numeric(data[order(data[,5]),2]) # Alive
treeNbrJ.M=as.numeric(data[order(data[,5]),3]) # dead (three statuts)
DOMIN.s.T1=as.numeric(data[order(data[,5]),4]) # pourcentage of dominance

Mydf[,"treeNbrJ"]=treeNbrJ # number of lines = number of individuals (including the mistakes, see above)
Mydf[,"treeNbrJ.A"]=treeNbrJ.A # number of recruted
Mydf[,"treeNbrJ.M"]=treeNbrJ.M # number of real dead (statut 4)
Mydf[,"DOMIN.s.T1"]=DOMIN.s.T1 # number of dead ( + statut 3 and statut 5 also). For check at the end  of the script

QUERUB2 <- split(Mydf,Mydf$plotcode) # Resplit the table 
QUERUB2 <- lapply(QUERUB2, function(x){
  x[,"MaxDomin.T1"] <- max(x[,"DOMIN.s.T1"]) # What is the porportion of the dominant species at T1
  x[,"Domin.sp.T1"] <- x[x[,"DOMIN.s.T1"]==max(x[,"DOMIN.s.T1"]),"binomial"][1] #What is the dominant species at T1
  x[,"richness.T1"] <- length(unique(x[!is.na(x[,"dbh1"]),"code"])) # nbr of species in the plot at t1
  ;x[x[,"code"]=="QUERUB",]})

QUERUB2 <- do.call(rbind,QUERUB2)

## add climate and spei and  save it ! 
CODE <- "QUERUB"
Dir = c(paste0(Dir, "our-data/species/", CODE, "/"))
setwd(Dir)
saveRDS(QUERUB2,paste0("Mydf_FR_",CODE,".rds")) ## Treedatabase ! 

## Add the climate and save the rds for both species 
############################################################
####    Extraction de la base de donnée climatique     #####
############################################################
Dir <- c("~/SynologyDrive/FUNDIV - NFI - Europe/")
setwd(Dir)
source(paste0(Dir,"Myscripts/Fundiv.project/6.climateSpecies.R"))
Allcode <- c("QUERUB")
for (code in Allcode){
  try(ExtractClimate(CODE=code,yearINI=yearI,yearFIN=yearF,Intervalle=Inter,files.wanted=files.w,save=T),silent=F) # Maybe not all the intervalle
}
Mydf <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,"_allVariable.rds"))
Mydf1 <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,".rds"))
# the new database has the climatic variable ! 
dfQUERUB <-Mydf[!duplicated(Mydf$plotcode), -c(4)] # Keep unique plot codes
dfQUERUB$ptreeNbrJ.A <- dfQUERUB$treeNbrJ.A/dfQUERUB$treeNbrJ # proportion of alive on total nbr of trees
saveRDS(dfQUERUB,paste0("dfplot_FR_",CODE,".rds")) ## Plotdatabase ! 


# need to add spei 


## ROBPSE
ROBPSE <- unique(tfinal[tfinal$code=="ROBPSE" & tfinal$country=="FR","plotcode"])
Mydf <- tfinal[tfinal$plotcode%in%ROBPSE & !is.na(tfinal$speciesid),] #remov unkown species
dat.fundiv <- split(Mydf,Mydf$plotcode)
data <- do.call(rbind, lapply(dat.fundiv,function(DF){
  ## Add something for species Lvl !!! 
  i=DF[1,"plotcode"]
  dat=matrix(NA,nrow(Mydf[Mydf$plotcode==i,]),5)
  dat[,5]=c(1:nrow(Mydf))[Mydf$plotcode==i]
  if(nrow(DF)>0) {
    S=unique(DF$speciesid)
    S=S[!is.na(S)]
    if(length(S)>0)
      for(s in S) {
        df=DF[!is.na(DF$speciesid) & DF$speciesid==s,]
        DOMIN.s.T1=(df$BAj.plot.1/df$BA.ha.plot.1)
        #DOMIN.s.T1[is.na(DOMIN.s.T1)] <- 0
        #DOMIN.s.T2=(df$BAj.plot/df$BA.plot.ha) # Pas de T2 ici. We checked and they are identical (both variable)
        #if(length(DOMIN.s.T1)==0 | DOMIN.s.T1==0) DOMIN.s.T1=0 #rajout march 0 instead of na
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,1] <- as.numeric(nrow(df)) # Number of lines = total number of trees of the species
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,2] <- as.numeric(nrow(df[df$treestatus_th=="1",])) # Alive
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,3] <- as.numeric(nrow(df[df$treestatus_th=="0",])) # Dead
        dat[Mydf$speciesid[Mydf$plotcode==i]==s,4]=DOMIN.s.T1
        # The previous line should include only real dead trees (4) but in the calculation of the total dbh of the plot (script 10bis.meanDBH),
        # all trees with a value of dbh1 were included. (Except just recruited trees)
      }}
  as.data.frame(dat)}))
treeNbrJ=as.numeric(data[order(data[,5]),1]) # total lines of the species
treeNbrJ.A=as.numeric(data[order(data[,5]),2]) # Alive
treeNbrJ.M=as.numeric(data[order(data[,5]),3]) # dead (three statuts)
DOMIN.s.T1=as.numeric(data[order(data[,5]),4]) # pourcentage of dominance

Mydf[,"treeNbrJ"]=treeNbrJ # number of lines = number of individuals (including the mistakes, see above)
Mydf[,"treeNbrJ.A"]=treeNbrJ.A # number of recruted
Mydf[,"treeNbrJ.M"]=treeNbrJ.M # number of real dead (statut 4)
Mydf[,"DOMIN.s.T1"]=DOMIN.s.T1 # number of dead ( + statut 3 and statut 5 also). For check at the end  of the script

ROBPSE2 <- split(Mydf,Mydf$plotcode) # Resplit the table 
ROBPSE2 <- lapply(ROBPSE2, function(x){
  x[,"MaxDomin.T1"] <- max(x[,"DOMIN.s.T1"]) # What is the porportion of the dominant species at T1
  x[,"Domin.sp.T1"] <- x[x[,"DOMIN.s.T1"]==max(x[,"DOMIN.s.T1"]),"binomial"][1] #What is the dominant species at T1
  x[,"richness.T1"] <- length(unique(x[!is.na(x[,"dbh1"]),"code"])) # nbr of species in the plot at t1
  ;x[x[,"code"]=="ROBPSE",]})

ROBPSE2 <- do.call(rbind,ROBPSE2)

## add climate and spei and  save it ! 
CODE <- "ROBPSE"
Dir = c(paste0(Dir, "our-data/species/", CODE, "/"))
setwd(Dir)
saveRDS(ROBPSE2,paste0("Mydf_FR_",CODE,".rds")) ## Treedatabase ! 

## Add the climate and save the rds for both species 
############################################################
####    Extraction de la base de donnée climatique     #####
############################################################
Dir <- c("~/SynologyDrive/FUNDIV - NFI - Europe/")
setwd(Dir)
source(paste0(Dir,"Myscripts/Fundiv.project/6.climateSpecies.R"))
Allcode <- c("ROBPSE")
for (code in Allcode){
  try(ExtractClimate(CODE=code,yearINI=yearI,yearFIN=yearF,Intervalle=Inter,files.wanted=files.w,save=T),silent=F) # Maybe not all the intervalle
}
Mydf <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,"_allVariable.rds"))
Mydf1 <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/","Mydf_",CODE,".rds"))
# the new database has the climatic variable ! 
dfROBPSE <-Mydf[!duplicated(Mydf$plotcode), -c(4)] # Keep unique plot codes
dfROBPSE$ptreeNbrJ.A <- dfROBPSE$treeNbrJ.A/dfROBPSE$treeNbrJ # proportion of alive on total nbr of trees
saveRDS(dfROBPSE,paste0("dfplot_FR_",CODE,".rds")) ## Plotdatabase ! 


Allcode <- c("ROBPSE","QUERUB")
for (CODE in Allcode){
  dfplot <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot_FR_",CODE,".rds")) ## Plotdatabase ! 
  ### add SPEI for both species 
  Years <- c("spei01","spei03","spei06","spei12","spei18","spei24","spei36","spei48")
  for (i in Years){
    load(file = paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/climate/SPEI/plots_",i,"_1981_2015.RData"))
    dfplot[,paste0("mean_",i)] <- spei_plots_all$mean_spei_survey_years[match(dfplot$plotcode, spei_plots_all$plotcode,incomparables = NA)]
  }
  saveRDS(get("dfplot"), paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot2_FR_",CODE,".rds")) # Work at the plot scale NO SCALE
}
dfplot <- readRDS(paste0("~/SynologyDrive/FUNDIV - NFI - Europe/our-data/species/",CODE,"/dfplot2_FR_",CODE,".rds")) ## Plotdatabase ! 



