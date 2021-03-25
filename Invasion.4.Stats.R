## test small model to identify to what vriable this (propension to expand) is sensitive to 
## N_ha for competition, total Ba.ha (compet), or climate, spei ?


## Cor test with competition, climatic variable, density 

## basic statistics 

nrow(dfQUERUB[dfQUERUB$ptreeNbrJ.R=="1",]) # Plot with 100% of new trees = new areas
nrow(dfQUERUB[dfQUERUB$sp.recruitment.plot.rate=="1",]) # IDEM but based on BA

nrow(dfQUERUB[dfQUERUB$ptreeNbrJ.R=="0",]) #prop with no regen
nrow(dfQUERUB[dfQUERUB$sp.recruitment.plot.rate=="0",]) #idem

nrow(dfQUERUB[dfQUERUB$ptreeNbrJ.R!="0"&dfQUERUB$ptreeNbrJ.R!="1",]) # prop plot with regen but already present
nrow(dfQUERUB[dfQUERUB$sp.recruitment.plot.rate!="0"&dfQUERUB$sp.recruitment.plot.rate!="1",])
hist(dfQUERUB$sp.recruitment.plot.rate)


nrow(dfROBPSE[dfROBPSE$ptreeNbrJ.R=="1",]) # Plot with 100% of new trees = new areas
nrow(dfROBPSE[dfROBPSE$sp.recruitment.plot.rate=="1",]) # IDEM but based on BA

nrow(dfROBPSE[dfROBPSE$ptreeNbrJ.R=="0",]) #prop with no regen
nrow(dfROBPSE[dfROBPSE$sp.recruitment.plot.rate=="0",]) #idem

nrow(dfROBPSE[dfROBPSE$ptreeNbrJ.R!="0"&dfROBPSE$ptreeNbrJ.R!="1",]) # prop plot with regen but already present
nrow(dfROBPSE[dfROBPSE$sp.recruitment.plot.rate!="0"&dfROBPSE$sp.recruitment.plot.rate!="1",])
hist(dfROBPSE$sp.recruitment.plot.rate)


## modéliser la cover progression or model the recruitment rate ?