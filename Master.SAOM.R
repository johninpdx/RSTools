# SETUP =======================================================================

setwd ("T:/Data&Analysis/Output/Bethel/SienaBook")
getwd()

load("T:/Data&Analysis/Data/Bethel/BethelW1345v2.RData")
save.image("T:/Data&Analysis/Data/Bethel/BethelW1345v2.RData")

# Put in correct path to this set of functions
source('T:/Data&Analysis/Scripts/NetProj/PInfDataFunctions.r')

library(sna)
#library(gdata)
library(network)
library(RODBC)
library(RSienaTest)
library(Matrix)
library(dplyr)
library(data.table)

# STEP 1: create networks =====================================================
# Network set with no structural zeros
netSetSP <- getNetworkSet(pWavVec = c(1,3,4,5),
                          pSchVec = c(3,4,5,6,30),
                          pElig = 1,
                          pDid = 1,
                          pTyp = "BF",
                          pOut = "SP",
                          pS0 = "")
# Network set with structural 0's among multiple middle schools in wave 1
netSetSPS0 <- getNetworkSet(pWavVec = c(1,3,4,5),
                             pSchVec = c(3,4,5,6,30),
                             pElig = 1,
                             pDid = 1,
                             pTyp = "BF",
                             pOut = "SP",
                             pS0 = "S0")

# A set of 'network' class networks (these are much larger than the dgTSparse
#   matrix objects, by a factor of just over 50). These could be created and
#   used for descriptive purposes.
netSetNT <- getNetworkSet(pWavVec = c(1,3,4,5),
                          pSchVec = c(3,4,5,6,30),
                          pElig = 1,
                          pDid = 1,
                          pTyp = "BF",
                          pOut = "NT")

# STEP 2: create some useful ancillary objects================================

# A vector of all SIDs who were survey eligible in any of the waves
#   in pWavVec (useful for creating the next two tables). The SIDs from
#   the last element of the output of 'getNetworkSet' will usually
#   be a subset of these (because this set ignores whether the participant
#   actually did any surveys)
netSetSID.elig <- getEligNodes(pWavVec = c(1,3,4,5), pSchVec = c(3,4,5,6,30))

# An <ever elig> x <nWaves> DF of survey eligibility (0/1) x wave
eligXWave  <- getEligXWave(c(1,3,4,5), c(3,4,5,6,30))

# an <ever elig> x <nWaves> DF of suvery 'completion' (0/1) x wave
# ('completion' as defined in function 'allNQNotNA')
diditXWave <- getDiditXWave(pWavVec = c(1,3,4,5), pSchVec = c(3,4,5,6,30))

# Here are the SIDs finally included in the NetSet (accounts for
# survey completion requirements too). (Could have been obtained by running
#    'diditXWave' and selecting rows on the basis of which waves were
#    completed, or how many, etc.)
netSetSID.did1 <- netSetSP[[5]]

# STEP 3: Vars & CompChg ======================================================

# Create a 'sienaCompositionChange'-compatible 'changelist'
# Comp Change ----
#     Uses the vector of SIDs for which there was some data (>= 1 survey)
# You might use a more stringent criterion for inclusionxxxx in an analysis, but
# (obviously) never a less stringent one!
# _______________________________
# For w1,3,4,5 -- n=412 (must select for SIDs in the Analysis Set)
ccVec     <- makeCCVec(eligXWave[eligXWave$SID %in% netSetSID.did1,])

# _____________________________
#Create a crosswalk of SIDs and RowIDs, sorted by SID (so RowIDs apply
# to ordinally-corresponding SIDs)
# SAOM and 'network' objects both use this 'order number' as a way to
# identify network nodes; however, selecting and labeling (in output)
# require original SIDs. This dataframe can be used to translate.
# _____________________________
sidRowID <- data.frame(netSetSID.did1,seq(1,length(netSetSID.did1))) #n=412
names(sidRowID)<-c("SID","RID")

# Fixed Covariates   ----
FxCovBSD <- getFixedCovs(c(3,4,5,6,30))
# ________________________________
FxCovBSD.did1 <- FxCovBSD[FxCovBSD$SID %in% netSetSID.did1,]

# * Gender (Fxd Cov) ----
Gndr <- FxCovBSD.did1$Gender    #1=M, 0=F

# TVC/BDV's  ----
TVCovBSDz <- getNQTVCs(c(1,3,4,5),c(3,4,5,6,30)) #BSD = Bethel School Dist.
  length(unique(TVCovBSD$SID)) #count how many individuals

# Selects only those who did at least 1 survey in the 5 waves. Some of the above
# are probably 'empty' surveys (kid did not answer anything, or 'opted out')
TVCovBSD.did1 <- TVCovBSD[SID %in% did1] # (note data.table syntax)
tvc <- TVCovBSD.did1 #convenient alias for TVCov
#________________________
# * Antisocial Behavior    ----
#________________________
# w1,3,4,5
ABTbl <- makeTVTbl(pTVTbl = TVCovBSD.did1,
                  pSIDs  = netSetSP[[5]],
                  pVar = "AB",
                  pCut = c(0,.5,1,1.5,20),
                  pMaxNA = 2
                  )
#________________________
#  * E-cigarette/vape      ----
#________________________
ETTbl <- makeTVTbl(pTVTbl = TVCovBSD.did1,
                   pSIDs  = netSetSP[[5]],
                   pVar = "E3",
                   pCut = c(0,.1,5,30),
                   pMaxNA = 0
                   )

#________________________
# * Alc use                ----
#________________________
ACTbl <- makeTVTbl(pTVTbl = TVCovBSD.did1,
                   pSIDs  = netSetSP[[5]],
                   pVar = "A3",
                   pCut = c(0,.1,5,30),
                   pMaxNA = 0
                   )

#_________________________
# * Alc Life (onset) ----
#_________________________
# W 1,3,4,5 (based on construction of TVCovBSD.did1)
# Table of lifetime use x wave
ALTbl <- makeTVTbl(pTVTbl = TVCovBSD.did1,
                   pSIDs  = netSetSP[[5]],
                   pVar = "AL",
                   pCut = c(0,.1,5,30),
                   pMaxNA = 0
                   )
# Up-only indicator variable created from ALTbl
AOTbl <- createOnset(ALTbl)

# _____________________________________________________________________________
# STEP 4: Create RSiena Objs ----
# _____________________________________________________________________________
# Comp Change
 ccg <-sienaCompositionChange(ccVec)

# Fixed covs
# __________
sex <- coCovar(Gndr) #0 = F, 1 = M

# * Dependent Behviors ----
# ___________
# Alc Onset
al1345v <- sienaNet(as.matrix(AOTbl[,c(2:5)]),type = "behavior")

# * Networks ----
# ___________
# w1,3,4,5 -- includes structural zeros, W1
netBSD1345.OBF <- makeSAOMNet(netSetSPS0)

#'net' is used as the network in all single-network code below
net <- netBSD1345.OBF
# _____________________________________________________________________________
# STEP 5: SAOM analysis obj & models ----
# _____________________________________________________________________________
# DATA objects [set 'net' to correct var for # of waves first!]
dataObAL1345 <- sienaDataCreate(net,al1345,sex,ccg)

# As with 'net', use the same name in modesl for any data object:
dataOb <- dataObAL1345

# * Effects Obj ----
modEff <- getEffects(dataOb)
# In the model now:
  print(modEff, expand=TRUE)
  fix(modEff)

# * Get Report ----
print01Report(dataOb, modelname = 'BSD4wvALRpt')

# Define algorithm: MoM
alMod <- sienaAlgorithmCreate(useStdInits = T, projname = "BSD1345ALOutput.out")
#alMod <- sienaAlgorithmCreate(useStdInits = F, projname = "BSDw34AL_Output.2")
#abMLMod <- sienaAlgorithmCreate(useStdInits = F, projname = "Sch1_103wvOutput",
#                                n3=1200,nsub=4,maxlike=T,mult=40)

# _____________________________________________________________________________
# * Effects: add/remove ----
# _____________________________________________________________________________
#   Code for adding/removing effects [This type of stuff may or may not
#   belong in this script...but I tend to find it useful to leave these
#   commands here to jog my memory, make it easier to include or exclude
#   an effect, and so forth.]

# Network function Fx
modEff<-includeEffects(modEff,density,recip,include=T)
modEff<-includeEffects(modEff,transTrip1,cycle3,include=T)
  modEff<-includeEffects(modEff,transTrip1,include=T)
  modEff<-includeEffects(modEff,cycle3,include=F)

modEff<-includeEffects(modEff,inPopSqrt,include=F)
modEff<-includeEffects(modEff,sameX,interaction1 = "sex13",include=T)
  modEff <- includeEffects(modEff,altX,egoX,interaction1 = "sex",include = F)
modEff<-includeEffects(modEff,X, interaction1="sameEth", include=F)
modEff<-includeEffects(modEff,X, interaction1="sameSch13", include=T)
modEff <- includeEffects(modEff,altX,egoX,sameX,name="net",interaction1="al1fx", include=T)
modEff <- includeEffects(modEff,altX,egoX,sameX,name="net",interaction1="al345.TVC", include=T)
modEff <- includeEffects(modEff,altX,simX,name="net",interaction1="ac13", include=T)
  modEff <- includeEffects(modEff,egoX,name="net",interaction1="ac34", include=F)
modEff <- includeEffects(modEff,altX,name="net",interaction1="al345.TVC", include=F)
  modEff <- includeEffects(modEff,altX,name="net",interaction1="al13", include = F)
# Network function interactions
modEff <- includeInteraction(modEff,egoX,simX,interaction1=c("sex13","ac13")) #selec x sex?
# __________________________
# Behavior function Fx
modEff <- includeEffects(modEff,indeg,name="ac",interaction1="net",include=F)  # **
  modEff <- includeEffects(modEff,outdeg,name="ac",interaction1="net",include=F)
modEff <- includeEffects(modEff,avSim,name="ac34",interaction1="net",include=T)
modEff <- includeEffects(modEff,avSimRecip,name="ac34",interaction1="net",include=F)
modEff <- includeEffects(modEff,totSimRecip,name="ac13",interaction1="net",include=F)
modEff <- includeEffects(modEff,totSim,name="ac13",interaction1="net",include=F)
modEff <- includeEffects(modEff,avRecAlt,name="ac13",interaction1="net",include=F)
modEff <- includeEffects(modEff,maxAlt,name="ac",interaction1="net",include=F)
modEff <- includeEffects(modEff,minAlt,name="ac",interaction1="net",include=F)
modEff <- includeEffects(modEff,totSimPopAlt,name="ac13",interaction1="net",include=T)
modEff <- includeEffects(modEff,totAltDist2,name="ac13",interaction1="net",include=F)
modEff <- includeEffects(modEff,isolate,name="ac13",interaction1="net",include=F)
# Behavior Covariate effects
modEff <- includeEffects(modEff,effFrom,name="ac13",interaction1="sex13",type="eval",include=F)

# Behavior Rate (Onset) function Fx
modEff <- includeEffects(modEff,avExposure,name="al345",interaction1="net",type="rate",include=F)
modEff <- includeEffects(modEff,totExposure,name="al345",interaction1="net",type="rate",include=F)
modEff <- includeEffects(modEff,infectIn,name="al345",interaction1="net",type="rate",include=F)
modEff <- includeEffects(modEff,infectOut,name="al345",interaction1="net",type="rate",include=F)
modEff <- includeEffects(modEff,susceptAvIn,name="al345",interaction1="net",type="rate",include=T)
modEff <- includeEffects(modEff,susceptAvCovar,name="al13",interaction1="net",interaction2="sex13",
                         type="rate",include=F)
# * Set initial values ----
modEff[modEff$effectName=="sameSch" & modEff$type=="eval","initialValue"]<-0.48
modEff[modEff$effectName=="same sex" & modEff$type=="eval","initialValue"]<-0.40
# in the results object (for use with 'prevAns')
prevResult$theta[4]<- 0.48
prevResult$theta[5]<- 0.40

# * Check Model Spec -----
# Check
print(modEff, expand=TRUE)
fix(modEff)

# _____________________________________________________________________________
# * Run model -----
# _____________________________________________________________________________
modl <- acMod
prevResult <- alResult
#First time
alResult <- siena07(modl, data=dataOb, effects=modEff, batch=FALSE,
                    verbose=TRUE, nbrNodes=8, useCluster=TRUE, initC=TRUE)
#2nd thru last times
alResult <- siena07(modl, data=dataOb, effects=modEff, batch=FALSE,
                       verbose=TRUE, nbrNodes=8, useCluster=TRUE, initC=TRUE,
                       returnDeps=F,
                       prevAns=prevResult)
#______________________________________________________________________________

# Load/Save Workspace ----
load("T:/Data&Analysis/Data/Bethel/BethelW1345v2.RData")
save.image("T:/Data&Analysis/Data/Bethel/BethelW1345v2.RData")
