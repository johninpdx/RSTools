# Middle Layer Fns ---------------------------------------------------
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#Functions used for Peer INfluence data processing and analysis
# Naming conventions and abbreviatons:
# NQ: 'NetQ', the short survey comprising demographics,
#      the network questions, and several problem-behavior
#      inventories (Antisocial, bullying, victimization,
#      Alc, Tobacco and MJ use). 'Implicit Consent' used, so
#      response rates are typically over 80% for these questions.
# FQ: 'FullQ', the long survey added to the NetQ if participant gave explicit
#      consent. Questions include more significant antisocial behavior,
#      sexual behavior, sexual maturity, relationships w/parents.
# pWavVec: ordered integer vector of wave #'s for which data are requested
#      Waves 2,6, and 10 are all summer waves, where no NQ or FQ data is
#        collected (only EMA data)
#      Wave 1 is Spring of 2014 (8th grade, end of middle school) for Cohort
#         1 schools, Spring of 2015 for Cohort 2
#      Waves 3, 7 and 11 are Fall assessments (about October)
#      Waves 4 and 8 are Winter assessments (about January)
#      Waves 1, 5, and 9 are Spring assessments (about May)
# pSchVec: ordered integer vector of School IDs for which data are requested
# SID: Subject ID number
# SchID: School ID number
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# 'Without' operator (complement of %in%):

"%w/o%" <- function(x,y) x[!x %in% y] #Bin operator, x NOT IN y (vectors)
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getDiditXWave <<
# _____________________________________________________________________________
# Uses: getNQTVCs, AllNQNotNA, pkg 'RODBC' (indirect)
# _____________________________________________________________________________
#' Output: dataframe with flags indicating survey completion
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec A numeric vectorof School IDs to be included in the analysis.
#' @param pElig The minimum number of eligible waves for SID to be included.
#' @param pDid The minimum number of survey-completed waves for SID to be
#'     included.
#' @return A (#eligible SIDs x length(pWavVec)+1) dataframe. Col 1 is SID,
#'     Cols 2 on (each named 'wK', where K is the kth ordered wave in pWavVec)
#'     are flags with the integer 1 if individual SID 'completed' a survey
#'     for that wave (completion is determined in the function 'allNQQNotNA')
#' @examples
#' # gets dataframe including SIDs for waves 1, 3, 4, and 5, schools 3-6 and
#' # 30 (one school district); each included SID was eligible for the Short
#' # survey at least once, and completed at least 1 survey in these waves.
#' sidVec <- getSIDSet(c(1,3,4,5), c(3,4,5,6,30), 1, 1)
getDiditXWave <- function(pWavVec,pSchVec){
  #Get a node list of all eligible, so the output DF records participation
  #  for all eligible at any wave
  allNodes <- getEligNodes(pWavVec,pSchVec)
  # Construct output DF
  outDF <- data.frame(allNodes)
  names(outDF)[1] <- "SID"
  # Add cols to output DF for each wave, 1 if participant 'did it', else 0
  tvc <- allNQQNotNA(getNQTVCs(pWavVec,pSchVec))
  for (i in 1:length(pWavVec)){
    vWvN <- paste("wv",toString(pWavVec[i]),sep="") # new col name
    didItNodesi <- sort(unique(tvc[tvc$DidIt==1 & tvc$WID==pWavVec[i],"SID"]))
    outDF[,i+1] <- ifelse(match(allNodes,didItNodesi,nomatch=0) ==0,0,1)
    names(outDF)[i+1] <- vWvN
  }
  return(outDF[order(outDF$SID),])
}



#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getEligNodes <<
# _____________________________________________________________________________
# Uses:
#    getSWNodes, pkgs 'RODBC' (indirect)
# _____________________________________________________________________________
#' Creates a vector of SIDs who were Short Survey eligible on any input wave
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec A numeric vectorof School IDs to be included in the analysis.
#' @return A (#eligible SIDs)-length numerical vector of SIDs.
#' @details Used by other functions in this package (e.g. getNetworkSet,
#'     makeTVTbl, )
#' @examples
#' # gets vector of SIDs for waves 1, 3, 4, and 5, schools 3-6 and 30 (one
#' # school district)
#' sidVec <- getSIDSet(c(1,3,4,5), c(3,4,5,6,30), 1, 1)
getEligNodes <- function(pWavVec, pSchVec){
  # For efficiency, we create a list of
  # length w x s and create a vector of SIDs for each school/wave combination
  sidHolder <- vector("list", length(pWavVec)*length(pSchVec))
  for (i in seq_along(pWavVec)){
    for (j in seq_along(pSchVec)){
      # This call gets the SIDs of kids in school j, survey eligible in wave i
      sidHolder[[((i-1)*length(pSchVec))+j]] <- getSWNodes(pWav = pWavVec[i], pSch = pSchVec[j])
    }
  }
  # Combines unique SIDs into a sorted vector
  return (sort(unique(unlist(sidHolder))))
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getEligXWave <<
# _____________________________________________________________________________
# Uses: getSWNodes, pkg 'RODBC' (indirect)
# _____________________________________________________________________________
#' Creates a dataframe with flags x wave for Short Survey eligibility
#'
#'     The function is used to determine analyses sets (viz., by
#'     'getNetworkSet', 'makeTVTbl', 'getFixedCovs', and 'makeCCVec').
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec A numeric vector of School IDs to be included in the analysis.
#' @return A (#eligible SIDs x length(pWavVec)+1) dataframe. Col 1 is SID,
#'     Cols 2 on (each named 'wK', where K is the kth ordered wave in pWavVec)
#'     are flags with the integer 1 if individual SID was Short Survey (NetQ)
#'     eligible for that wave.
#' @details A sproc 'PInf1.dbo.StudentSIDInWave' in the SQL Server database
#'     PInf1 obtains eligible IDs. It is wapped by the function 'getSWNodes'
#'     in this package, called by the present function.
#' @examples
#' # Gets dataframe including SIDs for waves 1, 3, 4, and 5, schools 3-6 and
#' # 30 (one school district); each included SID was eligible for the Short
#' # survey at least once, and completed at least 1 survey in these waves.
#' EligXWave <- getEligXWave(c(1,3,4,5), c(3,4,5,6,30))
getEligXWave <- function(pWavVec,pSchVec){
  # Get the eligible node list for each wave
  nodeList <- vector("list",length(pWavVec))
  allNodes <- numeric()
  for (i in 1:length(pWavVec)){
    wavNodes <- numeric()
    for (j in 1:length(pSchVec)){
      nodesij <- getSWNodes(pWavVec[i],pSchVec[j],1,0)
      wavNodes <- append(wavNodes, nodesij, after=length(wavNodes))
      allNodes <- append(allNodes, nodesij, after=length(allNodes))
    }
    # Each list element is the eligible nodes for that wave
    nodeList[[i]]<-wavNodes
  }

  # Construct Output Dataframe
  allNodesUnique <- getEligNodes(pWavVec,pSchVec) # All Elig SIDs, all waves
  outDF <- data.frame(allNodesUnique)
  names(outDF)[1] <- "SID"

  # Will hold output
  vWvNames <-character(length(pWavVec))
  for (i in 1:length(pWavVec)){
    vWvN <- paste("wv",toString(pWavVec[i]),sep="") # new col name
    outDF[,i+1] <- ifelse(match(allNodesUnique,nodeList[[i]],nomatch=0) ==0,0,1)
    names(outDF)[i+1] <- vWvN
  }
  return(outDF[order(outDF$SID),])
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getMSRIDVect <<
# _____________________________________________________________________________
# Uses: getSWNodes
# _____________________________________________________________________________
#' Creates a list of *Row* ID vectors for each middle school requested
#'
#'     The function is used for multiple middle school districts to
#'     add structural zeros to a wave 1 network edge table.
#'
#' @param mSchVec A numeric vector of middle school IDs to be separated
#'     at Wave 1 (only, because in this study, participants are in middle
#'     school only for Wave 1) by structural zeros.
#' @param sidRowID A dataframe-format crosswalk of the SIDs and corresponding
#'     Row IDs in the analysis set being developed by the function that
#'     calls this one.
#' @return A length(mSchVec) list of Row IDs of individuals within
#'     each of the middle schools with IDs in 'mSchVec'.
#' @details Adding structural zeros is an option of 'getNetworkSet', which
#'     is where this function is currently used. In the PInf study, this is
#'     only meaningful for middle schools from the same school district at
#'     Wave 1.
#' @examples
#' # Gets dataframe including SIDs for the wave vector 'mSchVec' and the
#' # two-column dataframe 'sidRowID'.
#' msIDList <- getMSRIDVecs (mSchVec, sidRowID)
getMSRIDVecs <- function(mSchVec, sidRowID){
  outList <- vector(mode = "list", length = length(mSchVec))
  for(i in 1:length(mSchVec)){
    holdr1 <- getSWNodes(1,mSchVec[i])
    holdr2 <- sidRowID$RID[match(holdr1,sidRowID$SID)]
    outList[[i]] <- holdr2[!is.na(holdr2)]
  }
  return(outList)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getNetworkSet <<
#______________________________________________________________________________
# Uses:  getNetwork, pkgs 'network', 'Matrix', 'RODBC'
# _____________________________________________________________________________
#' Creates a list of networks forming a longitudinal Analysis Set
#'
#' An Analysis Set is a set of observations satisfying a particular set of
#'     criteria to be included in an analysis. Requiring complete
#'     data is too stringent for most longitudinal analysis. This is true in
#'     both descriptive network studies (where even individuals who come
#'     and go still form part of the social context) and also SAOM and
#'     related network modeling methods (which can include partial data on
#'     individuals if MAR assumptions are met)
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec ordered integer vector of school IDs
#' @param pElig the minimum number of eligible waves for SID to be included
#' @param pDid  the minimum number of completed waves for SID to be included
#' @param pTyp  string, defines type of network (BF => 'one of my best
#'               friends', FT => 'spent free time with')
#' @param pOut string, type of network format for output (SP =>
#'              dgTMatrix-class sparse matrix (pkg 'Matrix), NT =>
#'              network-class matrix (pkg 'network'))
#' @param pS0 string; if "S0", pOut = "SP", and pWavVec includes wave 1,
#'             structural zeros are inserted in the edge list of the w1
#'             networks between individuals in different middle schools.
#' @return A <pWavVec>-length list of either sparse matrix (dgTMatrix class)
#'     networks, one list element per wave, with RowIDs identifying chooser
#'     and chosen, -- OR -- 'network' class networks (pkg 'network'),
#'     one list element per wave (also with RowIDs). The last element of
#'     the list is a vector of the SIDs to be included in the analysis
#'      (this has many uses in other package functions).
#' @details The schools specified will normally comprise a single
#'  network, for descriptive or analysis purposes. If they are not, for some
#'  waves, then structural zeros should be included for pairs of nodes that
#'  cannot form relationships on particular waves (e.g. if the kids went to
#'  separate middle schools, and then all attend the same high school)
#' The 'length(pWavVec)+1st' element of the output contains a vector of
#'   the SIDs considered to be 'in the analysis' by the selection criteria
#'   implied by the input parameters. It's useful for obtaining
#'   corresponding variables such as vertex properties, RSIena covariates,
#'   and so on.
#' This function can be used to pull networks suitable for descriptive
#' analysis (e.g. by later converting them to 'network' objects and using 'sna')
#' or by combining them into a SAOM network object using sienaNet (from
#' 'RSiena') as shown in the example below.
#' @examples
#' # Creates a list of length 4; the first 3 elements of the list
#' # are dgTSparse-class matrices, one for each wave, for all the
#' # middle and high schools in one school district of the PInf study.
#' # Inclusion criteria are: eligible for 2 or more surveys, completed
#' # 2 or more surveys. Networks are defined as 'Best Friends'. Output
#' # networks are in dgTSparse format, and the Wave 1 middle schools
#' # have structural zeros between individuals in different schools.
#' netList <- getNetworkSet(pWavVec = c(1,3,4), pSchVec = c(3,4,5,6,30),
#'     pElig = 2, pDid = 2, pTyp = "BF", pOut = "SP", pS0 = "S0")
#' # Create SAOM longitudinal network object 'myNet'
#' myNet <- sienaNet(netList[1:3],sparse=T)
#' # Even easier way, using 'makeSAOMNet' wrapper function
#' myNet2 <- makeSAOMNet(netList)
getNetworkSet <- function(pWavVec, pSchVec, pElig=1, pDid=1, pTyp="BF",
                          pOut = "SP", pS0 = ""){
  # ______________________
  require(network, Matrix, data.table)
  # ______________________
  # Check parameters
  # ______________________
  if (pElig > length(pWavVec)){
    stop("Error: waves of eligibility cannot exceed # of waves.")
  }
  if (pDid > length(pWavVec)){
    stop("Error: number of completed surveys cannot exceed # of waves.")
  }
  if (pDid > pElig){
    stop("Error: # ofcompleted surveys cannot exceed # of eligible waves.")
  }
  if (!(pTyp %in% c("BF","FT"))){
    stop("Error: relationship type must be BF or FT")
  }
  if (!(pOut %in% c("SP","NT"))){
    stop("Error: output type must be SP or NT")
  }
  if (pS0 != "" & pOut != "SP"){
    stop("Error: Structural zeros only work with sparse output")
  }
  if (pS0 == "S0" & pWavVec[1] != 1){
    stop("Error: struc zeros only pertain to wave 1, which is not included")
  }
  # ___________________________________________________________________________
    cat("Begin obtaining SID Set for Analysis (this may take a minute or two)",
        "\n")
  subSIDs <- getSIDSet(pWavVec, pSchVec, pElig, pDid)
    cat("Done obtaining SID Set for Analysis", "\n")
  # Make a crosswalk of SIDs and row IDs
  sidRowID <- data.frame(subSIDs, seq(1, length(subSIDs)))
  names(sidRowID) <- c("SID", "RID")

  # Get the associated network from 'sAffiliation'--process to "network"
  #   object.
  # ___________________________________________________________________________
  net <- getNetwork(pWavVec, pSchVec) # Raw network
  rawNet <- with(net, net[SID %in% subSIDs & AffSID %in% subSIDs, ]) # Subset
  edgNet <- with(rawNet, rawNet[, c("SID", "AffSID", "bff", "WID")]) #Edge format
  rownames(edgNet) <- seq(length=nrow(edgNet))

  # Substitute RowIDs for SIDs ('network' objects require this)
  edgNet$SID    <- sidRowID$RID[match(edgNet$SID,sidRowID$SID)]
  edgNet$AffSID <- sidRowID$RID[match(edgNet$AffSID,sidRowID$SID)]
  names(edgNet) <- c("RIDOut", "RIDIn", "bff", "WID") #Names reflect rows now

  #++++++++++++++++
  cat("Got raw network, substituted Row IDs for SIDs", "\n")
  #++++++++++++++++

  # Create a new ** DATA.TABLE ** version of the networ from 'edgNet',
  #   with ones where the edges represent the type of relationship
  #   requested (currently BF of FT)
  if (pTyp=="BF"){
    # "One of my best friends" -- involves only selecting for bff>0
    rlpNet <- data.table(with(edgNet, edgNet[bff>0, ]))
    #++++++++++++++++
    cat("Relationship values coded ('best friend')", "\n")
    #++++++++++++++++
  } else {
    if (pTyp=="FT"){
      # "Spend free time with" -- set 0=1; all others are already 1
      rlpNet <- data.table(edgNet)
      rlpNet$bff <- ifelse(rlpNet$bff==0, 1, rlpNet$bff)
      #++++++++++++++++
      cat("Relationship values coded ('spent free time with')", "\n")
      #++++++++++++++++
    }
  }
  #++++++++++++++++


  # Split networks by wave, if more than one wave
  # _____________________________________________
  ll <- length(pWavVec)
  # will hold the output networks, plus the combined SID:
  outList <- vector(mode = "list", length = ll+1)
  nnodes <- length(subSIDs)
  #ccccccccccccccc
  cat("Begin constructing network output, type = ", pOut, "\n")
  #ccccccccccccccc
  for (i in 1:ll){
    # NOTE: the selection of wave ('rlpNet$WID') effectively reduces the
    #       raw edgelist to a 'triplet' (RIDOut, RIDIn, value), as required by
    #       both the 'spMatrix' and 'network' functions.
    if (pOut == "SP"){
      if(pS0 == "S0" & pWavVec[i] == 1){
        # Add Structural Zeros between any elementary schools, if requested
        # Select just the elementary schools
        mSchVec <- pSchVec[pSchVec %in% c(1:9,101,102)]
        if (length(mSchVec) > 1){
          # This is where the structural zeros are inserted into 'rlpNet'
            cat("Begin inserting structural zeros", "\n")
          # _______________________________
          # Create vectors of RowIDs associated with each input middle school
          msIDList <- getMSRIDVecs (mSchVec, sidRowID)
          # Get the new rows (rlpNet format) with S0's inserted between MS's
          rlpNetS0 <- s0Assemble(msIDList)
          # Add the new rows to rlpNet (notice the 'data.table' syntax...)
          rlpNet2 <- do.call(rbind,list(rlpNet,
                                        rlpNetS0[,.(RIDOut,RIDIn,bff,WID)]))
          #_________________________________
            cat("Done insterting structural zeros","\n")

        } else{
          cat("Warning: <2 middle schools given; struc 0 request ignored","\n")
        }
        # This one has structural 0s
        outNeti <- with (rlpNet2[rlpNet2$WID == pWavVec[i], ],
                         spMatrix(nnodes, nnodes,
                                  RIDOut, RIDIn, x = bff))
      }
      else{
        # This one has no structural 0s
        outNeti <- with (rlpNet[rlpNet$WID == pWavVec[i], ],
                         spMatrix(nnodes, nnodes,
                                  RIDOut, RIDIn, x = bff))
      }
    } else {
      # Notice that 'network' requires the input matrix columns *backwards*
      # (RIDIn, RIDOut, value). (Note again data.table syntax)
      if (pOut == "NT"){
        mtx <- data.matrix(rlpNet[rlpNet$WID == pWavVec[i],
                                  .(RIDIn,RIDOut,bff)])
        outNeti <- network(mtx,
                         directed = TRUE,
                         matrix.type = "edgelist")
      }
    }
    itemName <- paste("wv", toString(pWavVec[i]), sep = "")
    outList[[i]] <- outNeti
    names(outList)[i] <- itemName # name it
  }
  #cccccccccccccccc
  cat("Done constructing network output", "\n")
  #cccccccccccccccc
  outList [[ll+1]] <- subSIDs # This is returned as a convenience
  names (outList)[ll+1] <- c("subSIDs")
  return (outList)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getSIDSet <<
# _____________________________________________________________________________
# Uses:  getEligXWave, getDidItXWave, pkgs 'network', 'Matrix'
# Used by: getNetworkSet
# _____________________________________________________________________________
#' Creates a vector of SIDs that are consistent with some Analysis Set
#'
#' This function is a bit slow, probably because it uses calls to
#'     'getEligXWave' and 'getDiditXWave'...but it still runs in a couple
#'     of minutes even for fairly large Analysis Sets, and this design
#'     has the advantage of not requiring the output from these functions
#'     to be created ahead of time and passed in.
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec A numeric vectorof School IDs to be included in the analysis.
#' @param pElig the minimum number of eligible waves for SID to be included
#' @param pDid  the minimum number of completed waves for SID to be included
#' @return An Analysis Set-length numerical vector of SIDs.
#' @details Used by other functions in this package (e.g. getNetworkSet).
#'     Could be used stand-alone but is not really designed to be.
#' @examples
#' # Obtains the SID set for a network analysis:

#' # Use the last element of 'netList', which is a vector of the SIDs in
#' # the analysis
getSIDSet <- function(pWavVec, pSchVec, pElig=1, pDid=1){
  #_______________________
  require(network, Matrix)
  #_______________________
  ll <- length(pWavVec) # saves some line space below
  if (pElig < pDid){
    stop("Error: You cannot ask for more completed surveys than elig waves")
  }
  if (pElig>ll | pDid>ll){
    stop("Error: More completed surveys or elig waves than total waves ")
  }
  # Create an eligibility and suvery-completion ("did") dataframe; SID, then
  #  'll' cols for Elig flag, then ll cols for 'did' flags
  bothDF <- merge(getEligXWave(pWavVec,pSchVec),getDiditXWave(pWavVec,pSchVec),
                  by = "SID")

  # Determine the node set based on input parameters
  # (column calcs count 1's in the cols for eligibility x wave and then
  #  for survey completion by wave, contained in the last 2*ll cols).
  # This set determines who will be considered 'part of the analysis',
  # (although in RSiena, 'composition change' can be used to alter this
  #  wave by wave).
  # ___________________________________________________________________________
  if (ll>1){
    # (this is necessary because rowSums will not work if it's only
    #  summing 1 column. Is that stupid, or what??)
    subSIDs <- sort(bothDF[rowSums(bothDF[,c(2:(ll+1))]) >= pElig
                           & rowSums(bothDF[,c((ll+2):(1+(2*ll)))]) >= pDid
                           ,"SID"])
  } else {
    subSIDs <- sort(bothDF[bothDF[,2]>=pElig & bothDF[,3]>=pDid,"SID"])
  }
  return(subSIDs)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeSAOMNet <<
# _____________________________________________________________________________
# Uses: pkgs 'Matrix, 'RSiena' (or 'RSienaTest')
# _____________________________________________________________________________
#' Creates a longitudinal 'sienaNet' object directly from 'getNetworkSet'
#'
#' This function is a very thin wrapper for RSiena::sienaDependent (or
#'     equivalently, 'sienaNet'), saving maybe half a line of code. It takes
#'     the output from 'getNetworkSet' with no additional parameters or
#'     modification (BUT the networks included in that output must be of
#'     class dgTSparse), and creates a longitudinal network dependent
#'     variable object from it, using whatever networks it is given (w of
#'     them).The last element of the 'getNetworkSet' function output, a
#'     vector of the SIDs included in the analysis set, is ignored.
#'
#' @param pNetInput The list of length (w+1) generated by 'getNetworkSet',
#'     where the first w elements *must be* class dgTSparse networks.
#' @return A 'sienaNet' object, ready to be used as an endogenous network
#'     variable in a SAOM.
#' @examples
#' # 'net' will be a 'sienaNet' network dependent variable:
#'   net <- makeSAOMNet(getNetworkSet(pWavVec = c(1,3,4),
#'     pSchVec = c(3,4,5,6,30),
#'     pElig = 2, pDid = 2, pTyp = "BF", pOut = "SP", pS0 = "S0"))
makeSAOMNet <- function(pNetInput){
  #__________________
  require(RSienaTest)
  #__________________
  lenlst <- length(pNetInput) # how long is the list?
  if (class(pNetInput[[lenlst]])[1] != "dgTMatrix"){
    lenlst <- lenlst - 1 #Ignore last list element (it's an SID vector)
  }
  if (class(pNetInput[[1]])[1] != "dgTMatrix"){
    stop("Error: Input must begin with a list of sparse matrices")
  }

  outSienaNet <- sienaDependent(pNetInput[1:lenlst],sparse=TRUE)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeTVTbl <<<
# _____________________________________________________________________________
# Uses: getTVCCols, pkgs dplyr, data.table
# _____________________________________________________________________________
#' Creates a data.table of a time varying behavior variable
#'
#' @param pCCTbl A dataframe or data.table of all the time-varying covariates
#'  in long-form (1 row per subject per wave) produced by the function
#'     'getNQTVCs'. It is an image of the database table PInf1.dbo.SWave.
#' @param pSIDs A numeric vector of SIDs of the individuals in this Analysis
#'     Set (i.e. for example, created by 'getNetworkSet'). Ensures the table
#'     created is consistent with such a set.
#' @param pVar A string giving the name of the variable to create. These are
#'     defined in 'getNQTVCs'. Options are listed below.
#' @param pCut  A numeric vector of cut points for classifying values of
#'     the requested variable (see Base R function 'cut'). Intervals are
#'     closed on the left, open on the right.
#' @param pMaxNA The maximum number of NAs among the variable's items
#'     before the returned value for the particular subject-wave is set to NA
#' @return The table format is w+1 columns (w=#waves present in pCCTbl); first
#'     column is an ordered set of SIDs, columns 2:(w+1) are the scored
#'     (mean of items) value of the requested behaivor variable
#'     for the w waves.
#' @details May be used to create tables that can be easily converted to SAOM
#'     dependent behavior or time-varying covariate (predictor) variables by
#'     just selecting the last w columns (see example below). Generally such
#'     variables will be consistent with a network Analysis Set (of
#'     individuals) produced by 'getNetworkSet', in which case 'pSIDs' can
#'     simply be taken from the last list element of the output of the
#'     latter function. Of course, if you wanted to create variables for any
#'     longitudinal analysis (i.e. score them, select the individuals and
#'     waves you wanted), you could use this function as well, but you would
#'     need to generate the pSIDs by directly executing 'getSIDSet'.
#' @note Choices for pVar are defined in 'getTVCCols'. They are:
#'     AB (Antisocial behavior),
#'     OV (Others victimization of you),
#'     YV (You victimizing others),
#'     T3 (Tobacco, freq of use in last 30 days),
#'     E3 (E-tobacco, freq of use in last 30 days),
#'     C3 (Chewing Tobacco, freq of use in last 30 days),
#'     A3 (Alcohol, freq of use in last 30 days),
#'     B3 (Binge drinking, freq in last 30 days),
#'     M3 (Marijuana use, freq in last 30 days),
#'     AL (Alcohol, freq of use, lifetime up to now).
#' @examples
#' # When the network part of an analysis set is created by 'getNetworkSet',
#' # the last element of the output is a vector of participating SIDs. We
#' # use that as part of the input to the present function.
#' netList <- getNetworkSet(pWavVec = c(1,3,4), pSchVec = c(3,4,5,6,30),
#'     pElig = 2, pDid = 2, pTyp = "BF", pOut = "SP", pS0 = "S0")
#' # Create the time varying behavior table
#' ccTbl <- getNQTVCs(pWavVec = c(1,3,4), pSchVec = c(3,4,5,6,30))
#' # Creates a data.table of antisocial behavior variables x subject & wave,
#' # for the 3 waves & other criteria used above, grouped into 4 categories,
#' # and with no more than 3 NAs out of the 6 items.
#' abTbl <- makeTVTbl(ccTbl, netList[[4]], pVar = "AB",
#'     pCut = c(0,.5,1,5,20), pMaxNA = 3)
makeTVTbl <- function(pCCTbl, pSIDs, pVar="X", pCut = c(0), pMaxNA = 1){
  require(dplyr, data.table)
  if (pVar == "X"){
    cat("Warning: No var name supplied; name defaults to X", "\n")
  }
  # Create a name for the binned (final form, scaled) variable:
  binVar <- paste(pVar,"B", sep = "") # Name of binned variable (e.g."ABB", ..)
  # Select only the SIDs from the input data.table to be used in the analysis
  ccDT <- data.table(filter(pCCTbl,SID %in% pSIDs))
  setkey(ccDT, SID, WID)

  # Get the vector (length >= 2) of variables implied by pVar
  items4TVC <- getTVCCols(pVar)

  # Calculate rowmeans
  ccDT[[pVar]] <- rowMeans(ccDT[, items4TVC, with = F],na.rm = T)
  # find rows for which the pVar needs to be set to NA. Only looks at the sub-
  #  dataframe with col names in 'items4TVCs'. naSums contains #NAs per row.
  naSums <- apply(ccDT[, items4TVC, with = F], 1, function(x) sum(is.na(x)))
  ccDT[[pVar]][which(naSums > pMaxNA)] <- NA

  # Bin 'pVar' according to the cut points in 'pCut'; place in 'binVar'
  ccDT[[binVar]] <- as.numeric(cut(ccDT[[pVar]], pCut, right=F,
                                     include.highest = T,
                                     labels = c(1:(length(pCut)-1))))


  # Build the DT for this TVC. The target is a length(subSIDs) x
  #  length(pWavVec)+1 DT. The last length(pWavVec) set of cols is then
  #  input as a matrix to 'sienaDependent' or 'varCovar'.
  # The whole DT is returned.
  ccDTLong <- data.table(ccDT$SID)
  ccDTLong$WID <- ccDT$WID
  ccDTLong[[pVar]] <- ccDT[[binVar]] # output name will be the original
  names(ccDTLong) <- c("SID", "WID", pVar)
    ccDTLong[[pVar]] <- ccDTLong[[pVar]] - 1 #makes scales 0-based
  ccDTWide <- reshape(ccDTLong,timevar = "WID", idvar = "SID",
                      direction = "wide")
  return(ccDTWide)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> s0Assemble <<
# FFFFFFFFFFFFFFFFF
# Output:
#   A (typically large) *data.table* (new, improved 'dataframe') with columns
#    * rNum (row number)
#    * RIDOut (row # chooser)
#    * RIDIn (row # chosen)
#    * bff (relationship code, set to 10 for SAOM structural 0)
#    * WID (set to 1, because S0's are only relevant to wave 1 schools)
# _____________________________________________________________________________
# Input:
#    pMSchVec: list of vectors, each containing the Wave 1 analysis set RowIDs
#              for a specific middle school (identity irrelevant)
# _____________________________________________________________________________
# Used by: getNetworkSet (to add structural zero edges to sparse matrix output)
# Uses: s0Make (which uses pkg 'data.table')
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
s0Assemble <- function(pMSchVec){
  # Calculate number of combinations (order irrelevant)
  ll <- choose(length(pMSchVec),2)
  numSch <- length(pMSchVec)
  tblList <- vector(mode = "list", length = ll)
  k <- 1
  for (i in 1:(numSch-1)){
    for(j in (i+1):numSch){
      tblList[[k]] <- s0Make(pMSchVec[[i]],pMSchVec[[j]])
      k <- k+1
    }
  }
  # Collapse listed DTs into one
  # (this is actually very fast...surprisingly!)
  outTbl <- do.call(rbind,tblList)
  return(outTbl)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> s0make <<
#FFFFFFFFFFFFFFFFFFF
# Output:
#   An n x k data TABLE in the format of the variable 'rlpNet', used inside
#   function 'getNetworkSet', with a set of STRUCTURAL ZERO edges added
#   between the cartesion product of the two vectors of RowIDs.
# _____________________________________________________________________________
# Input:
#  --Two vectors of (not necessarily consecutive or ordered) RowIDs, of lengths
#    n and k.
#  --Note: since only Wave 1 schools are considered 'separate networks', this
#    function always sets wave (WID) to 1.
# _____________________________________________________________________________
# Used by: s0Assemble (which is called by getNetworkSet)
# Uses: pkg 'data.table'
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
s0Make <- function(pVec1, pVec2){
  require(data.table)

  # calculate number of rows needed (all combinations ofIDs, in
  #  both directions)
  numrows <- length(pVec1) * length(pVec2)*2
  # This code gives you two data frames, the first with all
  # combinations of pVec1 x pVec2, the second with these
  # reversed. These comprise all the combinations requiring
  # structural zeros
  DTpart1 <- expand.grid(pVec1,pVec2)
  DTpart2 <- expand.grid(pVec2,pVec1)
    names(DTpart1) <- names(DTpart2) <- c("RIDOut","RIDIn")
  # Combine them into a single data table, and add the other
  # two cols. This code is VERY efficient, btw ... :D
  outDT <- data.table(rNum = c(1:numrows),
                      RIDOut=c(DTpart1$RIDOut,DTpart2$RIDOut),
                      RIDIn =c(DTpart1$RIDIn, DTpart2$RIDIn), bff=10, WID=1)
  setkey(outDT,rNum) #Gives it a key, for binary searching
 return(outDT)
}

# DB Layer Fns ----------------------------------------------------------

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getDOB <<
#FFFFFFFFFFFFFF
# This function gets DOB for these schools & waves.
# _____________________________________________________________________________
# Input is a vector of SIDs, typically a set of kids you're doing
# data analysis on.
# Output is a dataframe with two columns: SID (sort order) and DOB
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
getDOB <- function(pSIDVec){
  require(RODBC)
  dobQuery <- paste ("SELECT SID,DOB",
                     " From vStudentCurrent",
                     " WHERE SID In (",paste(pSIDVec,collapse=","),
                     ") ORDER BY SID ")
  # Extract data
  conn <- odbcConnect(dsn="PInf1")
  dobData<- sqlQuery(conn,dobQuery)
  return(dobData)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getFixedCovs <<
#FFFFFFFFFFFFFFFFFFFF
# This function gets all SMaster records for these schools & waves.
# _____________________________________________________________________________
# Obtains dataframe of fixed NQ data (from the table 'SMaster' in the
# database 'PInf1'), for one " (pSchVec)"school cohort".
# To get only the SIDs for a given set of waves, you have to select the
# relevant SIDs first with other code, then select from this data frame.)
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
getFixedCovs <- function(pSchVec){
  require(RODBC)
  smQuery <- paste ("SELECT * From PInf1.dbo.SMaster WHERE SID IN ",
                    "(SELECT Distinct SID From PInf1.dbo.qqelig ",
                    "WHERE SchID In (",paste(pSchVec,collapse=","),
                    "))")
  # Extract data
  conn <- odbcConnect("PInf1")
  smData <- sqlQuery(conn,smQuery)
  return(smData)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getNetwork <<
#FFFFFFFFFFFFFFFFFF
# Output: a dataframe of network data (edgelist format)
#______________________________________________________________________________
# Input:
#    pWavVec: ordered integer vector of wave numbers
#    pSchVec: ordered integer vector of school IDs
#______________________________________________________________________________
# Uses: pkg 'RODBC'
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
getNetwork <- function(pWavVec,pSchVec){
  require(RODBC)
  dbQuery <- paste("SELECT * From PInf1.dbo.SAffiliation ",
                   "WHERE SchID In (",paste(pSchVec,collapse=","),
                   ") AND WID IN (",paste(pWavVec,collapse=","),")")
  conn <- odbcConnect(dsn="PInf1")
  outNet <- sqlQuery(conn, dbQuery)
  return(outNet)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getNQTVCs <<
#FFFFFFFFFFFFFFFFF
# Output:
#   A data.table of time-varying NQ data (from the table 'SWave' in the
#   database 'PInf1')
#______________________________________________________________________________
# Input:
#    pWavVec: integer vector of wave numbers
#    pSchVec: integer vector of school IDs
#______________________________________________________________________________
# Uses: pkgs 'RODBC', 'data.table'
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
getNQTVCs <- function(pWavVec,pSchVec){
  require(RODBC,data.table)
  nqQuery <- paste ("SELECT SchID, SID, WID, ULied,UHit,UMean,USkip,UDamage,ULate,UFam,",
                    "UOKids,ONames,OHit,OThreat,ONoTalk,OExclu,OGossip,OEncourg,",
                    "YNoTalk,YExclu,YGossip,YLies,YEncourg,TobLife,Tob30Day,",
                    "ETobLife,ETob30Day,ChewLife,Chew30Day,AlcLife,Alc30Day,",
                    "BngLife,Bng30Day,MJLife,MJ30Day,OptOutNow ",
                    "FROM PInf1.dbo.SWave ",
                    "WHERE SchID In (",paste(pSchVec,collapse=","),
                    ") AND WID IN (",paste(pWavVec,collapse=","),")")
  # Extract data
  conn <- odbcConnect(dsn="PInf1")
  nqData <- data.table(sqlQuery(conn,nqQuery), key = "SID")
  return(nqData)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getSWNodes <<
#FFFFFFFFFFFFFFFF
# Output: A vector of nodes (SIDs) for individuals who are NQ (only) or
#         NQ + FQ eligible (dep on parameters).
#
# Get survey-eligible nodelist for a *specific* school (pSch) and wave (pWav),
# (hence the 'SW' in the function name)
# who have done a NetQ (pNQW) or a FullQ (pFQW).
# (You can't ask for both NQ and FQ, it's one or the other)
# _____________________________________________________________________________
# Input:
#    pWavVec: integer vector of wave numbers
#    pSchVec: integer vector of school IDs
#    pNQW: If 1, produces NQ-elig nodes during this wave for this school
#    pFQ:  If 1, produces FQ-elig nodes only (pNQW must be 0)
# _____________________________________________________________________________
# Uses: pkgs 'RODBC', 'data.table'
#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
getSWNodes <- function(pWav, pSch, pNQW = 1, pFQW = 0){
  require(RODBC, data.table) # NOTE: data.table syntax
  # Infer cohort from SchID
  coh1Schools <-c(1,2,3,4,5,6,10,20,30,31)
  coh2Schools <-c(7,8,9,70,80,101,102,110,111,112,113,120)
  if (pSch %in% coh1Schools){pCoh<-1}
  if (pSch %in% coh2Schools){pCoh<-2}
  if (!(pSch %in% coh1Schools | pSch %in% coh2Schools)){
    stop("School ",pSch," does not exist.")
  }
  # Infer period from WID and Cohort
  pPer <- ifelse (pCoh==1,pWav,pWav+4)
  if ((pCoh==1 & pPer>11) | (pCoh==2 & pPer>15) | pWav %in% c(2,6,10)){
    stop("Wave # ",pWav," does not exist or is a summer wave (no data)")
  }
  # Extract data
  conn <- odbcConnect("PInf1")
  # The sproc called here creates a DB table of survey-eligible kids for this
  # cohort and wave. Then the correct school is selected from that table.
  # ____________________________________________________
    cat("Begin pulling eligible SIDs from database", "\n")
  tblOfNodesQuery <- paste("exec PInf1.dbo.StudentSIDInWave @CohortID = ",pCoh,",@PeriodID = ",
                           pPer,",@Wave = ",
                           pWav,",@NQWanted = ",pNQW,",@FQWanted = ",pFQW)
  eligNums <- sqlQuery(conn,tblOfNodesQuery)
  nodeQuery <- paste("SELECT SID,SchID From PInf1.dbo.NQEligTbl ")
  nodes_temp <- data.table(sqlQuery(conn,nodeQuery), key = "SID")
  #
    cat("Done pulling eligible SIDs from database", "\n")
  # _____________________________________________________
  # If the query returned no records, return a length 0 vector
  if (dim(nodes_temp)[1] == 0){
    return (integer())
  } else {
  # Since kids could have been eligible in multiple schools,
  # check each against the kid's SWave record (if he/she has one). If the kid
  # shows a different SchID in SWave then pSch, mark that node for
  # removal.
  # ____________________________________________________
    cat ("Begin test of eligibility vs. actual data in SWave", "\n")
  sWaveQuery <- paste("SELECT SID, SchID From PInf1.dbo.SWave ",
                      " WHERE SchID =",pSch, " AND WID=",pWav)
  sWaveSIDSch <- data.table(sqlQuery(conn,sWaveQuery),key = "SID")
  # select any rows for which school ID's in Elig and Actual Survey (sWave)
  #  DTs do not match; these SIDs should not be in this school.
  SIDDifSch <- sWaveSIDSch[SchID != pSch,.(SID)]
  nodes <- nodes_temp[SID %w/o% SIDDifSch & !is.na(SID)] # The 'eligible' data.table

  return (nodes[SchID == pSch,SID])
  }
}

#  Create/Score Vars ----------------------------------------------------------

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> allNQQNotNA <<
#FFFFFFFFFFFFFFFFFFF
# Adds a variable 'DidIt' to the input dataframe pDF of the NQ questions
# (e.g. output of 'getNqTVCs').
#_____________________________________________________________________________-
# 'DidIt'=1 if kid answered any of 4 lifetime SU questions, or 2 of the
# AB questions, and =0 otherwise.
# Weeds out kids who have a an SWave record but did not actually do the
#  survey.
#_____________________________________________________________________________
allNQQNotNA <- function(pDF){
  pDF$DidIt <- ifelse(!is.na(pDF$ULied) | !is.na(pDF$UHit)
                      | !is.na(pDF$TobLife)
                      | !is.na(pDF$AlcLife)
                      | !is.na(pDF$BngLife)
                      | !is.na(pDF$MJLife),1,0)
  return(pDF)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> createOnset <<
#FFFFFFFFFFFFFFFFFFF
# Output: data.table (or dataframe) with SID followed by onset variables
# in each row.
# __________________
# Input:
#  -- pInDT: A data.table (or dataframe) containing SID plus a lifetime use
#     (NA, 0, >0) variable for each of c-1 ordered time periods, where c is
#     the number of columns in each DF.
#  -- pTHold: a number indicating the threshold value for onset; default is
#     1 (typically, any use). Onset = 1 if input value >= pTHold.
# NOTE:
# This function assumes there could be reporting error, i.e. a later column
#   value could be 0, though an earlier one was not. It assumes the first
#   positive event (ignorning NAs) is correct and everything before was 0.
# There is also an implicit assumption that NA==0; hence this function
#   codes the first KNOWN onset.
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
createOnset <- function(pInDT,pTHold=1){
  colnum <- dim(pInDT)[2]
  rownum <- dim(pInDT)[1]
  OutDT  <- pInDT
     OutDT[,2:colnum] <- 0
  foundEvent <- 0 #Flag gets set if any 'event' is found
  for (i in 1:rownum){
  for (j in 2:colnum){
    if (!is.na(pInDT[[i,j]])){
    if (pInDT[[i,j]]>=pTHold){
      foundEvent <- 1
      for (k in j:colnum){
        OutDT[[i,k]] <- 1
      }
      break
    }} #nested Ifs
   }#i loop
  } #j loop
  return(OutDT)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> createNbhdOnset
# FFFFFFFFFFFFFFFFF
# This is like 'createNewOnset' except the first onset event is lagged 1
# wave, to show the 'Onset Neighborhood' the individual had in the wave
# prior to onset. This is the wave that serves to 'condition' onset in SAOM.
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
createNbhdOnset <- function(pInDT,pTHold=1){
  # pInDT is a data.table (or dataframe) with 1 row for each subject.
  # The first column is SID
  # The second through cth columns are 0 if the event has not yet occurred
  #   up to time (c-1), > pTHold if it has.
  colnum <- dim(pInDT)[2]
  rownum <- dim(pInDT)[1]
  OutDT  <- pInDT
  OutDT[,2:colnum] <- 0
  foundEvent <- 0 #Flag gets set if any 'event' is found
  for (i in 1:rownum){
    for (j in 2:colnum){
      if (!is.na(pInDT[[i,j]])){
        if (pInDT[[i,j]]>=pTHold){
          foundEvent <- 1
          if ((j-1)>=2) {
            OutDT[[i,j-1]] <- 1
            OutDT[[i,j]] <- 2
          } else {
            OutDT[[i,j]] <- 2
          }
          for (k in j+1:colnum){
            OutDT[[i,k]] <- 2
          }
          break
        }} #nested Ifs
    }#i loop
  } #j loop
  return(OutDT)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> createNewOnset
# FFFFFFFFFFFFFFFFF
# This is like 'createOnset' except the first onset event is 1, and all later
# values are 2. It is a way to identify NEW onsets, e.g. in a set of network
# plots, etc.
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
createNewOnset <- function(pInDT,pTHold=1){
  # pInDT is a data.table (or dataframe) with 1 row for each subject.
  # The first column is SID
  # The second through cth columns are 0 if the event has not yet occurred
  #   up to time (c-1), > pTHold if it has.
  colnum <- dim(pInDT)[2]
  rownum <- dim(pInDT)[1]
  OutDT  <- pInDT
  OutDT[,2:colnum] <- 0
  foundEvent <- 0 #Flag gets set if any 'event' is found
  for (i in 1:rownum){
    for (j in 2:colnum){
      if (!is.na(pInDT[[i,j]])){
        if (pInDT[[i,j]]>=pTHold){
          foundEvent <- 1
          OutDT[[i,j]] <- 1
          for (k in j+1:colnum){
            OutDT[[i,k]] <- 2
          }
          break
        }} #nested Ifs
    }#i loop
  } #j loop
  return(OutDT)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getTVCCols <<
# FFFFFFFFFFFFFFFFFFFFF

# THIS WORKS BUT IS USED ONLY BY THE  FUNCTION 'getTVCs', WHICH IS NOT FINISHED

# Given an input variable code, this function returns a vector of the
#  column names from 'SWave' (generated also by 'getNQTVCs') that will
#  need to be used (or aggregated) to produce the desired measure
#
# NetQ Only So Far <<<<<<------ !!!!
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

getTVCCols <- function(pVar){
  if (!(pVar %in% c("AB", "OV", "YV", " T3", "E3", "C3", "A3",
                    "B3", "M3","AL"))){
    stop("Error: Var must be AB, OV, YV, T3, E3, C3, A3, B3, M3, AL")
  }
  if (pVar == "AB"){
    # Antisocial behavior
    outCols <- c("ULied", "UHit","UMean", "USkip", "UDamage", "ULate")
  }
  if (pVar == "OV"){
    # Others victimize you
    outCols <- c("ONames", "OHit", "OThreat", "ONoTalk",
                 "OExclu", "OGossip", "OEncourg")
  }
  if (pVar == "YV"){
    # You victimize others
    outCols <-c("YNoTalk", "YExclu", "YGossip", "YLies", "YEncourg")
  }
  if (pVar == "T3"){
    # Tobacco use last 30 days
    outCols <- c("Tob30Day")
  }
  if (pVar == "E3"){
    # E-cig use last 30 days
    outCols <- c("ETob30Day")
  }
  if (pVar == "C3"){
    # Chew tobacco last 30 days
    outCols <- c("Chew30Day")
  }
  if (pVar == "A3"){
    # Alcohol freq last 30-days
    outCols <- c("Alc30Day")
  }
  if (pVar == "B3"){
    # Binge drinking freq last 30 days
    outCols <- c("Bng30Day")
  }
  if (pVar == "M3"){
    # MJ freq last 30 days
    outCols <- c("MJ30Day")
  }
  if (pVar == "AL"){
    # Alc Lifetime Freq
    outCols <- c("AlcLife")
  }
  return(outCols)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> netvtxAttr <<
#FFFFFFFFFFFFFFFFF
# Output:
#   a DF with col.2 OK for assignment a to a 'network' object as a
#   NODE ATTRIBUTE
# _____________________________________________________________________________
# Input:
#  pAttrMasterDF: A 2-col data frame with SID and the vals of some variable 'x'
#                 for at least all SIDs in the school and wave range, for
#                 which there are data (if no data, attribute will be 'NA')
#  pSIDVec: A vector of SIDs exactly matching the 'node set' for the network
#           of interest (e.g. if the network has n nodes, this vector is of
#           length n) -- this will be the length of the vertex DF returned.
#  pNetwork: the 'network' object to assign the vertex attribute to
#
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
netVtxAttr <- function(pAttrMasterDF,pSIDVec,pNetwork) {
  # Check a few things
  if (length(pAttrMasterDF)!=2){
    stop("Dataframe containing attributes does not have 2 cols.")
  }
  if (length(pNetwork$oel)!=length(pSIDVec)){
    stop("Network vertex count not equal to # SIDs (pSIDVec)")
  }
  # make sure everything is in order, because vertex attributes are
  # assigned based on some original order of the nodes, which in our
  # case will correspond to SID order
  pAttrMasterDF <- pAttrMasterDF[order(pAttrMasterDF$SID),]
  pSIDVec <- sort(pSIDVec)
  targDF <- merge(as.data.frame(pSIDVec),pAttrMasterDF,
                  by.x = getNameAsString(pSIDVec), by.y = "SID", all.x = T)
  names(targDF)[1] <- "SID"
  return(targDF)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> zero30dSU <<
#FFFFFFFFFFFFFFFFF
# Zeros out last-30-day subs use in a DF containing both lifetime and
#  30-day substance use, if lifetime use is 0
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
zero30dSU <- function(pDF){
  # The pDF has to be an object created by 'getNQVTCs' or equivalent
  cct <- transform(pDF,
                   Tob30Day = ifelse(TobLife==0,0,Tob30Day),
                   ETob30Day = ifelse(ETobLife==0,0,ETob30Day),
                   Chew30Day = ifelse(ChewLife==0,0,Chew30Day),
                   Alc30Day = ifelse(AlcLife==0,0,Alc30Day),
                   Bng30Day = ifelse(BngLife==0,0,Bng30Day),
                   MJ30Day = ifelse(MJLife==0,0,MJ30Day)
  )
  return (cct)
}

# Utility Fns -----------------------------------------------------------------

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> addRow <<
#FFFFFFFFFFFFFF
# This function adds a row to a dataframe. It replaces rbind, which is very
# slow. The rows are tacked on to the end of the DF. It's still a bad way to
# add rows!
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
addRow <- function(existingDF, newrow) {
  r<-dim(existingDF)[1]
  existingDF[r+1,] <- newrow
  existingDF
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getNameAsString <<
#FFFFFFFFFFFFFFFFFFFFFFF
# You give it a variable, it gives you a character string of the variable's
#  name
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
getNameAsString <- function(pVarName){
  deparse(substitute(pVarName))
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeCCVec <<
#FFFFFFFFFFFFFF
# This function creates a GENERAL comp-change list.
#_____________________________________________________________________________
#pElig is an n x 1+w dataframe, with 1 row for every kid who was survey-eligible
# for any of the waves of interest, and 1 col for each wave, containing 1 if
# the kid was survey-eligible that wave, and 0 if not (col 1 is SID)
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
makeCCVec <- function(pElig){
  wv<-dim(pElig)[2] # cols (waves-1)
  nn<-dim(pElig)[1] # rows (nodes)
  CCVec <- vector("list", nn) # empty list with nn elements
  #
  for (i in 1:nn){
    for (j in 2:wv) {
      # -- Wave is 1st wave
      #browser()
      if (j==2 & pElig[i,j]==1) {CCVec[[i]]<-c(1)}
      # -- any other wave than 1st or last
      if (j>2 & j<wv) {
        # switch from 1 to 0
        if(pElig[i,j]==0 & pElig[i,(j-1)]==1){
          CCVec[[i]]<-c(CCVec[[i]],c(j-1.5))}
        if(pElig[i,j]==1 & pElig[i,(j-1)]==0){
          CCVec[[i]]<-c(CCVec[[i]],c(j-1.5))}
      } # if j>2,<wv
      # -- if last wave
      if (j==wv) {
        if(pElig[i,wv]==0 & pElig[i,(wv-1)]==1){
          CCVec[[i]]<-c(CCVec[[i]],wv-1.5)}
        if(pElig[i,wv]==1 & pElig[i,(wv-1)]==0){
          CCVec[[i]]<-c(CCVec[[i]],c(wv-1.5,wv-1))}
        if(pElig[i,wv]==1 & pElig[i,(wv-1)]==1){
          CCVec[[i]]<-c(CCVec[[i]],c(wv-1))}
      } # if j==wv
    } # j loop
  } # i loop

  return(CCVec)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> pValT <<
#FFFFFFFFFFFF
# Calculates t, 2-tailed p value, 95% CI
# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
pValT <- function(rawDiff,sE){
  #rawDiff<-  2.51
  #sE<-  .87
  tRat<-rawDiff/sE
  #p-Value
  pVal2t <- (1-pnorm(abs(tRat)))*2
  #95% CI
  ci95<-c(rawDiff-(1.96*sE),rawDiff+(1.96*sE))
  c(tRat,pVal2t,ci95)
}
