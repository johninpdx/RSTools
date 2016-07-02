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
# Uses: getNQTVCs, AllNQNotNA, pkg 'RODBC' (indirect), pkg 'data.table'
# _____________________________________________________________________________
#' Returns dataframe with flags indicating survey completion
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
#' sidVec <- getSIDSet(c(1, 3, 4, 5), c(3, 4, 5, 6, 30), 1, 1)
getDiditXWave <- function(pWavVec,pSchVec){
  cat("Obtaining survey completion info...", "\n")
  require(data.table)
  #Get a node list of all eligible, so the output DF records participation
  #  for all eligible at any wave
  allNodes <- getEligNodes(pWavVec,pSchVec)
  # Construct output DF
  outDF <- data.frame(allNodes)
  names(outDF)[1] <- "SID"
  # Add cols to output DF for each wave, 1 if participant 'did it', else 0
  tvc <- allNQQNotNA(getNQTVCs(pWavVec,pSchVec)) # WATCH OUT, this is a
                                                 # data.table !!
  for (i in 1:length(pWavVec)){
    vWvN <- paste("wv",toString(pWavVec[i]),sep="") # new col name
    diditNodesi <- unique(tvc[DidIt == 1 & WID == pWavVec[i],SID])
    outDF[,i+1] <- ifelse(match(allNodes,diditNodesi,nomatch=0) ==0,0,1)
    names(outDF)[i+1] <- vWvN
  }
  cat("Done", "\n")
  return(outDF[order(outDF$SID),])
}



#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getEligNodes <<
# _____________________________________________________________________________
# Uses:
#    getSWNodes, pkgs 'RODBC' (indirect)
# _____________________________________________________________________________
#' Returns a vector of SIDs who were Short Survey eligible on any input wave
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec A numeric vectorof School IDs to be included in the analysis.
#' @return A (#eligible SIDs)-length numerical vector of SIDs.
#' @details Used by other functions in this package (e.g. getNetworkSet,
#'     makeTVTbl, )
#' @note This function just repeatedly calls 'getSWNodes' for each requested
#'     school & wave combination
#' @examples
#' # gets vector of SIDs for waves 1, 3, 4, and 5, schools 3-6 and 30 (one
#' # school district)
#' sidVec <- getSIDSet(c(1, 3, 4, 5), c(3, 4, 5, 6, 30), 1, 1)
getEligNodes <- function(pWavVec, pSchVec){
  # For efficiency, we create a list of
  # length w x s and create a vector of SIDs for each school/wave combination
  sidHolder <- vector("list", length(pWavVec)*length(pSchVec))
  for (i in seq_along(pWavVec)){
    for (j in seq_along(pSchVec)){
      # This call gets the SIDs of kids in school j, survey eligible in wave i
      sidHolder[[((i-1)*length(pSchVec))+j]] <- getSWNodes(pWav = pWavVec[i],
                                                           pSch = pSchVec[j])
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
#' Returns a dataframe with flags x wave for Short Survey eligibility
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
  cat("Obtaining survey eligibility info...", "\n")
  nodeList <- vector("list",length(pWavVec))
  allNodes <- numeric()
  for (i in 1:length(pWavVec)){
    wavNodes <- numeric()
    for (j in 1:length(pSchVec)){
      # execute only if school and wave combination were observed
      if (schWvExists(pWavVec[i],pSchVec[j])){
      cat("Pulling eligible SIDs from database W",
          pWavVec[i],",Sch",pSchVec[j], "\n")
      # We can't use 'getEligNodes', because we need the wave-
      # level eligibility to construct the output DF.
      nodesij <- getSWNodes(pWavVec[i],pSchVec[j],1,0)
      wavNodes <- append(wavNodes, nodesij, after=length(wavNodes))
      allNodes <- append(allNodes, nodesij, after=length(allNodes))
      }
      # ...otherwise go on to the next i,j pair.
    }
    # Each list element is the eligible nodes for that wave
    nodeList[[i]]<-wavNodes
  }

  # Construct Output Dataframe
  allNodesUnique <- unique(allNodes) # All Elig SIDs, all waves
  outDF <- data.frame(allNodesUnique)
  names(outDF)[1] <- "SID"

  # Will hold output
  # vWvNames <-character(length(pWavVec))
  cat("Saving elig info...", "\n")
  for (i in 1:length(pWavVec)){
    vWvN <- paste("wv",toString(pWavVec[i]),sep="") # new col name
    outDF[,i+1] <- ifelse(match(allNodesUnique,nodeList[[i]],nomatch=0) == 0,0,1)
    names(outDF)[i+1] <- vWvN
  }
  cat("Done", "\n")
  return(outDF[order(outDF$SID),])
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getMSRIDVect <<
# _____________________________________________________________________________
# Uses: getSWNodes
# _____________________________________________________________________________
#' Returns a list of *Row* ID vectors for each middle school requested
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
#' Returns a list of networks forming a longitudinal Analysis Set
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
#' netList <- getNetworkSet(pWavVec = c(1, 3, 4),
#'                          pSchVec = c(3, 4, 5, 6, 30),
#'                          pElig = 2, pDid = 2, pTyp = "BF",
#'                          pOut = "SP", pS0 = "S0")
#' # Create SAOM longitudinal network object 'myNet'
#' myNet <- sienaNet(netList[1:3], sparse=T)
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
#' Returns a vector of SIDs that are consistent with some Analysis Set
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
#' SIDVec <- getSIDSet(c(1, 3, 4), c(3, 4, 5, 6, 30))
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
#' Returns a longitudinal 'sienaNet' object directly from 'getNetworkSet'
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
#'     pSchVec = c(3, 4, 5, 6, 30),
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
#' Returns a data.table of a time varying behavior variable
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
#' @return The output table format is w+1 columns (w=#waves present in
#'     pCCTbl); first column is an ordered set of SIDs, columns 2:(w+1)
#'     are the scored (mean of items) value of the requested behaivor
#'     variable for the w waves.
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
#'     pCut = c(0, .5, 1, 5, 20), pMaxNA = 3)
makeTVTbl <- function(pTVTbl, pSIDs, pVar="X", pCut = c(0), pMaxNA = 1){
  require(dplyr, data.table)
  if (pVar == "X"){
    cat("Warning: No var name supplied; name defaults to X", "\n")
  }
  # Create a name for the binned (final form, scaled) variable:
  binVar <- paste(pVar,"B", sep = "") # Name of binned variable (e.g."ABB", ..)
  # Select only the SIDs from the input data.table to be used in the analysis
  tvDT <- data.table(filter(pTVTbl,SID %in% pSIDs))
  setkey(tvDT, SID, WID)

  # Get the vector (length >= 2) of variables implied by pVar
  items4TVC <- getTVCCols(pVar)

  # Calculate rowmeans
  tvDT[[pVar]] <- rowMeans(tvDT[, items4TVC, with = F],na.rm = T)
  # find rows for which the pVar needs to be set to NA. Only looks at the sub-
  #  dataframe with col names in 'items4TVCs'. naSums contains #NAs per row.
  naSums <- apply(tvDT[, items4TVC, with = F], 1, function(x) sum(is.na(x)))
  tvDT[[pVar]][which(naSums > pMaxNA)] <- NA

  # Bin 'pVar' according to the cut points in 'pCut'; place in 'binVar'
  tvDT[[binVar]] <- as.numeric(cut(tvDT[[pVar]], pCut, right=F,
                                     include.highest = T,
                                     labels = c(1:(length(pCut)-1))))


  # Build the DT for this TVC. The target is a length(subSIDs) x
  #  length(pWavVec)+1 DT. The last length(pWavVec) set of cols is then
  #  input as a matrix to 'sienaDependent' or 'varCovar'.
  # The whole DT is returned.
  tvDTLong <- data.table(tvDT$SID)
  tvDTLong$WID <- tvDT$WID
  tvDTLong[[pVar]] <- tvDT[[binVar]] # output name will be the original
  names(tvDTLong) <- c("SID", "WID", pVar)
    tvDTLong[[pVar]] <- tvDTLong[[pVar]] - 1 #makes scales 0-based
  tvDTWide <- reshape(tvDTLong,timevar = "WID", idvar = "SID",
                      direction = "wide")
  return(tvDTWide)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> schWvExists <<
# _____________________________________________________________________________
#' Returns logical TRUE if requested wave includes requested school
#'
#' This information is inherent in the study design; middle schools are only
#'     observed in Wave 1 (and high schools are not), whereas high schools
#'     are only observed in waves 3:5, 7:9, and 11.
#'
#' @param pWav The wave number of interest. Cannot be 2, 6, or 10.
#' @param pSch The school ID number of interest.
#' @return Logical variable; TRUE if the requested school and wave has any
#'     observations, otherwise FALSE.
#' @note: Used internally only
#'
schWvExists <- function(pWav,pSch){
  midSchools <- c(1:9,101,102)
  hiSchools <- c(10,20,30,70,80,110,120)
  # Initialize return value
  if (!(pWav %in% c(1,3:5,7:9,11))){
    stop("Error: Illegal wave requested. Must be 1, 3-5, 7-9, or 11.")
  }
  if ((pWav == 1 & pSch %in% midSchools) |
      (pWav > 2 & pSch %in% hiSchools)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> s0Assemble <<
# _____________________________________________________________________________
# Uses: s0Make
# Used by: getNetworkSet
# _____________________________________________________________________________
#' Returns a data.table with structural 0's for w1 middle schools
#'
#' This is a 'helper' function which, along with s0Make, adds rows of
#'     structural zeros for individuals from different middle schools
#'     who, however, will be going to the same high school the next
#'     year...and are thus considered part of the same network, for
#'     analysis purposes.
#'
#' @param pMSchVec A list of vectors, each containing the Wave 1 Analysis Set
#'     RowIDs (because Network Sets use RowIDs, as required by RSiena and
#'     network) for a specific middle school.
#' @return A data.table in edgelist format; cols are RowID (chooser),
#'     RowID (chosen), bff (value of the relationship, an integer >= 0),
#'     and WID (wave ID #)
#' @note This function should not normally be used stand-alone. To see how it
#'     is used in context, consult the source code for 'getNetworkSet'.
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
# _____________________________________________________________________________
# Used by: s0Assemble (which is called by getNetworkSet)
# Uses: pkg 'data.table'
# _____________________________________________________________________________
#' Returns a data.table of structural 0 edges between 2 schools
#'
#' This is a 'helper' function which, along with s0Assemble, adds rows of
#'     structural zeros for individuals from different middle schools
#'     who, however, will be going to the same high school the next
#'     year...and are thus considered part of the same network, for
#'     analysis purposes.
#'
#' @param pVec1 A vector of the RowIDs (note: not SIDs!) in middle school 1.
#' @param pVec2 A vector of the RowIDs in middle school 2.
#' @return A data.table in edgelist format; cols are RowID (chooser),
#'     RowID (chosen), bff (value of the relationship, an integer >= 0),
#'     and WID (wave ID #).
#' @details To make connections between two wave 1 middle schools impossible,
#'     all you need to do is create an edge with value '10 (structural zero,
#'     in RSiena terminology) between each pair of individuals in the two
#'     schools. This function accomplishes that in a simple but efficient way.
#'     The function 's0Assemble' takes all the data.table objects
#'     created here and assembles them into one big table to return to the
#'     calling function.
#' @note This function should not normally be used stand-alone. To see how it
#'     is used in context, consult the source code for 'getNetworkSet'.
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
# _____________________________________________________________________________
# Uses: RODBC
# _____________________________________________________________________________
#' Returns a data.table with DOBs for all SIDs input
#'
#' This function can be used to calculate an age variable
#'
#' @param pSIDVec A vector of SIDs for which DOB is wanted. These will
#'     normally be part of an 'Analysis Set', i.e. generated by
#'     'getNetworkSet' or 'getSIDSet'.
#' @return A data.table with two columns: SID and DOB. SID is integer,
#'     DOB is POSIXct/POSIxt
#' @details Makes a call to the database using RODBC
#' @examples
#' # Gets DOB for a couple of specific SIDs
#' aFewDOBs <- getDOB(c(10, 11, 12))
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
# _____________________________________________________________________________
# This function gets all SMaster records for these schools & waves.
# _____________________________________________________________________________
# Obtains dataframe of fixed NQ data (from the table 'SMaster' in the
# database 'PInf1'), for one " (pSchVec)"school cohort".
# To get only the SIDs for a given set of waves, you have to select the
# relevant SIDs first with other code, then select from this data frame.)
# _____________________________________________________________________________
# Uses: RODBC
# _____________________________________________________________________________
#' Returns a dataframe of fixed covariates for a set of input schools
#'
#' This is generally used to pull this data from the database, and
#'     subsequently subset it for a particular Analysis Set.
#'
#' @param pSchVec A vector of (integer) School IDs; any existing fixed
#'     covariate rows in the database where the participant is shown
#'     as currently in one of these schools will be included.
#' @return A dataframe that has the same format as the database table
#'     'PInf.dbo.SMaster'. Column names fairly accurately describe
#'     column contents, but one should consult the qq codebook for
#'     precise definition.
#' @details If a participant switches schools (which happens regularly),
#'     this function will only pull his/her data if his/her *current*
#'     school is in the input vector.
#' @examples
#' # Gets table of fixed covariats for all the schools in 1 district
#' fc <- getFixedCovs(c(3,4,5,6,30)
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
#______________________________________________________________________________
# Uses: pkg 'RODBC'
# Used by: getNetworkSet
# _____________________________________________________________________________
#' Returns a dataframe of network edges for input schools and waves
#'
#' This is generally used to pull this data from the database, and
#'     subsequently subset it for a particular Analysis Set. It is
#'     currently used by 'getNetworkSet'.
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec A vector of (integer) School IDs.
#' @return A dataframe that has the same format as the database table
#'     'PInf.dbo.SAffiliation'. Column names fairly accurately describe
#'     column contents, but one should consult the qq codebook for
#'     precise definition.
#' @details Chooser was asked for more information on an individual (if any)
#'     designated as Chooser's 'very best friend'. These variables are
#'     missing for any alters not so designated, or if Chooser did not
#'     make this designation at all.
#' @examples
#' # Gets edgelist (with addtional info on some alters) for individuals in
#' # waves 1, 3, and 4, schools 3:6 and 30 (one school district).
#' net <- getNetwork(c(1,3,4), c(3,4,5,6,30))
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
#______________________________________________________________________________
# Uses: pkgs 'RODBC', 'data.table'
# Used by: 'makeTVTbl'
# _____________________________________________________________________________
#' Returns a data.table of Short-Survey time-varying covariates
#'
#' This is generally used to pull this data from the database, and
#'     subsequently subset it for a particular Analysis Set. It is
#'     currently used by 'makeTVTbl'.
#'
#' @param pWavVec An ordered numeric vector of wave SIDs to be included in the
#'     analysis.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSchVec A vector of (integer) School IDs.
#' @return A dataframe that is a subset of the database table
#'     'PInf.dbo.SWave'., containing only the items asked on the 'Short Survey'.
#'     Column names fairly accurately describe
#'     column contents, but one should consult the qq codebook for
#'     precise definition.
#' @examples
#' # Gets a long-form data.table (each row represents an individual and a wave)
#' # of survey data from the 'Short Survey', for
#' # waves 1, 3, and 4, schools 3:6 and 30 (one school district).
#' tvcs <- getNQTVCs(c(1, 3, 4), c(3, 4, 5, 6, 30))
getNQTVCs <- function(pWavVec,pSchVec){
  require(RODBC,data.table)
  nqQuery <- paste ("SELECT SchID, SID, WID, ULied,UHit,UMean,USkip,UDamage,
                     ULate,UFam,","UOKids,ONames,OHit,OThreat,ONoTalk,OExclu,
                     OGossip,OEncourg,",
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
# _____________________________________________________________________________
# Uses: pkgs 'RODBC', 'data.table'
# Used by: getEligNodes, getEligXWave, getMSRIDVecs
# _____________________________________________________________________________
#' Returns a vector of SIDs for Short or Long-form survey-eligibles
#'
#' Output from this function is useful in creating subsets of data meeting
#'     survey-eligibility requirements.
#'
#' @param pWav A specific survey wave.
#'     Waves 2, 6, and 10 are summer waves; no Short Survey or Long Survey data
#'     were collected. Hence these waves should not ever be specified.
#' @param pSch A specific School ID.
#' @param pNQW  If 1, short-survey-eligible SIDs in this wave and school
#'     are returned (this would also include long-survey-eligibles as a
#'     subset).
#' @param pFQ If 1, only long-survey-eligible SIDs in this wave and school are
#'     returned.
#' @return An integer vector of SIDs
#' @examples
#' # Returns a vector of SIDs of individuals who were short-survey (NQ)
#' # eligible at wave 1, from school #3.
#' nodes <- getNQTVCs(pWav = 1, pSch = 3, pNQW = 1)
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
  tblOfNodesQuery <- paste("exec PInf1.dbo.StudentSIDInWave @CohortID = ",pCoh,",@PeriodID = ",
                           pPer,",@Wave = ",
                           pWav,",@NQWanted = ",pNQW,",@FQWanted = ",pFQW)
  eligNums <- sqlQuery(conn,tblOfNodesQuery)
  nodeQuery <- paste("SELECT SID,SchID From PInf1.dbo.NQEligTbl ")
  nodes_temp <- data.table(sqlQuery(conn,nodeQuery), key = "SID")
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
#______________________________________________________________________________
# Used by: getDiditXWave
#______________________________________________________________________________
#' Returns input dataframe of short survey questions with 'DitIt' flag.
#'
#' Used by 'getDididXWave' to create a wide-format dataframe indicating survey
#'     completion for each of a selected set of waves, by SID.
#'
#' @param pDF The dataframe produced by 'getNQTVCs'.
#' @return The long-form dataframe of time varying covariates (returned by
#'     getNQTVCs) with a column 'DidIt' added, which has the value one if
#'     any of the questions addressed below (lifetime substance use questions)
#'     were answered, 0 otherwise.
#' @details Used by 'getDidItXWave' to set values for the returned dataframe.
#' @examples
#' # Returns the table of time varying covariates produced by 'getNQTVCs' with
#' # the column 'DidIt' added, for the schools and waves in pSchVec and pWavVec.
#' tvc <- allNQQNotNA(getNQTVCs(pWavVec = c(1, 3, 4),
#'                              pSchVec = c(3, 4, 5, 6, 30)))
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
# _____________________________________________________________________________
#' Returns data.table of binary, up-only 'onset' variables x wave
#'
#' This is a stand-alone scoring function that takes a wide-form data.table
#'     or dataframe of 'lifetime use' variables for a (presumably consecutive)
#'     set of waves, and transforms them into up-only variables with the value
#'     1 when the participant first reports any lifetime use (zero prior to that).
#'
#' @param pInDT A data.table (or dataframe) containing SID plus a lifetime use
#'     (NA, 0, >0) variable for each of c-1 ordered time periods, where c is
#'     the number of columns in the DF.
#' @param pTHold The threshold (numerical value) for lifetime use; if the input
#'     use variable value is >= pTHold, then the event 'occurred'.
#' @return A data.table of the same dimension as the input data table, with rows
#'     transformed into up-only variables suitable for use in proportional
#'     hazard models, or onset/rate models (in RSiena)
#' @details The logic assigns 1's to the output variables for all waves after the
#'     first *known* positive instance (value >= pTHold) of the event. It also
#'     ignores instances where a participant reports the event at wave w, but
#'     then does not report any lifetime use at some wave w+k. Instead, the
#'     value of the onset variable will be 1 from the first instance of the event
#'     on. Also if there are NAs prior to an event being observed, these are all
#'     output as zeros.
#' @examples
#' # Returns a table of up-only variables which have the value 0 before the
#' # first known lifetime alcohol use (if any such times are available), and 1
#' # thereafter. The pTHold value of 1 means *any* use.
#' alcLife <- makeTVTbl(getNQTVCs(c(1,3,4),c(3,4,5,6,30)))
#' alcOnset <- createOnset(alcLife,pTHold = 1)
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
#______________________________________________________________________________
#' Returns data.table of *lagged* binary, up-only 'onset' variables
#'
#' This stand-alone scoring function takes a wide-form data.table
#'     or dataframe of 'lifetime use' variables for a (presumably consecutive)
#'     set of waves, and transforms them into up-only variables. Event onset is
#'     lagged one period. This data.table can be useful descriptively, because it
#'     shows the 'network neighborhood' of individuals in the wave before they
#'     onset to use. It is this neighborhood which presumably influenced onset
#'     (if there was any such influence).
#'
#' @param pInDT A data.table (or dataframe) containing SID plus a lifetime use
#'     (NA, 0, >0) variable for each of c-1 ordered time periods, where c is
#'     the number of columns in the DF.
#' @param pTHold The threshold (numerical value) for lifetime use; if the input
#'     use variable value is >= pTHold, then the event 'occurred'.
#' @return A data.table of the same dimension as the input data table, with rows
#'     transformed into up-only variables suitable for descriptive use.
#' @details The logic assigns 1's to the output variables for all waves after the
#'     first *known* positive instance (value >= pTHold) of the event. It also
#'     ignores instances where a participant reports the event at wave w, but
#'     then does not report any lifetime use at some wave w+k. Instead, the
#'     value of the onset variable will be 1 from the first instance of the event
#'     on. Also if there are NAs prior to an event being observed, these are all
#'     output as zeros. These considerations all still apply, even though the
#'     onset event is lagged one period.
#' @examples
#' # Returns a table of 1-period lagged up-only variables which have the value 0
#' # before the first known lifetime alcohol use (if any such times are
#' # available), and 1 thereafter. The pTHold value of 1 means *any* use.
#' alcLifeLag <- makeTVTbl(getNQTVCs(c(1, 3, 4), c(3, 4, 5, 6, 30)))
#' alcOnsetLag <- createOnset(alcLifeLag, pTHold = 1)
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
# _____________________________________________________________________________
#' Returns data.table of lag-1 onset variables with 3 different values
#'
#' This stand-alone scoring function takes a wide-form data.table
#'     or dataframe of 'lifetime use' variables for a (presumably consecutive)
#'     set of waves, and transforms them into variables with the value 0 prior
#'     to first known alcohol use, 1 if the individual is about to onset in the
#'     next wave, and 2 if he/she had already onset at some earlier wave. The
#'     onset event is lagged one wave (as for 'createNbhdOnset').
#' The variables returned by this function provide a particularly nice
#'     descriptive context for onset events--if you graph them in a network
#'     plot and assign, say, different node colors to each value, you can see
#'     who had or had not started (drinking, say) as of
#'     wave w, along with affiliations of everyone who was about to start in
#'     the next wave.
#'
#' @param pInDT A data.table (or dataframe) containing SID plus a lifetime use
#'     (NA, 0, >0) variable for each of c-1 ordered time periods, where c is
#'     the number of columns in the DF.
#' @param pTHold The threshold (numerical value) for lifetime use; if the input
#'     use variable value is >= pTHold, then the event 'occurred'.
#' @return A data.table of the same dimension as the input data table, with rows
#'     transformed into up-only variables suitable for descriptive use.
#' @details The logic assigns 1's to the output variables for all waves after the
#'     first *known* positive instance (value >= pTHold) of the event. It also
#'     ignores instances where a participant reports the event at wave w, but
#'     then does not report any lifetime use at some wave w+k. Instead, the
#'     value of the onset variable will be 1 from the first instance of the event
#'     on. Also if there are NAs prior to an event being observed, these are all
#'     output as zeros. These considerations all still apply, even though the
#'     onset event is lagged one period.
#' @examples
#' # Returns a table of 1-period lagged up-only variables which have the value 0
#' # before the first known lifetime alcohol use (if any such times are
#' # available), 1 on the occasion prior to onset, and 2 thereafter.
#' # The pTHold value of 1 means *any* use.
#' alcLifeLag <- makeTVTbl(getNQTVCs(c(1, 3, 4), c(3, 4, 5, 6, 30)))
#' alcOnsetLag <- createOnset(alcLifeLag, pTHold = 1
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

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getTVCCols <<
# _____________________________________________________________________________
#' Returns a vector of column (item) names for a scale or variable
#'
#' Used by 'getNQTVCs' to obtain the items necessary for a scale (or in some
#'     cases, just a single items, e.g. 'number of days drank, last 30'). It
#'     has the effect of generalizing 'getNQTVCs' so that the call to that
#'     function is compact, and does not require the user to go find the
#'     names of the items in any given scale.
#'
#' @param pVar A string giving one of the abbreviations for a variable
#'     understood by the function. See below for a list of possible values.
#' @return A vector of strings corresponding to the names of the items
#'     comprising a variable in the database table 'PInf1.dbo.SWave'
#' @note Choices for pVar are:
#'     AB (Antisocial behavior),
#'     OV (Others victimization of you),
#'     YV (You victimizing others),
#'     T3 (Tobacco, freq of use in last 30 days),
#'     E3 (E-tobacco, freq of use in last 30 days),
#'     C3 (Chewing Tobacco, freq of use in last 30 days),
#'     A3 (Alcohol, freq of use in last 30 days),
#'     B3 (Binge drinking, freq in last 30 days),
#'     M3 (Marijuana use, freq in last 30 days),
#'     AL (Alcohol, freq of use, lifetime)
#' @examples
#' # Internal function, not normally available to users. See 'makeTVTbl'
#' # source code to see how it is used.
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

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeCCVec <<
# ____________________________________________________________________________
#' Returns a list of vectors representing RSiena composition change.
#'
#' The output of this function may be input directly to the RSiena function
#'     'sienaCompositionChange'.
#'
#' @param pElig  An n x 1+w dataframe, with 1 row for every SID who was
#'     *survey-eligible* for any of the waves of interest, and 1 col for
#'     each wave, containing 1 if the SID was survey-eligible that wave,
#'     and 0 if not (col 1 is SID).
#' @return A list with n elements (one for each row of the inut DF). Each
#'     element is a vector of pairs of arrival and departure times from
#'     the network, as explained in the RSiena manual under 'method of
#'     changing composition'.
#' @details The input dataframe must be the object returned by
#'     'getEligXWave', or the equivalent.
#' @examples
#' # Returns a list of composition change vectors for the SIDs
#' # in waves 1, 3, and 4, and schools 3, 4, 5, 6, and 30.
#' ccDF <- makeCCVec(getEligXWave(pWavVec = c(1,3,4),
#'     pSchVec = c(3,4,5,6,30)))
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

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> netvtxAttr <<
# _____________________________________________________________________________
#' Returns a dataframe DF with col.2 a suitable 'network' node attribute.
#'
#' Standalone function for creating pkg 'network' node (vertex) attribute.
#'
#' @param pAttrMasterDF A two-column dataframe with SID and the values of
#'     some numerical variable 'x' as the columns. It should include all
#'     SIDs for some school and wave range (e.g. as generated by 'getSWNodes')
#'     for which there are data (if no data, attribute will be 'NA'). The SIDs
#'     actually used are selected from the next parameter.
#' @param pSIDVec An ordered vector of SIDs exactly matching the 'node set'
#'     for the network of interest; e.g. if the network has n nodes, the length
#'     of the returned dataframe will be n.
#' @param pNetwork The 'network' object to assign the vertex attribute to. This
#'     is not actually used to do the assignment, but rather, for some
#'     consistency checking.
#' @return A dataframe with two columns: SID (integer) and the associated values
#'     of some variable (numerical).
#' @examples
#' # AL134DF is a dataframe as output by 'createOnset'.
#' # Because this analysis is descriptive, this code creates a vertex
#' # attribute for just one wave, and in the 2nd statement, assigns it as
#' # a 'network' vertex attribute, naming it 'AlcOns'
#' ALDF.w1 <- netVtxAttr(AL134DF[,c("SID","al.1")],ds.winsSID1,ds.winsOBF1.no)
#' ds.winsOBF1.no %v% "AlcOns" <- ALDF.w1$al.1
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

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> zero30dSU <<
# _____________________________________________________________________________
# Used by: makeTVTbl
# _____________________________________________________________________________
#' Adjusts 30 day SU variables to zero if lifetime use is zero
#'
#' @param pDF A dataframe in the format returned by 'getNQTVCs', i.e. an
#'     image of the database table 'SWave' that includes at least the
#'     variables for lifetime and 30 day substance use included in the
#'     Short Survey.
#' @return A dataframe in the same format as the input DF, with 30 day
#'     substance use variables recoded to 0 if no lifetime use of the
#'     corresponding substance has been reported concurrently.
#' @details Survey skip logic skips over 30 day SU questions if no lifetime
#'     use of the substance is reported concurrently. This recode allows
#'     the 30 day variables to be used in analyses under such circumstances,
#'     instead of being missing.
#' @note This function is a helper function for 'makeTVTbl', and is not
#'     normally called directly by users
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

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getNameAsString <<
# _____________________________________________________________________________
#' Returns the name of a variable as a string.
#'
#' This function just removes the need to look up how to do this (fairly
#'     common) operation, which I seem to have to do all the time, because
#'     I can't remember the (little used) syntax.
#' @param pVarName A variable name (no quotes or anything)
#' @return The variable name as a string
#' @examples
#' # Returns c("myVariable")
#' vStr <- getNameAsString(myVariable)
getNameAsString <- function(pVarName){
  deparse(substitute(pVarName))
}
