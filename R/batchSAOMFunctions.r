#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getDistIDInfo <<
# _____________________________________________________________________________
#' Returns district-related school IDs
#'
#' Takes the district name and returns all school IDs associated with the
#' district
#' @param SchoolDist character: The name of school district. Options are:
#'        Douglas, Sutherlin, Creswell, Siuslaw, Woodburn,
#'        Junction, Willamette, SmallSchools.
#' @param rlpType character: The type of relationship. Options are: BF = best
#'              friend, FT = spent free time with.
#' @return List:
#'         Element 1--data.table: k rows, one for each of the k schools (middle + HS)
#'          in the district, and 1 numerical column 'SchID' containing each
#'          school ID.
#'         Element 2--
#' @export
getDistAnalysisObjects = function(SchoolDist,rlpType){
#' @import dplyr
#' @import data.table
#
# Open the connection to the SQL databases
  conn <- RODBC::odbcConnect(dsn="PInf1")

# Pull the school district ID (first obtain the high school(s) ID)
  highschoolID <- data.table(RODBC::sqlQuery(conn,"SELECT * From PInf1.dbo.School")) %>%
   filter(!grepl("Middle School",SchName)) %>%
   filter(grepl(paste0(SchoolDist),SchName)) %>%
   select(SchID)

  filter(SchID2==highschoolID[[1]]) %>%
   select(SchID1) %>%
   mutate(SchID=SchID1) %>%
   select(SchID)

# Bind together the school IDs
  schoolInfo<-bind_rows(midschoolID, highschoolID)
  # ('assign' creates a variable with a particular name...)
   assign(paste0(SchoolDist,"_schoolInfo"),schoolInfo)
  # ('do.call' allows a function to be called by a function name passed in
  #   or constructed in character string form, or a list of similarly-
  #   constructed parameters to pass to the function)
   try(do.call(
         save,
         list(paste0(SchoolDist,"_schoolInfo"),
         file = paste0(workingDir,
         SchoolDist,
         "_Binge_",
         rlpType,
         "_schoolInfo.RDS",
         sep="")))
       )

         # Close the connection to the SQL databases
   rm(conn)

   # NEXT: decide what to save and how, and what options to provide for what is
   #       returned. xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

         # Get the network information for the school district for the waves of interest
   netSetSP <- getNetworkSet(pWavVec = c(WavesofInt),
                             pSchVec = c(schoolInfo$SchID),
                             pElig = elig,
                             pDid = did,
                             pTyp = rlpType,
                             pOut = "SP",
                             pS0 = "")

         # Create a network object for possible visualization
   netSetNT <- getNetworkSet(pWavVec = c(WavesofInt), pSchVec = c(schoolInfo$SchID),
                             pElig = elig,
                             pDid = did,
                             pTyp = rlpType,
                             pOut = "NT")

         # Who is eligible?
   netSetSID.elig <- getEligNodes(pWavVec = c(WavesofInt), pSchVec = c(schoolInfo$SchID))

         # Who did the surveys?
   netSetSID.did1 <- netSetSP[[length(WavesofInt)+1]]

         # Who is eligible and also completed the surveys?
   eligXWave  <- getEligXWave(pWavVec = c(WavesofInt), pSchVec = c(schoolInfo$SchID))
   diditXWave <- getDiditXWave(pWavVec = c(WavesofInt), pSchVec = c(schoolInfo$SchID))
   eligXWave.did1 <- eligXWave %>%
     filter(SID %in% netSetSID.did1)

   assign(paste0(SchoolDist,"_eligXWave.did1"), eligXWave.did1)
   assign(paste0(SchoolDist,"_netSetSID.did1"), netSetSID.did1)
   assign(paste0(SchoolDist,"_netSetSP"), netSetSP)
   assign(paste0(SchoolDist,"_netSetNT"), netSetNT)

   try(do.call(
         save,
         list(paste0(SchoolDist,"_eligXWave.did1"),
              file = paste0(workingDir,
              SchoolDist,
              "_Binge_",
              rlpType,
              "_eligXWave.did1.RDS",
              sep="")))
       )
   try(do.call(
         save,
         list(paste0(SchoolDist,"_netSetSID.did1"),
              file = paste0(workingDir,
              SchoolDist,
              "_Binge_",
              rlpType,
              "_netSetSID.did1.RDS",
              sep="")))
       )
   try(do.call(
         save,
         list(paste0(SchoolDist,"_netSetSP"),
         file = paste0(workingDir,
         SchoolDist,
         "_Binge_",
         rlpType,
         "_netSetSP.RDS",
         sep="")))
       )
   try(do.call(
         save,
         list(paste0(SchoolDist,"_netSetNT"),
         file = paste0(workingDir,
         SchoolDist,
         "_Binge_",
         rlpType,
         "_netSetNT.RDS",
         sep="")))
       )

       }
