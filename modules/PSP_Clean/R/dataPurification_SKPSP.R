dataPurification_SKPSP <- function(SADataRaw,
                                   plotHeaderRaw,
                                   measureHeaderRaw,
                                   treeDataRaw) {
  browser()
  # range(SADataRaw$COUNTED_AGE) # NA NA
  # range(SADataRaw$TOTAL_AGE) # NA NA
  # unique(SADataRaw$TREE_STATUS)
  header_SA <- SADataRaw[!is.na(TOTAL_AGE) & TREE_STATUS == 1, ]
  header_SA[, baseYear := min(YEAR), by = PLOT_ID]
  header_SA[, treeAge := TOTAL_AGE - (YEAR - baseYear)]
  header_SA_Dom <-
    header_SA[CROWN_CLASS == 1, ] # the stand age first determined by dominant trees
  header_SA_Dom[, NofTrees := length(CROWN_CLASS), by  = PLOT_ID]
  # unique(SADataRawDomSA$NofTrees) # 1 2 3 4 5
  # stand age must determined by using at least 2 trees
  header_SA_Dom <- header_SA_Dom[NofTrees != 1, ]
  # SADataRawDomSA[, treeAgeDif:=max(treeAge)-min(treeAge), by = PLOT_ID]
  # range(SADataRawDomSA$treeAgeDif) # 0 44
  # mean(SADataRawDomSA$treeAgeDif) # 7.03
  header_SA_Dom[, baseSA := as.integer(mean(treeAge)), by = PLOT_ID]
  header_SA_Dom <-
    unique(header_SA_Dom[, .(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID")
  # for the other plots determine SA using codominant trees
  header_SA_CoDom <- header_SA[CROWN_CLASS == 2, ]

  header_SA_CoDom <-
    header_SA_CoDom[!(PLOT_ID %in% unique(header_SA_Dom$PLOT_ID)), ]
  header_SA_CoDom[, NofTrees := length(CROWN_CLASS), by  = PLOT_ID]
  # unique(SADataRawCodomSA$NofTrees)
  header_SA_CoDom <- header_SA_CoDom[NofTrees != 1, ]
  header_SA_CoDom[, baseSA := as.integer(mean(treeAge)), by = PLOT_ID]
  header_SA_CoDom <-
    unique(header_SA_CoDom[, .(PLOT_ID, baseYear, baseSA)], by = "PLOT_ID")
  headData_SA <- rbind(header_SA_Dom, header_SA_CoDom)

  headData_loca <-
    plotHeaderRaw[PLOT_ID %in% unique(headData_SA$PLOT_ID), ][, .(PLOT_ID, Z13nad83_e, Z13nad83_n, Zone = 13)]
  names(headData_loca)[2:3] <- c("Easting", "Northing")
  headData_SALoca <-
    setkey(headData_SA, PLOT_ID)[setkey(headData_loca, PLOT_ID),
                                 nomatch = 0]
  headData_PS <-
    measureHeaderRaw[PLOT_ID %in% unique(headData_SALoca$PLOT_ID), ][, .(PLOT_ID, PLOT_SIZE)][!is.na(PLOT_SIZE), ]
  headData_PS <- unique(headData_PS, by = "PLOT_ID")
  setnames(headData_PS, "PLOT_SIZE", "PlotSize")
  headData <-
    headData_SALoca[setkey(headData_PS, PLOT_ID), nomatch = 0]


  # for tree data
  treeDataRaw <- treeDataRaw[PLOT_ID %in% headData$PLOT_ID, ][, .(
    PLOT_ID,
    TREE_NO,
    YEAR,
    SPECIES,
    DBH,
    HEIGHT,
    TREE_STATUS,
    CONDITION_CODE1,
    CONDITION_CODE2,
    CONDITION_CODE3,
    MORTALITY
  )]

  # check the living trees
  # 1. by tree status codes
  #     1 1 Live
  #     2 2 Declining
  #     3 3 Dead or dying
  #     4 4 Loose bark snag
  #     5 5 Clean snag
  #     6 6 Snag with broken top
  #     7 7 Decomposed snag.
  #     8 8 Down snag
  #     9 9 Stump
  treeData <- treeDataRaw[is.na(TREE_STATUS) | # conservtively
                            TREE_STATUS == 0 |
                            TREE_STATUS == 1 |
                            TREE_STATUS == 2, ]
  # 2. by mortality codes
  #     Null 0
  #     Natural or Undetermined 1
  #     Disease 2
  #     Insect 3
  #     Human 4
  #     Wind 5
  #     Snow 6
  #     Other Trees 7
  #     Hail or Ice Storm 8
  treeData <- treeData[MORTALITY == 0 |
                         is.na(MORTALITY), ]
  # check the trees with both status and mortality are NA
  # unique(treeDataRaw[is.na(TREE_STATUS) & is.na(MORTALITY), ]$CONDITIONCODE1)
  # NULL
  treeData <- treeData[!is.na(DBH) & DBH != 0, ]
  treeData <-
    treeData[, .(PLOT_ID, OrigPlotID2 = NA, YEAR, TREE_NO, SPECIES,  DBH, HEIGHT)]
  names(treeData) <- c(
      "OrigPlotID1",
      "OrigPlotID2",
      "MeasureYear",
      "TreeNumber",
      "Species",
      "DBH",
      "Height"
    )
  setnames(headData, "PLOT_ID", "OrigPlotID1")
  measureidtable <-
    unique(treeData[, .(OrigPlotID1, MeasureYear)], by = c("OrigPlotID1", "MeasureYear"))
  measureidtable[, MeasureID := paste("SKPSP_", row.names(measureidtable), sep = "")]
  measureidtable <-
    measureidtable[, .(MeasureID, OrigPlotID1, MeasureYear)]
  treeData <- setkey(measureidtable, OrigPlotID1, MeasureYear)[setkey(treeData, OrigPlotID1, MeasureYear),nomatch = 0]
  treeData <- treeData[, .(MeasureID,
                 OrigPlotID1,
                 OrigPlotID2,
                 MeasureYear,
                 TreeNumber,
                 Species,
                 DBH,
                 Height)]
  headData <-
    setkey(measureidtable, OrigPlotID1)[setkey(headData, OrigPlotID1),
                                        nomatch = 0]
  headData <- headData[, .(
      MeasureID,
      OrigPlotID1,
      MeasureYear,
      Longitude = NA,
      Latitude = NA,
      Zone,
      Easting,
      Northing,
      PlotSize,
      baseYear,
      baseSA
    )]
  return(list(plotHeaderData = headData, treeData = treeData))
}
