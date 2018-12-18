dataPurification_SKTSP_Mistic <- function(compiledPlotData,
                                          compiledTreeData) {
  browser()
  options(scipen = 999) # avoid scientific notation

  headData <- compiledPlotData[, .(ID_FOR, CRZ_ZONE, CRZNORTH, CRZ_EAST, PLOTNUM, YEAR, PSIZE, P_AGECLS)]

  headData <- unique(headData, by = c("ID_FOR", "PLOTNUM"))
  headData[, PlotSize := sum(PSIZE), by = ID_FOR]
  headData <- unique(headData, by = "ID_FOR")[, ':='(PLOTNUM = NULL, PSIZE = NULL)]
  headData[, MeasureID := paste("SKTSP_Mistik_", row.names(headData), sep = "")]
  setnames(headData,c("CRZNORTH", "CRZ_EAST", "CRZ_ZONE", "YEAR", "P_AGECLS"),
           c("Northing", "Easting", "Zone", "MeasureYear", "SA"))

  treeData <- compiledTreeData[, .(ID_FOR, TREENO, SPECIES, DBH, HEIGHT, CONDCOD1, CONDCOD2, CONDCOD3)]
  treeData <- treeData[ID_FOR %in% unique(headData$ID_FOR),]
  # remove dead trees
  treeData <- treeData[CONDCOD1 != "DE", ]
  treeData <- treeData[CONDCOD2 != "DE", ]
  treeData <- treeData[CONDCOD3 != "DE", ]
  set(treeData, , c("CONDCOD1", "CONDCOD2", "CONDCOD3"), NULL)
  treeData <- setkey(headData[, .(MeasureID, ID_FOR, MeasureYear)],
                     ID_FOR)[setkey(treeData, ID_FOR), nomatch = 0]


  treeData <- treeData[, .(MeasureID, OrigPlotID1 = ID_FOR, OrigPlotID2 = NA, MeasureYear,
                           TreeNumber = TREENO, Species = SPECIES, DBH, Height = HEIGHT)]

  headData <- headData[, .(MeasureID, OrigPlotID1 = ID_FOR, MeasureYear, Longitude = NA,
                           Latitude = NA, Zone, Easting = Easting * 10000, Northing = Northing * 10000,
                           PlotSize, baseYear = MeasureYear, baseSA = SA)]

  return(list(plotHeaderData = headData, treeData = treeData))
}
