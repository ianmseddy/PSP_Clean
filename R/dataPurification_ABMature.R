dataPurification_ABMature <- function(treeDataRaw, plotHeaderDataRaw) {
  setnames(
      treeDataRaw,
      c("Groupnumber", "Plotnumber", "Treenumber"),
      c("GroupNumber", "PlotNumber", "TreeNumber")
    )
    treeDataRaw[, GroupNumber := as.character(GroupNumber)]
    setnames(plotHeaderDataRaw,
             c("PLOT..", "TYPE", "PLOTS", "DEC...LONG","DEC...LAT","Plot.Size.m2","Stand.origin", "Managed."),
      c("GroupNumber", "Type" ,"NofSubplot", "Longitude", "Latitude", "PlotSize", "StandOrigin", "Managed")
    )
    headerData <- plotHeaderDataRaw[, .(GroupNumber, Type, NofSubplot, Longitude, Latitude, Easting,
                                        Northing, Meridian, Elevation, PlotSize, StandOrigin, Managed)]
    headerData[, GroupNumber := as.character(GroupNumber)]
    headerData[Meridian == "UTM 117(NAD83)", Zone := 11]
    headerData[Meridian == "UTM 111(NAD83)", Zone := 12]

    # generate head data for each plot, unmanaged, SA available, location available
    headerData_SA <- treeDataRaw[TreeNumber == 0 & (!is.na(DBHage) | !is.na(Stumpage)), ]
    SADiff <- as.integer(mean(headerData_SA[!is.na(DBHage) &
                                      !is.na(Stumpage)]$Stumpage - headerData_SA[!is.na(DBHage) &
                                                                                   !is.na(Stumpage)]$DBHage))
    headerData_SA <- headerData_SA[!is.na(DBHage) & is.na(Stumpage),
                                   Stumpage := DBHage + SADiff][, .(GroupNumber, PlotNumber, MeasureYear, Stumpage)]
    headerData_SA[, firstMeasureYear := min(MeasureYear),
                  by = c("GroupNumber")][, treeAge := Stumpage - MeasureYear + firstMeasureYear]
    headerData_SA <-  headerData_SA[, .(baseYear = mean(firstMeasureYear),
                        baseSA = round(mean(treeAge))), by = c("GroupNumber")]


    # select PSPs

    # select the plots with locations
    headerData <- headerData[(Longitude != 0 & Latitude != 0) |(Northing != 0 & Easting != 0 & !is.na(Zone)), ]
    # select plots unmanaged
    headerData <- headerData[Managed == "No", ]
    # joining the SA information
    headerData <- setkey(headerData, GroupNumber)[setkey(headerData_SA, GroupNumber),
                                      nomatch  = 0][, ':='(Type = NULL, NofSubplot = NULL,
                                                           StandOrigin = NULL, Managed = NULL)]
    headerData <- unique(headerData, by = "GroupNumber")

    treeData <- treeDataRaw[TreeNumber != 0, ][
      , .(GroupNumber, PlotNumber, MeasureYear, TreeNumber, Species, DBH,
          Height, Conditioncode1, Conditioncode2, Conditioncode3, Treeplotsize)]
    # remove DBH is not available
    treeData <- treeData[!is.na(DBH) & DBH != 0, ]

    treeData <- treeData[GroupNumber %in% headerData$GroupNumber, ]
    tempPlotID <- unique(treeData[, .(GroupNumber, PlotNumber, MeasureYear)],
             by = c("GroupNumber", "PlotNumber", "MeasureYear"))
    tempPlotID[, MeasureID := as.numeric(row.names(tempPlotID))]
    tempPlotID <- tempPlotID[, .(MeasureID, GroupNumber, PlotNumber, MeasureYear)]
    setkey(tempPlotID, GroupNumber, PlotNumber, MeasureYear)
    treeData <- tempPlotID[setkey(treeData, GroupNumber, PlotNumber, MeasureYear), nomatch = 0]

    # treeData condition check
    treeData <-  treeData[Conditioncode1 != 25 & Conditioncode1 != 61 &
                 Conditioncode1 != 79 & Conditioncode1 != 80, ]

    treeData <- treeData[is.na(Conditioncode2) |
                           (Conditioncode2 != 25 & Conditioncode2 != 61 & Conditioncode2 != 79 & Conditioncode2 != 80), ]
    treeData <- treeData[is.na(Conditioncode3) |
                           (Conditioncode3 != 25 & Conditioncode3 != 61 & Conditioncode2 != 79 & Conditioncode2 != 80), ]
    treeData[, ':='(Conditioncode1 = NULL, Conditioncode2 = NULL, Conditioncode3 = NULL)]

    # check the plot size
    treeData[, plotsizetime := as.numeric(length(unique(Treeplotsize))), by = c("MeasureID")]

    if (nrow(treeData[plotsizetime == 2, ]) > 0) {
      plotids <- unique(treeData[plotsizetime == 2, ]$MeasureID)
      for (plotid in plotids) {
        groupnumber <- unique(treeData[MeasureID == plotid, ]$GroupNumber)
        plotsize <- as.numeric(headerData[GroupNumber == groupnumber, ]$PlotSize) # obtain plot size from headData
        treeData[MeasureID == plotid, Treeplotsize := plotsize]
      }
    }

    setnames(treeData, "Treeplotsize", "PlotSize")
    measureiddata <- setkey(unique(treeData[, .(MeasureID, GroupNumber, PlotSize, MeasureYear)],
                                   by = "MeasureID"), GroupNumber)
    headerData[, PlotSize := NULL]
    headerData <- measureiddata[setkey(headerData, GroupNumber), nomatch  = 0][, Longitude := -(Longitude)]
    treeData[, ':='(DBH = DBH / 10, PlotSize = NULL, plotsizetime = NULL)]

    headerData[, MeasureID := paste("ABPSPMature_", MeasureID, sep = "")]
    setnames(headerData, "GroupNumber", "OrigPlotID1")


    headerData <- headerData[, .(MeasureID, OrigPlotID1, MeasureYear, Longitude, Latitude, Zone, Easting,
                                 Northing = as.numeric(Northing), PlotSize = PlotSize / 10000, baseYear, baseSA)]
    treeData[, MeasureID := paste("ABPSPMature_", MeasureID, sep = "")]
    setnames(treeData, c("GroupNumber", "PlotNumber"), c("OrigPlotID1", "OrigPlotID2"))

    treeData <- standardizeSpeciesNames(treeData, forestInventorySource = "ABPSP") #Need to add to pemisc

    #fix plots with treeNumber discontinuities -- plot 211 and 49
    #OrigPlotID1 211 has new numbers between 1976-1986
    headerData[OrigPlotID1 == 211, baseYear := 1986]
    headerData <- headerData[!OrigPlotID1 == 211 | MeasureYear > 1985]
    treeData <- treeData[OrigPlotID1 == 211, baseYear := 1986]
    treeData <- treeData[!OrigPlotID1 == 211 | MeasureYear > 1985]

    #OrigPlotID 49 has new numbers between 1961-1968
    headerData[OrigPlotID1 == 49, baseYear := 1968]
    headerData <- headerData[!OrigPlotID1 == 49 | MeasureYear > 1967]
    treeData <- treeData[OrigPlotID1 == 49, baseYear := 1968]
    treeData <- treeData[!OrigPlotID1 == 49 | MeasureYear > 1967]

    return(list(plotHeaderData = headerData,
                treeData = treeData))
  }
