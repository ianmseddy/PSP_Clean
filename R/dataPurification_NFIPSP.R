dataPurification_NFIPSP <- function(lgptreeRaw,
                                    lgpHeaderRaw,
                                    approxLocation) {
  # start from tree data to obtain plot infor
  lgptreeRaw[, year := as.numeric(substr(lgptreeRaw$meas_date, 1, 4))]
  lgpHeaderRaw[, year := as.numeric(substr(lgpHeaderRaw$meas_date, 1, 4))]
  lgpHeader <-
    lgpHeaderRaw[nfi_plot %in% unique(lgptreeRaw$nfi_plot), ][, .(nfi_plot, year, meas_plot_size, site_age)]
  approxLocation <-
    approxLocation[, .(nfi_plot, longitude, latitude, elevation)] %>%
    unique(., by = "nfi_plot")
  lgpHeader <- setkey(lgpHeader, nfi_plot)[setkey(approxLocation, nfi_plot), nomatch = 0]
  # remove the plots without SA and location infor
  lgpHeader <- lgpHeader[!is.na(site_age), ][!is.na(longitude), ][!is.na(latitude), ]
  treeData <- lgptreeRaw[, .(nfi_plot, year, tree_num, lgtree_genus, lgtree_species,
                             lgtree_status, dbh, height)][nfi_plot %in% unique(lgpHeader$nfi_plot), ]
  treeData <- treeData[lgtree_status != "DS" & lgtree_status != "M", ][, lgtree_status := NULL]
  setnames(treeData, c("nfi_plot", "year", "tree_num","lgtree_genus", "lgtree_species", "dbh", "height"),
    c("OrigPlotID1", "MeasureYear", "TreeNumber", "Genus", "Species", "DBH", "Height"))

  names(lgpHeader) <- c("OrigPlotID1", "baseYear", "PlotSize", "baseSA", "Longitude", "Latitude", "Elevation")
  lgpHeader <- unique(lgpHeader, by = "OrigPlotID1")
  newheader <- unique(treeData[, .(OrigPlotID1, MeasureYear)], by = c("OrigPlotID1", "MeasureYear"))
  newheader[, MeasureID := paste("NFIPSP_", row.names(newheader), sep = "")]
  lgpHeader <- setkey(lgpHeader, OrigPlotID1)[setkey(newheader, OrigPlotID1), nomatch = 0]
  treeData <- setkey(treeData, OrigPlotID1)[setkey(newheader[, .(OrigPlotID1, MeasureID)], OrigPlotID1), nomatch = 0]
  treeData <- treeData[, .(MeasureID, OrigPlotID1, OrigPlotID2 = NA, MeasureYear,
                           TreeNumber, Genus, Species, DBH, Height)]
  lgpHeader <- lgpHeader[, .(MeasureID, OrigPlotID1, MeasureYear, Longitude, Latitude, Zone = NA,
                             Easting = NA, Northing = NA, Elevation, PlotSize, baseYear, baseSA)]

  treeData <- standardizeSpeciesNames(treeData, forestInventorySource = "NFIPSP") #Need to add to pemisc

  return(list(plotHeaderData = lgpHeader, treeData = treeData))
}
