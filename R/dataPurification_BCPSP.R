dataPurification_BCPSP = function(treeDataRaw, plotHeaderDataRaw, damageAgentCodes, codesToExclude) {

  headerData <- plotHeaderDataRaw[tot_stand_age != -99,][
    ,':='(utmtimes = length(unique(utm_zone)),
          eastingtimes = length(unique(utm_easting)),
          northingtimes = length(unique(utm_northing)),
          SAtimes = length(unique(tot_stand_age)),
          plotsizetimes = length(unique(area_pm)),
          standorigtimes = length(unique(stnd_org)),
          treatmenttimes = length(unique(treatment))),
    by = SAMP_ID]

  # select the natural originated and untreated
  headerData <- headerData[stnd_org == "N" | stnd_org == "F",]
  #this removes the repeated DBH utilization limits
  headerData[,treatmenttimes := length(unique(treatment)), by = c("SAMP_ID", "meas_yr")]
  # unique(headDataRaw$treatmenttimes) # 1 2
  headerData <- headerData[treatmenttimes == 1 & treatment == "UNTREATED",
                           .(SAMP_ID, utm_zone, utm_easting,
                             utm_northing, Elevation = elev, area_pm, tot_stand_age,
                             meas_yr)]

  headerData[, baseYear := min(meas_yr),
             by = SAMP_ID]
  headerData[, baseSA := as.integer(tot_stand_age-(meas_yr-baseYear))]
  # get the plots with locations
  headerData <- headerData[!is.na(utm_zone) & !is.na(utm_easting) & !is.na(utm_northing),]
  # get plots with plot size

  headerData <- headerData[!is.na(area_pm),][,':='(tot_stand_age = NULL, meas_yr = NULL)]
  names(headerData)[c(1:4, 6)] <- c("OrigPlotID1", "Zone", "Easting",
                              "Northing", "PlotSize")
  headerData <- unique(headerData, by = c("OrigPlotID1"))

  # for tree data
  if (is.null(treeDataRaw$OrigPlotID1)) {
    #SAMP_ID is lower case in new BC dataset
    setnames(treeDataRaw, c("SAMP_ID", "plot_no"),
             c("OrigPlotID1", "OrigPlotID2"))
  }
  treeData <- treeDataRaw[OrigPlotID1 %in% unique(headerData$OrigPlotID1),]
  # range(treeData$meas_yr) # 1926 2014
  # unique(treeData$Plotnumber) # 01 02 03
  
  #filtering damage codes - must remove tree from every measurement
  #there are unlikely to ever be trees with damage codes 3-5

  if (!is.null(codesToExclude)) {
    damAgents <- damageAgentCodes[`Damage Agent Code` %in% codesToExclude,]$Definition
    message(paste("removing trees marked as damaged by: "), paste(damAgents, collapse = ", "))
    bad1 <- treeData[dam_1 %in% codesToExclude, .(OrigPlotID1, OrigPlotID2, tree_no, meas_yr)]
    bad2 <- treeData[dam_2 %in% codesToExclude, .(OrigPlotID1, OrigPlotID2, tree_no, meas_yr)]
    bad3 <- treeData[dam_3 %in% codesToExclude, .(OrigPlotID1, OrigPlotID2, tree_no, meas_yr)]
    bad4 <- treeData[dam_4 %in% codesToExclude, .(OrigPlotID1, OrigPlotID2, tree_no, meas_yr)]
    bad5 <- treeData[dam_5 %in% codesToExclude, .(OrigPlotID1, OrigPlotID2, tree_no, meas_yr)]
    allBad <- rbind(bad1, bad2, bad3, bad4, bad5)
    allBad <- allBad[, .N, .(OrigPlotID1, OrigPlotID2, tree_no)]
    message(paste("removing", nrow(allBad), "trees due to damage"))
    
    #removing the tree from all measurements means it won't be confused for mortality - 
    #but it makes the module less reliable for other PSP uses... 
    treeData <- treeData[!allBad, on = c("OrigPlotID1", "OrigPlotID2", "tree_no")]
  }
  
  treeData <- treeData[sub_plot_tree == "N",][, OrigPlotID2 := as.numeric(OrigPlotID2)]
  treeData <- treeData[tree_cls == 1 | tree_cls == 2,
                       .(OrigPlotID1, OrigPlotID2, meas_yr, tree_no, species, dbh, height)]
  
  # unique(treeData$ld)
  names(treeData)[3:7] <- c("MeasureYear", "TreeNumber", "Species", "DBH", "Height")

  measreidtable <- unique(treeData[,.(OrigPlotID1, OrigPlotID2, MeasureYear)],
                          by = c("OrigPlotID1", "OrigPlotID2", "MeasureYear"))
  measreidtable[,MeasureID:= paste("BCPSP_",row.names(measreidtable), sep = "")]
  measreidtable <- measreidtable[,.(MeasureID, OrigPlotID1, OrigPlotID2, MeasureYear)]
  headerData <- setkey(measreidtable, OrigPlotID1)[setkey(headerData, OrigPlotID1), nomatch = 0]
  set(headerData, NULL,"OrigPlotID2", NULL)
  headerData <- headerData[,.(MeasureID, OrigPlotID1, MeasureYear, Longitude = NA,
                              Latitude = NA, Zone, Easting, Northing, Elevation,
                              PlotSize, baseYear, baseSA)]
  treeData <- setkey(measreidtable, OrigPlotID1, OrigPlotID2, MeasureYear)[setkey(treeData,
                                                                                  OrigPlotID1,
                                                                                  OrigPlotID2,
                                                                                  MeasureYear), nomatch = 0]

  treeData <- standardizeSpeciesNames(treeData, forestInventorySource = "BCPSP") #Need to add to pemisc
  return(list(plotHeaderData = headerData, treeData = treeData))
}
