defineModule(sim, list(
  name = "PSP_Clean",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2.9002", PSP_Clean = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "PSP_Clean.Rmd"),
  reqdPkgs = list("data.table", "sf"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "pspABplotMeasure", objectClass = "data.table", 
                 desc = "PSP plot measurement data from the Government of Alberta",
                 sourceURL = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing"),
    expectsInput(objectName = "pspABtreeMeasure", objectClass = "data.table", 
                 desc = "PSP tree measurement data from the Government of Alberta",
                 sourceURL = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing"),
    expectsInput(objectName = "pspABtree", objectClass = "data.table", 
                 desc = "PSP tree data from the Government of Alberta - needed for species",
                 sourceURL = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing"),
    expectsInput(objectName = "pspABplot", objectClass = "data.table", 
                 desc = "PSP plot data from Government of Alberta - needed for locational attributes",
                 sourceURL = "https://drive.google.com/file/d/1qGiHEpkeSjiHR73zhmSFEFq8BO8_9GTP/view?usp=sharing"),
    expectsInput(objectName = "pspBCRaw", objectClass = "list", desc = "BC PSP data",
                 sourceURL = "https://drive.google.com/open?id=1P6dcyqwH41-umWvfoCTNQC3BAAWdkitO"),
    expectsInput(objectName = "pspSKRaw", objectClass = "list", desc = "SK PSP data",
                 sourceURL = "https://drive.google.com/open?id=1yvOzfNqZ28oLYKTfdDb8-Wip7jBPtCz6"),
    expectsInput(objectName = "tspSKMistikRaw", objectClass = "list", desc = "temporary sampling plots from Saskatchewan?",
                 sourceURL = "https://drive.google.com/open?id=1PCn0DpGwsXBhquW3jOaqks1QC9teo_Xx"),
    # expectsInput(objectName = "tspSKPPPARaw", objectClass = "list", desc = "temporary sampling plots from Saskatchewan?",
    #              sourceURL = ) #This data exists in Yong's script but is not in the google drive
    expectsInput(objectName = "pspNFILocationRaw", objectClass = "data.table", desc = "NFI PSP sampling locations",
                 sourceURL = "https://drive.google.com/file/d/1S-4itShMXtwzGxjKPgsznpdTD2ydE9qn/view?usp=sharing"),
    expectsInput(objectName = "pspNFIHeaderRaw", objectClass = "data.table", desc = "NFI PSP header?",
                 sourceURL = "https://drive.google.com/file/d/1i4y1Tfi-kpa5nHnpMbUDomFJOja5uD2g/view?usp=sharing"),
    expectsInput(objectName = "pspNFITreeRaw", objectClass = "data.table", desc = "",
                 sourceURL = "https://drive.google.com/file/d/1i4y1Tfi-kpa5nHnpMbUDomFJOja5uD2g/view?usp=sharing")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "PSPmeasure", objectClass = "data.table", desc = "merged PSP and TSP individual measurements"),
    createsOutput(objectName = "PSPplot", objectClass = "data.table", desc = "merged PSP and TSP plot data"),
    createsOutput(objectName = "PSPgis", objectClass = "sf", desc = "Plot location sf object. Contains duplicates")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.PSP_Clean = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- Init(sim)

    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


### template initialization
Init <- function(sim) {

  #Alberta
  pspAB <- dataPurification_ABPSP(treeMeasure = sim$pspABtreeMeasure, 
                                  plotMeasure = sim$pspABplotMeasure,
                                  tree = sim$pspABtree,
                                  plot = sim$pspABplot)

  #BC
  pspBC<- dataPurification_BCPSP(treeDataRaw = sim$pspBCRaw$treedata, plotHeaderDataRaw = sim$pspBCRaw$plotheader)

  #Saskatchewan PSP
  pspSK <- dataPurification_SKPSP(SADataRaw = sim$pspSKRaw$plotheader1, plotHeaderRaw = sim$pspSKRaw$plotheader3,
                                  measureHeaderRaw = sim$pspSKRaw$plotheader2, treeDataRaw= sim$pspSKRaw$treedata)

  #Saskatchewan Mistik
  tspSKMistik <- dataPurification_SKTSP_Mistik(compiledPlotData = sim$tspSKMistikRaw$plotheader,
                                               compiledTreeData = sim$tspSKMistikRaw$treedata)
  #Yong's original script did not remove treeNumber 0 (with 0 measurements).

  tspSKMistik$treeData <- tspSKMistik$treeData[tspSKMistik$treeData$TreeNumber != 0,]

  #NFI
  pspNFI <- dataPurification_NFIPSP(lgptreeRaw = sim$pspNFITreeRaw, lgpHeaderRaw = sim$pspNFIHeaderRaw,
                                    approxLocation = sim$pspNFILocationRaw)

  pspNFI$treeData[, Species := paste0(Genus, "_", Species)]
  pspNFI$treeData[,Genus := NULL] #This column is not in any of the other PSP datasets

  #Rename keys before combining PSP datasets (BC is already unique with BC identified in value)
  #Composite keys remain separate; 2nd part of key missing from HeaderData
  pspAB$treeData$OrigPlotID1 <- paste0("AB", pspAB$treeData$OrigPlotID1)
  pspAB$plotHeaderData$OrigPlotID1 <- paste0("AB", pspAB$plotHeaderData$OrigPlotID1)

  pspBC$treeData$OrigPlotID1 <- paste0("BC", pspBC$treeData$OrigPlotID1)
  pspBC$plotHeaderData$OrigPlotID1 <- paste0("BC", pspBC$plotHeaderData$OrigPlotID1)

  pspSK$treeData$OrigPlotID1 <- paste0("SK", pspSK$treeData$OrigPlotID1)
  pspSK$plotHeaderData$OrigPlotID1 <- paste0("SK", pspSK$plotHeaderData$OrigPlotID1)

  pspNFI$treeData$OrigPlotID1 <- paste0("NFI", pspNFI$treeData$OrigPlotID1)
  pspNFI$plotHeaderData$OrigPlotID1 <- paste0("NFI", pspNFI$plotHeaderData$OrigPlotID1)

  tspSKMistik$treeData$OrigPlotID1 <- paste0("SKMistik", tspSKMistik$treeData$OrigPlotID1)
  tspSKMistik$plotHeaderData$OrigPlotID1 <- paste0("SKMistik", tspSKMistik$plotHeaderData$OrigPlotID1)
  sim$PSPmeasure <- rbindlist(list(pspAB$treeData,
                                   pspBC$treeData,
                                   pspSK$treeData,
                                   tspSKMistik$treeData,
                                   pspNFI$treeData),
                              use.names = TRUE, 
                              fill = TRUE)

  sim$PSPplot <- rbindlist(list(pspAB$plotHeaderData,
                                pspBC$plotHeaderData,
                                pspSK$plotHeaderData,
                                tspSKMistik$plotHeaderData,
                                pspNFI$plotHeaderData),
                           use.names = TRUE, fill = TRUE)


  sim$PSPgis <- geoCleanPSP(Locations = sim$PSPplot)

  #Some plots are dropped because of crappy locational data.
  #Exclude measure/plot data with incorrect GIS
  sim$PSPmeasure <- sim$PSPmeasure[sim$PSPmeasure$MeasureID %in% sim$PSPgis$MeasureID]
  #Above method drops 4 NFI duplicates that weren't removed using 'on = "MeasureID"'
  sim$PSPplot <- sim$PSPplot[sim$PSPplot$MeasureID %in% sim$PSPgis$MeasureID]
  #Get unique GIS
  sim$PSPgis <- unique.data.frame(sim$PSPgis[, "OrigPlotID1"])
  #PSPgis now joins with PSPplot by OrigPlotID1. PSPplot joins with PSPmeasure by MeasureID.
  #MeasureID in plot incorporated 'stand' (present in some PSP), which is why PSPmeasure
  #does not join PSPplot by the more obvious 'OrigPlotID1' variable.
  #This is confusing and seems intuitively wrong, but it works.

  #In case you want a KML or shapefile
  # st_write(sim$PSPgis,
  #          dsn = file.path(outputPath(sim), "allLocationsWGS.kml"),
  #          driver = "KML")

  return(invisible(sim))
}

geoCleanPSP <- function(Locations) {

  #Seperate those using UTM
  LocationsUTM <- Locations[is.na(Longitude)| Longitude == 0,]
  LocationsWGS <- Locations[!is.na(Longitude) & Longitude != 0,]

  # a few points in UTM 11 are missing northing digits. Blame Alberta?
  LocationsUTM <- LocationsUTM[nchar(LocationsUTM$Northing) > 3,] #better way to fix?
  LocationsWGS <- st_as_sf(x = LocationsWGS,
                              coords = c("Longitude", "Latitude"),
                              crs = "+proj=longlat +datum=WGS84")
  set(LocationsWGS, NULL, c("Northing", "Easting"),NULL) #need equal number of columns

  LocationsReproj <- lapply(unique(LocationsUTM$Zone), FUN = function(x, points = LocationsUTM) {
    output <- st_as_sf(x = points[points$Zone == x,],
                       coords = c("Easting", "Northing"),
                       crs = paste0("+proj=utm +zone=", x, " +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
    set(output, NULL, c("Latitude", "Longitude"),NULL) #mostly NA or wrong
    output <- st_transform(output, crs = "+proj=longlat +datum=WGS84") #reproject to longlat
    return(output)
  })
  names(LocationsReproj) <- paste0("prev_UTMzone", unique(LocationsUTM$Zone))

  #Merge all datasets together
  LocationsReproj$WGS <- LocationsWGS
  LocationsReproj$deparse.level <- 1

  Locations <- do.call(rbind, args = LocationsReproj)

  #The dataset contains separate entries for different years at the same location, presumably for when CMI is sampled
  set(Locations, NULL, "Zone", NULL)
  return(Locations)
}


.inputObjects <- function(sim) {
  dPath <- dataPath(sim)

  if (!suppliedElsewhere("pspABtreeMeasure", sim)) {

    sim$pspABtreeMeasure <- prepInputs(targetFile = file.path(dPath, "trees_measurement.csv"),
                            url = extractURL(objectName = "pspABtreeMeasure"),
                            fun = "fread",
                            overwrite = TRUE,
                            destinationPath = dPath)
  }
  
  if (!suppliedElsewhere("pspABtree", sim)) {
    
    sim$pspABtree <- prepInputs(targetFile = file.path(dPath, "trees.csv"),
                                   url = extractURL(objectName = "pspABtree"),
                                   fun = "fread",
                                   overwrite = TRUE,
                                   destinationPath = dPath)
  }

  if (!suppliedElsewhere("pspABplotMeasure", sim)) {

    sim$pspABplotMeasure <- prepInputs(targetFile = file.path(dPath, "plot_measurement.csv"),
                                   url = extractURL(objectName = "pspABplotMeasure"),
                                   destinationPath = dPath,
                                   overwrite = TRUE,
                                   fun = 'fread')
  }
  
  if (!suppliedElsewhere("pspABplot", sim)) {
    
    sim$pspABplot <- prepInputs(targetFile = file.path(dPath, "plot.csv"),
                                   url = extractURL(objectName = "pspABplot"),
                                   fun = "fread",
                                   overwrite = TRUE,
                                   destinationPath = dPath)
  }

  if (!suppliedElsewhere("pspBCRaw", sim)) {

    pspBCRaw <- prepInputs(targetFile = file.path(dPath, "BC_PSP.RData"),
                           url = extractURL(objectName = "pspBCRaw"),
                           destinationPath = dPath,
                           fun = 'readRDS',
                           useCache = TRUE,
                           userTags = c(currentModule(sim), "pspBCRaw"))
    sim$pspBCRaw <- pspBCRaw
  }

  if (!suppliedElsewhere("pspSKRaw", sim)) {

    pspSKRaw <- prepInputs(targetFile = file.path(dPath, "SKPSP.RData"),
                           url = extractURL(objectName = "pspSKRaw"),
                           destinationPath = dPath,
                           fun = "load",
                           overwrite = TRUE)
    sim$pspSKRaw <- pspSKRaw
  }

  if (!suppliedElsewhere("tspSKMistikRaw", sim)) {

    tspSKMistikRaw <- prepInputs(targetFile = file.path(dPath, "SK_TSP_Mistik.RData"),
                                 url = extractURL(objectName = "tspSKMistikRaw"),
                                 destinationPath = dPath,
                                 fun = 'load',
                                 overwrite = TRUE)

   sim$tspSKMistikRaw <- tspSKMistikRaw
  }

  if (!suppliedElsewhere("pspNFILocationRaw", sim)) {

    pspNFILocationRaw <- prepInputs(targetFile = file.path(dPath, "all_gp_site_info.csv"),
                                    url = extractURL(objectName = "pspNFILocationRaw"),
                                    destinationPath = dPath,
                                    fun = 'read.csv',
                                    useCache = TRUE,
                                    userTags = c(currentModule(sim), "pspNFILocationRaw"))

    sim$pspNFILocationRaw <- data.table(pspNFILocationRaw)
  }

  if (!suppliedElsewhere("pspNFIHeaderRaw", sim)) {

    pspNFIHeaderRaw <- prepInputs(targetFile = file.path(dPath, "all_gp_ltp_header.csv"),
                                   url = extractURL(objectName = "pspNFIHeaderRaw"),
                                   destinationPath = dPath,
                                   fun = 'read.csv',
                                   overwrite = TRUE)

    sim$pspNFIHeaderRaw <- data.table(pspNFIHeaderRaw)
  }

  if (!suppliedElsewhere("pspNFITreeRaw", sim)) {

    pspNFITreeRaw <- prepInputs(targetFile = file.path(dPath, "all_gp_ltp_tree.csv"),
                                url = extractURL(objectName = "pspNFITreeRaw"),
                                destinationPath = dPath,
                                fun = 'read.csv',
                                overwrite = TRUE)

    sim$pspNFITreeRaw <- data.table(pspNFITreeRaw)
  }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
