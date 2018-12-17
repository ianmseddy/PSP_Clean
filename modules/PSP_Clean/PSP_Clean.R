
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
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
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "pspABMatureRaw", objectClass = "data.table", desc =  "Alberta PSP for mature trees only?",
                 sourceURL = "https://drive.google.com/open?id=13g6Cbtdv4x-KXo9-1ATWLpVseVipkKQp"),
    expectsInput(objectName = "pspLocationABRaw", objectClass = "data.table", desc = "location of Alberta PSP",
                 sourceURL = "https://drive.google.com/open?id=11GVbtqUKQXD2dxp5Tzo_Rom2Ke3VLTjU"),
    expectsInput(objectName = "pspBCRaw", objectClass = "list", desc = "BC PSP data",
                 sourceURL = "https://drive.google.com/open?id=1X5bbU5oBIcSWpl-1WXGszHhz1ltrDTx2"),
    expectsInput(objectName = "pspSKRaw", objectClass = "list", desc = "SK PSP data",
                 sourceURL = "https://drive.google.com/open?id=1yvOzfNqZ28oLYKTfdDb8-Wip7jBPtCz6"),
    expectsInput(objectName = "tspSKMisticRaw", objectClass = "list", desc = "temporary sampling plots from Saskatchewan?",
                 sourceURL = "https://drive.google.com/open?id=1PCn0DpGwsXBhquW3jOaqks1QC9teo_Xx"),
    # expectsInput(objectName = "tspSKPPPARaw", objectClass = "list", desc = "temporary sampling plots from Saskatchewan?",
    #              sourceURL = ) #This data exists in Yong's script but is not in the google drive
    expectsInput(objectName = "pspNFILocationRaw", objectClass = "data.table", desc = "NFI PSP sampling locations",
                 sourceURL = "https://drive.google.com/open?id=1_mIraySUqcmT8boD5TMX7qMfRe8bQ7l6"),
    expectsInput(objectName = "pspNFIHeaderRaw", objectClass = "data.table", desc = "NFI PSP header?",
                 sourceURL = "https://drive.google.com/open?id=1jGk3ebWnLO_tcgeUgVVr9Mi0htd_JzhG"),
    expectsInput(objectName = "pspNFITreeRaw", objectClass = "data.table", desc = "",
                 sourceURL = "https://drive.google.com/open?id=1qePgxqEyG0nUtVJSrs4cuaognybKqbGE")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "plotLocations", objectClass = "sf", desc = "merged PSP location data"),
    createsOutput(objectName = "allSP", objectClass = "data.table", desc = "merged PSP and TSP data")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.PSP_Clean = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # do stuff for this event
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
  pspAB <- dataPurification_ABMature(treeDataRaw = sim$pspABMatureRaw, plotHeaderDataRaw = sim$pspLocationABRaw)
  #Yong's original script did not remove treeNumber 9999. I prefer to keep his functions unchanged,  so I do so here
  pspAB$treeData <- pspAB$treeData[pspAB$treeData$TreeNumber != 9999,]

  #BC
  pspBC<- dataPurification_BCPSP(treeDataRaw = sim$pspBCRaw$treedata, plotHeaderDataRaw = sim$pspBCRaw$plotheader)

  #Saskatchewan PSP
  pspSK <- dataPurification_SKPSP(SADataRaw = sim$pspSKRaw$plotheader1, plotHeaderRaw = sim$pspSKRaw$plotheader3,
                                  measureHeaderRaw = sim$pspSKRaw$plotheader2, treeDataRaw= sim$pspSKRaw$treedata)

  #Saskatchewan Mistic
  tspSKMistic <- dataPurification_SKTSP_Mistic(compiledPlotData = sim$tspSKMisticRaw$plotheader,
                                               compiledTreeData = sim$tspSKMisticRaw$treedata)
  #Yong's original script did not remove treeNumber 0 (with 0 measurements).
  tspSKMistic$treeData <- tspSKmistic$treeData[tspSKmistic$treeData$treeNumber != 0,]

  #NFI
  pspNFI <- dataPurification_NFIPSP(lgptreeRaw = sim$pspNFITreeRaw, lgpHeaderRaw = sim$pspNFIHeaderRaw,
                                    approxLocation = sim$pspNFILocationRaw)

  pspNFI$treeData[,Genus := NULL] #This column is not in any of the other PSP datasets

  allSP <- data.table::rbindlist(list(pspAB$treeData,
                                       pspBC$treeData,
                                       pspSK$treeData,
                                       tspSKMistic$treeData,
                                       pspNFI$treeData),
                                  use.names = TRUE)

  allLocations <- data.table::rbindlist(list(pspAB$plotHeaderData,
                                             pspBC$plotHeaderData,
                                             pspSK$plotHeaderData,
                                             tspSKMistic$plotHeaderData,
                                             pspNFI$plotHeaderData),
                                        use.names = TRUE)

  allLocations <- geoCleanPSP(Locations = allLocations)

  sim$allLocations <- allLocations
  sim$allSP <- allSP

  # set(sim$allLocations, NULL, c("baseYear", "MeasureYear"), NULL) #I dont' think we need this, not sure yet

  # problems <- sim$allLocations[, .N, .(MeasureID, treeNumber, species)]
  #
  # merged <- allPSP[sim$allLocations, on = c("MeasureID", "OrigPlotID1", "MeasureYear")]
  # merged <- st_as_sf(merged)

  # st_write(allLocationsWGS,
  #          dsn = file.path(outputPath(sim), "allLocationsWGS.kml"),
  #          driver = "KML")

  return(invisible(sim))
}

geoCleanPSP <- function(inTree, Locations) {

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

  #Zone is now incorrect, I don't know why baseYear and MeasureYear are in plotLocation.
  set(Locations, NULL, "Zone", NULL)
  return(Locations)
}


.inputObjects <- function(sim) {
  dPath <- dataPath(sim)

  if (!suppliedElsewhere("pspABMatureRaw", sim)) {

    pspABMatureRaw <- Cache(prepInputs, targetFile = file.path(dPath, "ABMatureTreeData.csv"),
                              url = extractURL(objectName = "pspABMatureRaw"),
                              fun = "read.csv",
                              destinationPath = dPath,
                              filename2 = "ABMatureTreeData.csv")
    sim$pspABMatureRaw <- data.table(pspABMatureRaw)

  }

  if (!suppliedElsewhere("pspLocationAB", sim)) {

    pspLocationABRaw <- Cache(prepInputs, targetFile = file.path(dPath, "plotLocation.csv"),
                                     url = extractURL(objectName = "pspLocationABRaw"),
                                     destinationPath = dPath,
                                     fun = 'read.csv',
                                     filename2 = "plotLocationAB.csv")
    sim$pspLocationABRaw <- data.table(pspLocationABRaw)
  }

  if (!suppliedElsewhere("pspBCRaw", sim)) {

    pspBCRaw <- Cache(prepInputs, targetFile = file.path(dPath, "BC_PSP.RData"),
                            url = extractURL(objectName = "pspBCRaw"),
                            destinationPath = dPath,
                            fun = load)
    sim$pspBCRaw <- pspBCRaw
  }

  if (!suppliedElsewhere("pspSKRaw", sim)) {

    pspSKRaw <- prepInputs(targetFile = file.path(dPath, "SKPSP.RData"),
                            url = extractURL(objectName = "pspSKRaw"),
                            destinationPath = dPath,
                            fun = load)
    sim$pspSKRaw <- pspSKRaw
  }

  if (!suppliedElsewhere("tspSKMisticRaw", sim)) {

   tspSKMisticRaw <- prepInputs(targetFile = file.path(dPath, "SK_TSP_Mistic.RData"),
                                  url = extractURL(objectName = "tspSKMisticRaw"),
                                  destinationPath = dPath,
                                  fun = load)
   sim$tspSKMisticRaw <- tspSKMisticRaw
  }

  if (!suppliedElsewhere("pspNFILocationRaw", sim)) {

    pspNFILocationRaw <- prepInputs(targetFile = file.path(dPath, "all_gp_climate_approx_loc.csv"),
                                     url = extractURL(objectName = "pspNFILocationRaw"),
                                     destinationPath = dPath,
                                     fun = read.csv)
    sim$pspNFILocationRaw <- data.table(pspNFILocationRaw)
  }

  if (!suppliedElsewhere("pspNFIHeaderRaw", sim)) {

    pspNFIHeaderRaw <- prepInputs(targetFile = file.path(dPath, "all_gp_ltp_header.csv"),
                                   url = extractURL(objectName = "pspNFIHeaderRaw"),
                                   destinationPath = dPath,
                                   fun = read.csv)
    sim$pspNFIHeaderRaw <- data.table(pspNFIHeaderRaw)
  }

  if (!suppliedElsewhere("pspNFITreeRaw", sim)) {

    pspNFITreeRaw <- prepInputs(targetFile = file.path(dPath, "all_gp_ltp_tree.csv"),
                                 url = extractURL(objectName = "pspNFITreeRaw"),
                                 destinationPath = dPath,
                                 fun = read.csv)
    sim$pspNFITreeRaw <- data.table(pspNFITreeRaw)
  }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
