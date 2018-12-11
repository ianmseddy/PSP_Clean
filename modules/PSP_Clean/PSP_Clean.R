
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
  reqdPkgs = list("data.table"),
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
    createsOutput(objectName = "pspAB", objectClass = "list", desc = "list of cleaned AB PSP header and tree data"),
    createsOutput(objectName = "pspBC", objectClass = "list", desc = "list of cleaned BC PSP header and tree data"),
    createsOutput(objectName = "pspSK", objectClass = "list", desc = "list of cleaned SK PSP header and tree data"),
    createsOutput(objectName = "pspSKMistic", objectClass = "list",
                  desc = "list of cleaned SK Temporary Sampling Plot Mistic header and tree data")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.PSP_Clean = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

    },

    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "PSP_Clean", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {

    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {


  sim$pspAB <- dataPurification_ABMature(treeDataRaw = sim$pspABMatureRaw, plotHeaderDataRaw = sim$pspLocationABRaw)

  sim$pspBC <- dataPurification_BCPSP(treeDataRaw = sim$pspBCRaw$treedata, plotHeaderDataRaw = sim$pspBCRaw$plotheader)

  sim$pspSK <- dataPurification_SKPSP(SADataRaw = sim$pspSKRaw$plotheader1, plotHeaderRaw = sim$pspSKRaw$plotheader3,
                                      measureHeaderRaw = sim$pspSKRaw$plotheader2, treeDataRaw= sim$pspSKRaw$treedata)

  sim$tspSKMistic <- dataPurification_SKTSP_Mistic(compiledPlotData = sim$tspSKMisticRaw$plotheader,
                                                   compiledTreeData = sim$tspSKMisticRaw$treedata)

  sim$pspNFI <- dataPurification_NFIPSP(lgptreeRaw = sim$pspNFITreeRaw, lgpHeaderRaw = sim$pspNFIHeaderRaw,
                                        approxLocation = sim$pspNFILocationRaw)

  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {

  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {

  return(invisible(sim))
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
