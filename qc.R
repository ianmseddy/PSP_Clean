allSP <- mySimOut$allSP
allLoc <- mySimOut$plotLocations

plot(allLoc)
allLoc
allSP[, .N, .(TreeNumber, Species, MeasureID)]

#How many entries have same treeNumber, Species, and MeasureID (so how many replicates?)
qc <- allSP[, .(.N), .(TreeNumber, Species, MeasureID)]
flag <- qc[N >1,]


# allSP[MeasureID == "SKTSP_Mistik_1054" & TreeNumber == 22,]
# allSP[MeasureID == "SKTSP_Mistik_1054" & TreeNumber == 23,]
# allSP[MeasureID == "SKTSP_Mistik_433"  & TreeNumber == 2,]
#E.G.
# MeasureID OrigPlotID1 OrigPlotID2 MeasureYear TreeNumber Species  DBH Height
# 1: SKTSP_Mistik_433 12655970594          NA        1999          2      WS  5.1    5.4
# 2: SKTSP_Mistik_433 12655970594          NA        1999          2      TA 18.6   19.9
# 3: SKTSP_Mistik_433 12655970594          NA        1999          2      WS  6.0    5.7
# 4: SKTSP_Mistik_433 12655970594          NA        1999          2      TA 23.7   20.9
# 5: SKTSP_Mistik_433 12655970594          NA        1999          2      TA 24.3   21.8
test <- allSP[, .(.N), .(TreeNumber, Species, MeasureID, MeasureYear)]
test2 <- test[N >1,]


test <- treeDataRaw
testAB <- test[, .(.N), .(MeasureYear, Treenumber, Groupnumber, Plotnumber)]
testAB[N > 1]
#All of the alberta duplicates of MeasureYear/Treenumber/Groupnumber/Plotnumber are tree 0 or 9999.
#There are 50,000 duplicates which isn't bad given 1.13 million observations

testAb2 <- test[, .(.N), .(Treenumber, Groupnumber, Plotnumber)]
#Shows repeat measurements (though NAs are considered repeats)
test[Plotnumber == 1 & Groupnumber == 1 & Treenumber == 1]
test[Plotnumber == 1 & Groupnumber == 938 & Treenumber == 46]#Actual repeat measurement
testAb2
