## ----eval=TRUE, include = FALSE-----------------------------------------------
knitr::opts_chunk$set(
  dev="pdf",
  highlight = TRUE,
  dpi = 1,
  collapse = TRUE,
  comment = "#>",
  rownames = FALSE, 
  #fig.width   = 6,      
  #fig.height  = 5,      
  # fig.path    = 'figs/', 
  fig.align   = 'center'
  #out.width='\\textwidth'
  )
old.par <- par(no.readonly = TRUE)  

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  install.packages("MorphoTools2")
#  library("MorphoTools2")
#  

## ----include=FALSE, eval=TRUE-------------------------------------------------
library(MorphoTools2)

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("MarekSlenker/MorphoTools2")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
data(centaurea)

## ----eval = FALSE, echo = TRUE------------------------------------------------
#  centaurea = read.morphodata(file = "<PATH>/centaurea.txt", dec = ".", sep = "\t")
#  centaurea = read.morphodata(file = "clipboard")

## ----echo = FALSE, eval = TRUE------------------------------------------------
summary<-function(object){
  cat("Object of class \'morphodata\'\
- contains 33 populations
- contains 4 taxa (defined groups)

Populations: BABL, BABU, BOL, BRT, BUK, CERM, CERV, CZLE, DEB, DOM, DUB, HVLT, KASH,
 KOT, KOZH, KRO, LES, LIP, MIL, NEJ, NSED, OLE1, OLE2, PREL, PRIS, PROS, RTE, RUS,
 SOK, STCV, STGH, VIT, VOL
Taxa (defined groups): hybr, ph, ps, st\n")
}

## ----echo = TRUE, eval=TRUE, collapse=TRUE------------------------------------
summary(centaurea)

## ----echo = FALSE, eval = TRUE------------------------------------------------
rm(summary)

## ----include=F----------------------------------------------------------------
options(max.print = 78)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
samples(centaurea)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
populations(centaurea)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
taxa(centaurea)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
characters(centaurea)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  viewMorphodata(centaurea)

## ----include=F----------------------------------------------------------------
options(max.print = 28)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
shapiroWilkTest(centaurea)


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  swTest = shapiroWilkTest(centaurea)
#  exportRes(swTest, file = "clipboard")
#  exportRes(swTest, file = "D:/Projects/Centaurea/morpho/shapiroWilkTest.txt")

## ----histCharacter, echo = TRUE, eval=TRUE, out.width = '320px'---------------
histCharacter(centaurea, character = "SF")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  histAll(centaurea, folderName = "histograms")

## ----qqnormCharacter, echo = TRUE, eval=TRUE, out.width = '320px', out.height= '300px'----
qqnormCharacter(centaurea, character = "SF")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  qqnormAll(centaurea, folderName = "qqnormPlots")

## ----transformations, echo = FALSE, eval=TRUE, out.width = '320px', out.height= '300px'----

par(mfrow=c(2,2))
par(mar=c(4,4,2,1))
par(mgp=c(2,0.8,0))

centSquareRoot = transformCharacter(centaurea, character = "SF", FUN = sqrt)
centLog = transformCharacter(centaurea, character = "SF", FUN = function(x) log(x+1))
centCubeRoot = transformCharacter(centaurea, character = "SF", FUN = function(x) x^(1/3))



stats::qqnorm(as.matrix( na.omit(centaurea$data["SF"])), main = "original data", cex = 0.9, bty="n")
stats::qqline(as.matrix( na.omit(centaurea$data["SF"])), lwd=2)


stats::qqnorm(as.matrix( na.omit(centSquareRoot$data["SF"])), main = "sqrt transformed", cex = 0.9, bty="n")
stats::qqline(as.matrix( na.omit(centSquareRoot$data["SF"])), lwd=2)

stats::qqnorm(as.matrix( na.omit(centLog$data["SF"])), main = "log(x+1) transformed",cex = 0.9, bty="n")
stats::qqline(as.matrix( na.omit(centLog$data["SF"])), lwd=2)

stats::qqnorm(as.matrix( na.omit(centCubeRoot$data["SF"])), main = "x^(1/3) transformed", cex = 0.9, bty="n")
stats::qqline(as.matrix( na.omit(centCubeRoot$data["SF"])), lwd=2)



## ----echo = TRUE, eval = FALSE------------------------------------------------
#  centaurea = transformCharacter(centaurea, character = "SF", newName = "SF.sqrt",
#                                 FUN = function(x) sqrt(x))

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  boxplotCharacter(centaurea, character="AL", col=c("blue","green","red","orange"))

## ----boxplotCharacter1, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----
graphics::par(mar=c(3, 4.1, 1.8, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
boxplotCharacter(centaurea, character = "AL", col = c("blue","green","red","orange"), cex.main=1.3)


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  boxplotCharacter(centaurea, character = "AL", pch = 1,
#                   lowerWhisker = 0.1, upperWhisker = 1)

## ----boxplotCharacter2, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----
graphics::par(mar=c(3, 4.1, 1.8, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
boxplotCharacter(centaurea, character = "AL", pch = 1, cex.main=1.3,
                 lowerWhisker = 0.1, upperWhisker = 1)
                 

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  boxplotCharacter(centaurea, character = "AL", outliers = FALSE,
#                   frame = FALSE, horizontal = T, notch = TRUE)

## ----boxplotCharacter3, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----
graphics::par(mar=c(3, 4.1, 1.8, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
boxplotCharacter(centaurea, character = "AL", outliers = FALSE,cex.main=1.3,
                 frame = FALSE, horizontal = T, notch = TRUE)
                 

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  boxplotAll(centaurea, folderName = "boxplots")

## ----include=F----------------------------------------------------------------
options(max.print = 36)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
descrTaxon(centaurea, format = "($MEAN ± $SD)", decimalPlaces = 2)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  descrTax = descrTaxon(centaurea, format = "($MEAN ± $SD)", decimalPlaces = 2)
#  
#  exportRes(descrTax, file = "clipboard")
#  exportRes(descrTax, file = "descrTax.txt")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  correlations.s = cormat(centaurea, method = "spearman")
#  exportRes(correlations.s, file = "correlations.spearman.txt")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  correlations.s.signifTest = cormatSignifTest(centaurea, method = "spearman")

## ----echo = FALSE, eval = TRUE------------------------------------------------
populOTU <-function(object, crossval="indiv"){
cat("Warning: Unable to calculate the means of characters AL AW ALW AP in
populations LIP PREL. Values are NA.")
}

## ----echo = TRUE, eval=TRUE---------------------------------------------------
pops = populOTU(centaurea)

## ----echo = FALSE, eval = TRUE------------------------------------------------
rm(populOTU)

## ----echo = FALSE, eval=TRUE--------------------------------------------------
pops = suppressWarnings(populOTU(centaurea))

## ----include = FALSE----------------------------------------------------------
options(max.print = 800)
centaurea = removePopulation(centaurea, populationName = c("BRT", "CERV", "HVLT", "KASH", "KRO", "MIL", "NSED", "CERM", "BABL", "OLE1", "OLE2", "PRIS", "PROS", "SOK", "STGH"))
# centaurea = removeCharacter(centaurea, characterName = c("SF", "ST", "IW", "ILW", "MW", "MLW", "AL", "ALW"))

## ----echo = TRUE, eval=TRUE---------------------------------------------------
# For demonstration only. Not all populations are displayed.
missingCharactersTable(centaurea, level = "pop")

## ----include = FALSE----------------------------------------------------------
centaurea$data = centaurea$data[-seq(4,10,1)]
centaurea = removeCharacter(centaurea, characterName = c("SF", "ST", "IW", "ILW", "MW", "MLW", "AL", "ALW", "AW", "IL"))

## ----echo = TRUE, eval = TRUE-------------------------------------------------
# For demonstration purposes only. Only a subset of data is displayed.
missingSamplesTable(centaurea, level = "pop")

## ----include = FALSE----------------------------------------------------------
data("centaurea")
options(max.print = 60)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))
pops = removePopulation(pops, populationName = c("LIP", "PREL"))

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  centaurea = naMeanSubst(centaurea)

## ----include = FALSE----------------------------------------------------------
centaurea = suppressWarnings(naMeanSubst(centaurea))


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  hierClust = clust(pops, distMethod = "Euclidean", clustMethod = "UPGMA")
#  plot(hierClust, hang = -1, sub = "", xlab = "", ylab = "distance")

## ----hierClust, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-----------
graphics::par(mar=c(3, 4.1, 1.5, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
hierClust = clust(pops, distMethod = "Euclidean", clustMethod = "UPGMA")
plot(hierClust, hang = -1, sub = "", xlab = "", ylab = "distance", cex=0.6, cex.main=1)


## ----echo = TRUE, eval=TRUE---------------------------------------------------
pca.centaurea = pca.calc(centaurea)

## ----include=F----------------------------------------------------------------
options(max.print = 64)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
summary(pca.centaurea)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(pca.centaurea, col = c("blue","green","red","orange"), axes=c(1,2),
#             pch = c(8,17,20,18), legend = T, ncol = 2, legend.pos="bottomright")
#  

## ----pca.centaurea1, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(pca.centaurea, col = c("blue","green","red","orange"), axes=c(1,2), cex = 0.9,
           pch = c(8,17,20,18))
plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"), box.lwd = 0.9,
x = "bottomright", cex = 0.8, ncol = 2)


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  exportRes(pca.centaurea$objects$scores, file="scoresPCA.centaurea.txt")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotCharacters(pca.centaurea)
#  

## ----plotCharacters.pca1, echo = FALSE, eval=TRUE, fig.width= 3.8, fig.height=3.3----
graphics::par(mar=c(3, 4.1, 2, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotCharacters(pca.centaurea, cex = 0.5, xlim=c(-0.5, 0.5),ylim=c(-0.5, 0.5), cex.main=0.9)


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  exportRes(pca.centaurea$eigenvectors, file="eigenvectors.centaurea.txt")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotBiplot(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.4,
#             pch = c(8,17,20,18), legend = T, ncol = 2, legend.pos="bottomright")

## ----plotBiplot, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----------
pca.pops = pca.calc(pops)
graphics::par(mar=c(2.8, 4.1, 0, 2.1), mgp=c(1.5, 0.5, 0), cex.axis=1, cex.lab=0.8, lwd=0.9)
# plotBiplot(pca.pops, col = c("blue","green","red","orange"), pch = c(8,17,20,18))
plotBiplot(pca.centaurea, col = c("blue","green","red","orange"), pch = c(8,17,20,18),
           cex = 0.4,legend = T, ncol = 2, legend.pos="bottomright")


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  pca.pops = pca.calc(pops)
#  plotPoints(pca.pops, col = c("blue","green","red","orange"), pch=c(8,17,20,18),
#              legend = FALSE, labels = FALSE)
#  plotAddLabels.points(pca.pops, labels=c("PROS","SOK","KASH","BOL","KRO","DUB",
#            "MIL","CERM","DOM","KOZH","KOT"), include=FALSE, pos=4, cex=0.7)
#  plotAddLabels.points(pca.pops, labels=c("PROS","SOK","KASH","BOL","CERM","DOM"),
#                        pos = 2, cex = 0.7)

## ----pca.pops1, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-----------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
pca.pops = pca.calc(pops)
plotPoints(pca.pops, col = c("blue","green","red","orange"), pch=c(8,17,20,18), 
            legend = FALSE, labels = FALSE)
plotAddLabels.points(pca.pops, labels=c("PROS","SOK","KASH","BOL","KRO","DUB",
          "MIL","CERM","DOM","KOZH","KOT"), include=FALSE, pos=4, cex=0.7)
plotAddLabels.points(pca.pops, labels=c("PROS","SOK","KASH","BOL","CERM","DOM"), 
                      pos = 2, cex = 0.75)
                      

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotCharacters(pca.pops, labels = FALSE)
#  plotAddLabels.characters(pca.pops,labels=c("ILW","MLW","LBA","IW","SFT"),cex=0.75)

## ----pca.pops.characters, echo = FALSE, eval=TRUE, fig.width= 3.8, fig.height=3.3----
graphics::par(mar=c(3, 4.1, 2, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotCharacters(pca.pops, labels = FALSE, cex = 0.5, xlim=c(-0.5, 0.5),ylim=c(-0.5, 0.5), cex.main=0.9)

plotAddLabels.characters(pca.pops,labels=c("ILW","MLW","LBA","IW","SFT"),cex=0.75)


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotBiplot(pca.pops, col = c("blue","green","red","orange"),
#             pch = c(8,17,20,18), legend = T, ncol = 2, legend.pos="bottomright")

## ----plotBiplot2, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3---------
pca.pops = pca.calc(pops)
graphics::par(mar=c(2.8, 4.1, 0, 2.1), mgp=c(1.5, 0.5, 0), cex.axis=1, cex.lab=0.8, lwd=0.9)
plotBiplot(pca.pops, col = c("blue","green","red","orange"), pch = c(8,17,20,18),
           legend = T, ncol = 2, legend.pos="bottomright")


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.5)
#  plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"),
#                 x = "bottomright", cex = 0.8, box.type = "n", ncol = 2)
#  
#  # Semi-transparent spiders
#  plotAddSpiders(pca.centaurea, col=c(rgb(0,0,255, max=255, alpha=50), # blue
#                                      rgb(0,255,0, max=255, alpha=50), # green
#                                      rgb(255,0,0, max=255, alpha=50), # red
#                                      # orange
#                                      rgb(255,102,0, max=255, alpha=50)))

## ----pca.spiders1, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3--------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)

plotPoints(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.5)
plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"), 
               x = "bottomright", cex = 0.8, box.type = "n", ncol = 2)

# Semi-transparent spiders
plotAddSpiders(pca.centaurea, col=c(rgb(0,0,255, max=255, alpha=50), # blue
                                    rgb(0,255,0, max=255, alpha=50), # green
                                    rgb(255,0,0, max=255, alpha=50), # red
                                    rgb(255,102,0, max=255, alpha=50))) # orange


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.5)
#  plotAddSpiders(pca.centaurea, col=c(NA,NA,NA,rgb(255,102,0,max=255,alpha=100)))

## ----pca.spiders2, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3--------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.5)

plotAddSpiders(pca.centaurea, col=c(NA,NA,NA,rgb(255,102,0,max=255,alpha=100)))


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.7)
#  plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"),
#                 x = "bottomright", pt.cex = 1.3, box.type = "n", ncol = 2)
#  
#  plotAddEllipses(pca.centaurea, col = c("blue","green","red","orange"), lwd = 2)

## ----pca.ellipses, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3--------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.7)
plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"), 
               x = "bottomright", cex = 0.7, pt.cex = 1.3, box.type = "n", ncol = 2)


plotAddEllipses(pca.centaurea, col = c("blue","green","red","orange"), lwd = 2)


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(pca.centaurea, type = "n", xlim = c(-5,7.5), ylim = c(-5,4))
#  
#  plotAddEllipses(pca.centaurea, col = c("blue","green","red","orange"), lwd = 2)
#  
#  plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"),
#                         x = "bottomright", box.type = "n", ncol = 2)

## ----pca.legend, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(pca.centaurea, type = "n", xlim = c(-5,7.5), ylim = c(-5,4))

plotAddEllipses(pca.centaurea, col = c("blue","green","red","orange"), lwd = 2)

plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"), 
              x = "bottomright", pt.cex = 0.8, cex = 0.8, box.type = "n", ncol = 2)
              

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plot3Dpoints(pca.centaurea, col = c("blue","green","red","orange"),
#               phi = 20, theta = 30)

## ----pca.3D, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3--------------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plot3Dpoints(pca.centaurea, col = c("blue","green","red","orange"), cex = 0.8,
             phi = 20, theta = 30)
             

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plot3Dpoints(pca.pops, col = c("blue","green","red","orange"), labels = T)

## ----pca.3D2, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-------------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plot3Dpoints(pca.pops, col = c("blue","green","red","orange"), labels = T, cex = 0.8)


## ----echo = FALSE, eval=TRUE--------------------------------------------------
summary.pcoadata <- function(object) {

  cat("Object of class 'pcoadata'; storing results of principal coordinates analysis\n")
  cat("Resemblance coefficient: ", object$distMethod,"\n")
  cat("\nVariation explained by individual axes")
  if (object$rank>3) {
    cat(" (listing of axes is truncated):\n")
  } else {
    cat(":\n")
  }

  descrTable = data.frame(row.names = names(object$eigenvaluesAsPercentages[1: min(object$rank, 3)]),
                          "Eigenvalues" = round(object$eigenvalues[1: min(object$rank, 3)], digits = 4),
                          "Eigenvalues as percentages" = round(object$eigenvaluesAsPercentages[1: min(object$rank, 3)], digits = 4),
                          "Cumulative percentage of eigenvalues" = round(object$cumulativePercentageOfEigenvalues[1: min(object$rank, 3)], digits = 4)
  )
  names(descrTable) = gsub(pattern = '\\.' , replacement = " ", x = names(descrTable))
  descrTable = t(descrTable)

  print(descrTable)
}

## ----echo = TRUE, eval=TRUE---------------------------------------------------
pcoa.res = pcoa.calc(centaurea, distMethod = "Manhattan")
summary(pcoa.res)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plot3Dpoints(pcoa.res, col = c("blue","green","red","orange"), pch = c(8,17,20,18),
#               phi = 20, theta = 70, legend = T)

## ----pcoa.3d, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-------------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plot3Dpoints(pcoa.res, col = c("blue","green","red","orange"), pch = c(8,17,20,18), 
             phi = 20, theta = 70, legend = F)
plotAddLegend(pca.centaurea, col = c("blue","green","red","orange"), box.lwd = 0.9,
              x = "topright", cex = 0.8)


## ----echo = TRUE, eval=TRUE, out.width = '320px'------------------------------
nmds.res = nmds.calc(centaurea, distMethod = "Euclidean", k = 3)
summary(nmds.res)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(nmds.res, col = c("blue","green","red","orange"), pch=c(8,17,20,18))

## ----nmds, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=2.7--------------
graphics::par(mar=c(2.8, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(nmds.res, col = c("blue","green","red","orange"), pch = c(8,17,20,18))


## ----include=F----------------------------------------------------------------
options(max.print = 260)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
stepdisc.calc(centaurea)

## ----include=F----------------------------------------------------------------
options(max.print = 60)


## ----include=F----------------------------------------------------------------
options(max.print = 34)


## ----echo = TRUE, eval=TRUE, out.width = '320px'------------------------------
cda.centaurea = cda.calc(centaurea)

summary(cda.centaurea)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  plotPoints(cda.centaurea, col = c("blue","green","red","orange"), axes = c(1, 2),
#             pch = c(8,17,20,18), legend = T, ncol = 2, legend.pos="bottomright")

## ----cda.points1, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3---------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(cda.centaurea, col = c("blue","green","red","orange"), axes = c(1, 2),
           pch = c(8,17,20,18), legend = F)
plotAddLegend(cda.centaurea, col = c("blue","green","red","orange"), box.lwd = 0.9,
              x = "bottomright", cex = 0.8, ncol = 2)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(cda.centaurea, col = c(NA, "green", NA, NA), cex = 0.8)
#  plotAddSpiders(cda.centaurea, col = c(rgb(0,0,255,max=255,alpha=130), # blue
#                                        NA, # green
#                                        rgb(255,0,0,max=255,alpha=130), # red
#                                        rgb(255,102,0,max=255,alpha=130))) # orange

## ----cda.points.spiders, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(cda.centaurea, col = c(NA, "green", NA, NA), cex = 0.8)
plotAddSpiders(cda.centaurea, col = c(rgb(0,0,255,max=255,alpha=130), # blue
                                      NA, # green
                                      rgb(255,0,0,max=255,alpha=130), # red
                                      rgb(255,102,0,max=255,alpha=130))) # orange

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(cda.centaurea, col = c(NA, "green", NA, NA), cex = 0.8)
#  plotAddEllipses(cda.centaurea, col = c("blue", NA, "red", "orange"), lwd = 2)
#  

## ----cda.points.ellipses, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(cda.centaurea, col = c(NA, "green", NA, NA), cex = 0.8)
plotAddEllipses(cda.centaurea, col = c("blue", NA, "red", "orange"), lwd = 2)


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(cda.centaurea, col = c("blue","green","red","orange"), cex = 0.4)
#  plotAddEllipses(cda.centaurea, col = c("blue","green","red","orange"), lwd = 2)
#  plotAddSpiders(cda.centaurea, col = c(rgb(0,0,255,max=255,alpha=130), # blue
#                                        rgb(0,255,0,max=255,alpha=130), # green
#                                        rgb(255,0,0,max=255,alpha=130), # red
#                                        rgb(255,102,0,max=255,alpha=130))) # orange

## ----cda.points.ellipses2, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3----
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(cda.centaurea, col = c("blue","green","red","orange"), cex = 0.4)
plotAddEllipses(cda.centaurea, col = c("blue","green","red","orange"), lwd = 2)
plotAddSpiders(cda.centaurea, col = c(rgb(0,0,255,max=255,alpha=130), # blue
                                      rgb(0,255,0,max=255,alpha=130), # green
                                      rgb(255,0,0,max=255,alpha=130), # red
                                      rgb(255,102,0,max=255,alpha=130))) # orange

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotCharacters(cda.centaurea, cex = 1.2)

## ----cad.characters, echo = FALSE, eval=TRUE, fig.width= 3.8, fig.height=3.3----
graphics::par(mar=c(3, 4.1, 2, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotCharacters(cda.centaurea, cex = 0.7, xlim=c(-1, 1),ylim=c(-1, 1), cex.main=0.9)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotBiplot(cda.centaurea, col = c("blue","green","red","orange"), cex = 0.5,
#             pch = c(8,17,20,18), legend = T, ncol = 2, legend.pos="bottomright")

## ----plotBiplot3, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3---------
graphics::par(mar=c(2.8, 4.1, 0, 2.1), mgp=c(1.5, 0.5, 0), cex.axis=1, cex.lab=0.8, lwd=0.9)
plotBiplot(cda.centaurea, col = c("blue","green","red","orange"), cex = 0.5,
           pch = c(8,17,20,18), legend = T, ncol = 2, legend.pos="bottomright")

## ----include=F----------------------------------------------------------------
options(max.print = 100)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  exportRes(cda.centaurea$totalCanonicalStructure, file = "centaurea_TCS.txt")

## ----echo = FALSE , eval=TRUE-------------------------------------------------
cda.centaurea$totalCanonicalStructure = cda.centaurea$totalCanonicalStructure[c(1,2,3,6,11,13,14,16, 17, 18,21,24,25),]

## ----echo = TRUE, eval=TRUE---------------------------------------------------
# For demonstration purposes only. Only a subset of data is displayed.
cda.centaurea$totalCanonicalStructure


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plot3Dpoints(cda.centaurea, col = c("blue","green","red","orange"),
#                              phi = 12, theta = 25)

## ----cda.3D, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3--------------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plot3Dpoints(cda.centaurea, col = c("blue","green","red","orange"), phi = 12, theta = 25)

## ----echo = TRUE, eval=TRUE, out.width = '320px'------------------------------
stPsHybr = removeTaxon(centaurea, taxonName = "ph")
cda.stPsHybr = cda.calc(stPsHybr)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(cda.stPsHybr, col = c("blue","red","orange"), pch = c(8,20,18),
#              legend = T, ncol = 2, legend.pos="bottomright")

## ----cda.stPsHybr1, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(cda.stPsHybr, col = c("blue","red","orange"), pch = c(8,20,18), 
            legend = F)
plotAddLegend(cda.stPsHybr, col = c("blue","red","orange"), box.lwd = 0.9,
              x = "bottomright", cex = 0.75, ncol = 2)

## ----include=F----------------------------------------------------------------
options(max.print = 38)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
cda.stPsHybr$totalCanonicalStructure

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotBiplot(cda.stPsHybr, col = c("blue","red","orange"), pch = c(8,20,18),
#              legend = T, ncol = 2, legend.pos="bottomright")

## ----plotBiplot5, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3---------
graphics::par(mar=c(2.8, 4.1, 0, 2.1), mgp=c(1.5, 0.5, 0), cex.axis=1, cex.lab=0.8, lwd=0.9)
plotBiplot(cda.stPsHybr, col = c("blue","red","orange"), pch = c(8,20,18), 
            legend = T, ncol = 2, legend.pos="bottomleft")


## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  
#  cda.stPs_passiveHybr = cda.calc(stPsHybr, passiveSamples = "hybr")
#  
#  plotPoints(cda.stPs_passiveHybr, legend = T, breaks = 0.2,
#                  col = c(rgb(0,0,255, alpha=255, max=255), # blue
#                          rgb(255,0,0, alpha=160, max=255), # red
#                          rgb(255,102,0, alpha=160, max=255))) # orange,

## ----cda.stPsHybr2, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)

cda.stPs_passiveHybr = cda.calc(stPsHybr, passiveSamples = "hybr")

plotPoints(cda.stPs_passiveHybr, legend = F, breaks = 0.2,
                col = c(rgb(0,0,255, alpha=255, max=255), # blue
                        rgb(255,0,0, alpha=160, max=255), # red
                        rgb(255,102,0, alpha=160, max=255))) # orange, 
plotAddLegend(cda.stPs_passiveHybr, pch = 22, col = "black", box.lwd = 0.9, pt.bg= c("blue","red","orange"),
              x = "topright", cex = 0.8, ncol = 1)

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  plotPoints(cda.stPs_passiveHybr, breaks = 0.2,
#                  col = c(rgb(0,0,0, alpha=255, max=255), # hybr - black
#                          rgb(255,255,255, alpha=80, max=255), # ps - white
#                          rgb(255,255,255, alpha=80, max=255))) # st - white

## ----cda.stPsHybr3, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-------
graphics::par(mar=c(3, 4.1, 0, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
plotPoints(cda.stPs_passiveHybr, breaks = 0.2,
                col = c(rgb(0,0,0, alpha=255, max=255), # hybr - black
                        rgb(255,255,255, alpha=80, max=255), # ps - white
                        rgb(255,255,255, alpha=80, max=255))) # st - white 

## ----include=F----------------------------------------------------------------
options(max.print = 460)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
stepdisc.calc(centaurea)

partialCent = keepCharacter(centaurea, c("MLW", "ML", "IW", "LS", "IV",
                          "MW", "MF", "AP", "IS", "LBA", "LW", "AL", "ILW",
                          "LBS","SFT", "CG", "IL", "LM", "ALW", "AW", "SF"))

descrTaxon(partialCent, format = "$SD")

partialCent$data[ partialCent$Taxon == "hybr", "LM" ][1] =
             partialCent$data[partialCent$Taxon=="hybr","LM"][1] + 0.000001
partialCent$data[ partialCent$Taxon == "ph", "IV" ][1] =
             partialCent$data[partialCent$Taxon=="ph","IV"][1] + 0.000001
partialCent$data[ partialCent$Taxon == "st", "LBS"][1] =
             partialCent$data[partialCent$Taxon=="st","LBS"][1] + 0.000001

## ----echo = TRUE, eval=TRUE---------------------------------------------------
boxMTest(partialCent)


## ----echo = TRUE, eval=TRUE---------------------------------------------------
classifRes.lda = classif.lda(partialCent)

## ----include=F----------------------------------------------------------------
options(max.print = 185)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
summary(classifRes.lda)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
classif.matrix(classifRes.lda, level = "taxon")

## ----include=F----------------------------------------------------------------
options(max.print = 250)

## ----echo = TRUE, eval=TRUE---------------------------------------------------
classif.matrix(classifRes.lda, level = "pop")

## ----echo = TRUE, eval=TRUE---------------------------------------------------
classifRes.qda = classif.qda(partialCent)


classif.matrix(classifRes.qda, level = "taxon")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  classif_qda = classif.matrix(classifRes.qda, level = "indiv")
#  
#  exportRes(object = classif_qda, file = "qda_classifMatrix.txt")

## ----echo = FALSE, eval = TRUE------------------------------------------------
knn.select<-function(object, crossval="indiv"){
  k = as.numeric(1:30)
  ksel = matrix(c(419,419,419,419,419,419,419,419,419,419,416,434,420,407,418,423,426,423,413,417,441,442,438,441,440,440,443,441,439,442,446,447,455,447,446,443,445,435,445,444,457,458,457,459,457,458,456,461,458,462,455,457,452,456,455,459,454,459,451,464,460,458,462,455,461,459,459,458,459,460,458,458,460,463,458,456,461,465,456,464,462,461,458,457,459,460,460,458,461,460,464,459,463,461,460,464,462,462,459,463,457,456,461,459,459,455,458,458,458,458,459,464,466,465,464,458,460,462,462,460,462,461,460,460,462,464,462,463,464,461,460,462,462,464,461,460,460,458,460,463,464,467,467,465,466,466,468,466,470,466,459,460,456,463,457,458,457,458,464,457,456,455,458,459,459,454,455,455,456,457,452,458,454,460,457,456,458,459,453,453,456,457,459,456,457,458,457,454,455,453,450,454,455,451,455,452,452,451,455,454,455,453,455,453,456,454,454,455,454,454,454,455,453,453,453,454,450,452,450,455,454,453,455,455,457,455,455,455,453,455,458,455,454,455,456,457,454,455,457,455,459,460,458,458,457,455,458,460,458,460,458,461,459,460,461,459,459,456,457,458,461,463,461,461,460,463,461,460,459,459,456,455,455,457,454,458,457,454,456,456,453,450,451,453,456,451,453,456,450,456,451,449,453,452,450,449,450,451,448,450), byrow = T, ncol = 10)
  
  ksel = t(ksel)
  
  for (j in seq(10,100,10))
  {
    cat("Tested ", j, "% of Ks \n")
  }
  
  kselmean = apply(ksel, MARGIN = 2, FUN = mean)
  kselmax = apply(ksel, MARGIN = 2, FUN = max)
  kselmin = apply(ksel, MARGIN = 2, FUN = min)
  graphics::plot(kselmean,type="p",pch=16,xlab="K",ylab="correct classifications", ylim=c(min(kselmin),max(kselmax)))

  sapply(k[-1],function(x) graphics::arrows(x, kselmin[x], x, kselmax[x], code = 3, angle = 90, length = 0.07))

  cat("\nThe highest number of correct classifications is at k = 15.\n")
}

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  knn.select(partialCent, crossval = "pop")

## ----knn, echo = FALSE, eval=TRUE, fig.width= 5, fig.height=3-----------------
graphics::par(mar=c(3, 4.1, 0.5, 2.1), mgp=c(1.7, 0.6, 0), cex.axis=0.8, cex.lab=0.8, lwd=0.9)
knn.select(partialCent, crossval = "pop")

## ----echo = TRUE, eval=TRUE---------------------------------------------------
classifRes.knn = classif.knn(partialCent, crossval = "pop", k = 15)

classif.matrix(classifRes.knn, level = "taxon")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  popClassifMatrix = classif.matrix(classifRes.knn, level = "taxon")
#  
#  exportRes(popClassifMatrix, file = "clipboard")

## ----echo = TRUE, eval=TRUE---------------------------------------------------
trainingSet = removePopulation(partialCent, populationName = "LES")
typeSpecimen = keepSample(partialCent, "LES1116")

classifSample.lda(typeSpecimen, trainingSet)

classifSample.qda(typeSpecimen, trainingSet)

classifSample.knn(typeSpecimen, trainingSet, k = 4)


## ----echo = FALSE, eval=TRUE--------------------------------------------------
par(old.par)

