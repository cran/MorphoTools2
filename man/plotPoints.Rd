\name{plotPoints}
\alias{plotPoints}

\title{The Default Scatterplot Function}

\description{
A generic function for plotting ordination scores stored in \code{\link{pcadata}}, \code{\link{pcoadata}}, \code{\link{nmdsdata}}, and \code{\link{cdadata}} objects.
}

\usage{
plotPoints(result, axes = c(1,2), xlab = NULL, ylab = NULL,
      pch = 16, col = "black", pt.bg = "white", breaks = 1,
      ylim = NULL, xlim = NULL, labels = FALSE, legend = FALSE,
      legend.pos = "topright", ncol = 1, ...)
}

\arguments{
\item{result}{an object of class \code{\link{pcadata}}, \code{\link{pcoadata}}, \code{\link{nmdsdata}}, or \code{\link{cdadata}}.}
\item{axes}{x, y axes of plot.}
\item{xlab, ylab}{a title of the respective axes.}
\item{pch}{a vector of plotting characters or symbols: see \code{\link{points}}.}
\item{col}{the colours for points. Multiple colours can be specified so that each taxon can be given its own colour. If there are fewer colours than taxa, they are recycled in the standard fashion.}
\item{pt.bg}{the background colours for points. Multiple colours can be specified, as above.}
\item{breaks}{a numeric, giving the width of one histogram bar.}
\item{xlim, ylim}{the range of x and y axes.}
\item{labels}{logical, if \code{TRUE}, labels are displayed. Only restricted number of parameters are supported. For more precise labels plotting, use \code{\link{plotAddLabels.points}} directly.}
\item{legend}{logical, if \code{TRUE}, legend is displayed. Only restricted number of legend parameters are supported. For more precise legend plotting, use \code{\link{plotAddLegend}} directly.}
\item{legend.pos}{a single keyword from the list \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"}, and \code{"center"}, to be used to position the legend.}
\item{ncol}{the number of columns in which to set the legend items.}
\item{...}{further arguments to be passed to \code{\link{plot.default}} or other graphical parameters in \code{\link{par}}.}
}

\value{
None. Used for its side effect of producing a plot.
}

\details{
This generic method holds separate implementations of plotting points for \code{\link{pcadata}}, \code{\link{pcoadata}}, \code{\link{nmdsdata}}, and \code{\link{cdadata}} objects.
If only one axis exists, sample scores are displayed as a histogram.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plotPoints(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"),
  pch = c(20, 17, 8, 21), pt.bg = "orange", legend = TRUE, legend.pos = "bottomright")

plotPoints(pcaRes, main = "My PCA plot", cex = 0.8)

cdaRes = cda.calc(centaurea)

plotPoints(cdaRes, col = c("red", "green", "blue", "red"),
  pch = c(20, 17, 8, 21), pt.bg = "orange", legend = TRUE)
}






