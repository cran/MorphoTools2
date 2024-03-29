\name{cormat}
\alias{cormat}
\alias{cormatSignifTest}

\title{Correlations of Characters}

\description{
The \code{cormat} function calculates the matrix of the correlation coefficients of the characters.
}

\usage{
cormat(object, method = "Pearson")

cormatSignifTest(object, method = "Pearson", alternative = "two.sided")
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}

\item{method}{a character string indicating which correlation coefficient is to be used for the test.
One of \code{"Pearson"} (default), or \code{"Ppearman"} can be applied.}

\item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"} (default),
\code{"greater"} (positive association) or \code{"less"} (negative association).}
}

\value{
A \code{data.frame}, storing correlation coefficients for each pair of morphological characters.
}

\details{
This function returns table with pairwise correlation coefficients for each pair of morphological characters. The result is formatted as a \code{data.frame} to allow export with the \code{\link{exportRes}} function.

Significance tests are usually unnecessary for morphometric analysis. Anyway, if tests are needed, they can be computed using the \code{\link{cormatSignifTest}} function.
}
\examples{
data(centaurea)

correlations.p = cormat(centaurea, method = "Pearson")
correlations.s = cormat(centaurea, method = "Spearman")

\donttest{\dontrun{exportRes(correlations.p, file = "correlations.pearson.txt")}}
\donttest{\dontrun{exportRes(correlations.s, file = "correlations.spearman.txt")}}

correlations.p = cormatSignifTest(centaurea, method = "Pearson")
}
