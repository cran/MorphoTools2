#' Hierarchical clustering
#' @export
clust <- function(object, distMethod = "Euclidean", clustMethod = "UPGMA", binaryChs = NULL, nominalChs = NULL, ordinalChs = NULL) {
  .checkClass(object, "morphodata")


  supportedClustMethods = c("ward.D", "ward.D2", "ward", "single", "complete", "average", "UPGMA",
                            "Mcquitty", "WPGMA", "median", "WPGMC", "centroid", "UPGMC")


  # skontroluj argumenty
  # if (! (distMethod %in% supportedDistMethods)) stop(paste("distMethod = \"", distMethod , "\" is not supported.", sep = ""), call. = FALSE)
  if (! (clustMethod %in% supportedClustMethods)) stop(paste("clustMethod \"", clustMethod , "\" is not supported.", sep = ""), call. = FALSE)

  if (any(is.na(object$data))) warning("Values of some characters are NA.", call. = FALSE)


  distances = .calcDistance(object, distMethod = distMethod, center = TRUE, scale = TRUE, binaryChs = binaryChs, nominalChs = nominalChs, ordinalChs = ordinalChs)

  # v parametri method mozme dostat akukolvek metodu, ktora je platna pre hclust
  if (clustMethod == "UPGMA") clustMethod = "average"
  if (clustMethod == "ward") clustMethod = "ward.D"
  if (clustMethod == "WPGMA" | clustMethod == "Mcquitty") clustMethod = "mcquitty"
  if (clustMethod == "WPGMC") clustMethod = "median"
  if (clustMethod == "UPGMC") clustMethod = "centroid"

  clustering = stats::hclust(distances, method = clustMethod)

  return(clustering)
}






