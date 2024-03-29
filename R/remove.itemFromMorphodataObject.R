#' Remove items (Taxa, Populations, morphological characters) from an morphodata object
#' @export
removeTaxon <- function(object, taxonName) {
  .checkClass(object, "morphodata")

  # skontroluj ci object ma taxName
  for (tax in taxonName) {
    if (! (tax %in% object$Taxon)) stop(paste("Taxon \"", tax , "\" does not exist.", sep = ""), call. = FALSE)
  }

  return(.removeByColumn(object, "Taxon", taxonName))
}


#' @rdname removeTaxon
#' @export
removePopulation <- function(object, populationName) {
  .checkClass(object, "morphodata")

  # skontroluj ci object ma popname
  for (pop in populationName) {
    if (! (pop %in% object$Population)) stop(paste("Population \"", pop , "\" does not exist.", sep = ""), call. = FALSE)
  }

  return(.removeByColumn(object, "Population", populationName))
}

#' @rdname removeTaxon
#' @export
removeSample <- function(object, sampleName = NULL, missingPercentage = NA) {
  .checkClass(object, "morphodata")

  # nemozu byt oba nenulova
  if (!is.na(missingPercentage) && !is.null(sampleName)) stop("Not implemented, use arguments 'sampleName' and 'missingPercentage' in separate runs.", call. = FALSE)

  if (!is.null(sampleName)) {

    # if (!all(is.character(sampleName))) stop("'sampleName' is not a string.", call. = FALSE)

    # skontroluj ci object ma popname
    for (samp in sampleName) {
      if (! (samp %in% object$ID)) stop(paste("Sample \"", samp , "\" does not exist.", sep = ""), call. = FALSE)
    }

    return(.removeByColumn(object, "ID", sampleName))
  }

  if (!is.na(missingPercentage)) {

    if (!is.numeric(missingPercentage)) stop("'missingPercentage' is not numeric.", call. = FALSE)

    aboveTreshold = rowMeans(is.na(object$data)) > missingPercentage

    newObject = .newMorphodata()
    newObject$ID = droplevels( object$ID[!aboveTreshold] )
    newObject$Population = droplevels( object$Population[!aboveTreshold] )
    newObject$Taxon = droplevels( object$Taxon[!aboveTreshold] )
    newObject$data = object$data[!aboveTreshold, ]

    return(newObject)
  }

  # nemozu byt oba nulove
  if (is.na(missingPercentage) && is.null(sampleName)) stop("One of the arguments: 'sampleName' or 'missingPercentage' has to be specified.", call. = FALSE)


}


#' @rdname removeTaxon
#' @export
removeCharacter <- function(object, characterName) {
  .checkClass(object, "morphodata")

  # check existence of CH
  for (ch in characterName) {
    if (! (ch %in% colnames(object$data))) stop(paste("Character \"", ch , "\" does not exist.", sep = ""), call. = FALSE)
  }

  # character - moze byt i viac
  toRemove = array(data = NA, dim = 0)
  for (ch in characterName) {
    toRemove = c(toRemove, which(colnames(object$data) == ch) )
  }


  if (length(toRemove) == dim(object$data)[2]-1) {

    colName = colnames(object$data)[-toRemove]
    object$data = data.frame(object$data[ ,-toRemove])
    colnames(object$data) = colName

  } else {
    object$data = object$data[ ,-toRemove]
  }

  return(object)
}


# internal
.removeByColumn <- function(object, column, groupName) {
  # obj je triedy morfodata, skontrolovane vyssie

  # groupName moze byt i viac
  toRemove = array(data = NA, dim = 0)
  for (name in groupName) {
    toRemove = c(toRemove, which( unlist(object[column]) %in% name) )
  }

  newObject = .newMorphodata()
  newObject$ID = droplevels( object$ID[-toRemove] )
  newObject$Population = droplevels( object$Population[-toRemove] )
  newObject$Taxon = droplevels( object$Taxon[-toRemove] )
  newObject$data = object$data[-toRemove, ]

  return(newObject)
}




