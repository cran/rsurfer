#' RSurfer
#'
#' A package for interfacing between R and 'Freesurfer'
#'
#' This package provides functionality for importing MRI data which has been processed with 'Freesurfer' into R. It also provides functions for manipulating this data within R such as functionality to perform intracranial volume normalisation and field manipulation.
"_PACKAGE"

options(repos = c(CRAN="http://cran.r-project.org"))

#' Set 'Freesurfer' Home
#'
#' This command is used to set the base directory where 'Freesurfer' is installed. My installation of 'Freesurfer' is located in: "/Applications/freesurfer/"; thus for my installation location setfshome("/Applications/freesurfer/") would be used.
#'
#' @param freesurferDirectory The directory 'Freesurfer' is installed to
#' @return None
#' @examples
#' setfshome("/Applications/freesurfer")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
setfshome <- function(freesurferDirectory) {
  options("rsurfer-FREESURFER_HOME"=freesurferDirectory)
}

#' Get 'Freesurfer' Home
#'
#' This command is used to get the base directory where 'Freesurfer' is installed as set using the command setfshome().
#'
#' @return The directory 'Freesurfer' is installed
#' @examples
#' setfshome("/Applications/freesurfer/")
#' getfshome()
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
getfshome <- function() {
  return(getOption("rsurfer-FREESURFER_HOME"))
}

#' Get 'Freesurfer' Version
#'
#' This command is used to get the version number of 'Freesurfer' which a certain subject was processed with
#'
#' @param subjectDir The directory of the subject to get the 'Freesurfer' version of
#' @return The version number
#' @examples
#' \dontrun{
#' getfsversion("/Users/alex/Desktop/Subjects/002_S_0413/")
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
getfsversion <- function(subjectDir) {
  buildStampDir <- paste(subjectDir,"scripts","build-stamp.txt",sep="/")
  content <- readChar(buildStampDir, file.info(buildStampDir)$size)
  x <- stringr::str_locate(content,"[v]([0-9][.])+[0-9]")
  return(substr(content, x[1,][1],))
}

#' Search For Abnormalities (Rows)
#'
#' Looks for rows (subjects) which have been exported from 'Freesurfer' and may cause classification problems, for example, rows with NAs in
#'
#' @param data The data frame imported using fsimport
#' @param verbose Whether the print debug information
#' @return Indices of abnormal rows
#' @examples
#' data <- generaterandomsubjects()
#' searchforabnormalities.rows(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
searchforabnormalities.rows <- function(data, verbose=T) {
  abRows <- c()

  for (i in 1:nrow(data)) {
    if (!stats::complete.cases(data[i,])) {
      if (verbose)
        print(paste("NA found in row ", i, " Subject ID (", rownames(data)[i], ")", sep="" ))
      abRows <- c(abRows,i)
    }
  }
  return(abRows)
}

#' Search For Abnormalities (Columns)
#'
#' Looks for columns which have been exported from 'Freesurfer' and may cause classification problems, for example, my personal abnormal columns are Left.WM.hypointensities, Right.WM.hypointensities, Left.non.WM.hypointensities, Right.WM.hypointensities only have values of zero
#'
#' @param data The data frame imported using fsimport
#' @param verbose Whether the print debug information
#' @return Indices of abnormal columns
#' @examples
#' data <- generaterandomsubjects()
#' searchforabnormalities.cols(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
searchforabnormalities.cols <- function(data, verbose=T) {
  #Common error is the column is all zeroes
  #So compare the min range and max range

  abCols <- c()

  for (i in 1:ncol(data)) {
    if (is.numeric(data[,i])) {
      r <- range(data[,i])
      if (r[1] == r[2]) {
        if (verbose)
          print(paste("Range of column ", i, " is the same (", names(data)[i], ")", sep="" ))
        abCols <- c(abCols,i)
      }
    }
  }

  return(abCols)
}

#' Remove Abnormalities
#'
#' Removes columns and rows which have been exported from 'Freesurfer' and may cause classification problems
#'
#' @param df The data frame imported using fsimport
#' @param verbose Whether the print debug information
#' @return The data frame with abnormal rows and columns removed
#' @examples
#' data <- generaterandomsubjects()
#' data <- removeabnormalrowsandcols(data,T)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
removeabnormalrowsandcols <- function(df,verbose) {
  abRows <- searchforabnormalities.rows(df,verbose=verbose)
  if (!is.null(abRows))
    df <- df[-abRows,]
  abCols <- searchforabnormalities.cols(df, verbose=verbose)
  if (!is.null(abCols))
    df <- df[,-abCols]
  return(df)
}

#' Extract Hippocampal Volumes
#'
#' This command takes a data frame as input and extracts all the hippocampal volumes from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The hippocampal volumes
#' @examples
#' data <- generaterandomsubjects()
#' extract.hippocampalvolumes(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.hippocampalvolumes <- function(data, additionalFields = c()) {
  hnames <- c("Left.Hippocampus.HS", "left.presubiculum", "left.CA1", "left.CA2.3", "left.fimbria", "left.subiculum", "left.CA4.DG",
              "left.hippocampal.fissure", "Right.Hippocampus.HS", "right.presubiculum", "right.CA1", "right.CA2.3",
              "right.fimbria", "right.subiculum", "right.CA4.DG", "right.hippocampal.fissure")

  hnames <- c(hnames, additionalFields)

  return(data[,which(names(data) %in% hnames)])
}

#' Extract All MRI Brain Features
#'
#' This command takes a data frame as input and extracts all the features segmented by 'Freesurfer'. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The MRI brain features
#' @examples
#' data <- generaterandomsubjects()
#' extract.brain.features(data)
#' @author Fabio Cappello
#' @export
extract.brain.features <- function(data, additionalFields = c()) {
  return(cbind(extract.hippocampalvolumes(data), extract.cortical(data), extract.subcorticalvolumes(data), data[,additionalFields]))
}

#' Extract Subcortical Volumes
#'
#' This command takes a data frame as input and extracts all the subcortical volumes from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The subcortical volumes
#' @examples
#' data <- generaterandomsubjects()
#' extract.subcorticalvolumes(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.subcorticalvolumes <- function(data, additionalFields = c()) {
  snames <- c("Left.Lateral.Ventricle","Left.Inf.Lat.Vent","Left.Cerebellum.White.Matter","Left.Cerebellum.Cortex",
              "Left.Thalamus.Proper","Left.Caudate","Left.Putamen","Left.Pallidum","X3rd.Ventricle","X4th.Ventricle",
              "Brain.Stem","Left.Hippocampus","Left.Amygdala","CSF","Left.Accumbens.area","Left.VentralDC","Left.vessel",
              "Left.choroid.plexus","Right.Lateral.Ventricle","Right.Inf.Lat.Vent","Right.Cerebellum.White.Matter",
              "Right.Cerebellum.Cortex","Right.Thalamus.Proper","Right.Caudate","Right.Putamen","Right.Pallidum","Right.Hippocampus",
              "Right.Amygdala","Right.Accumbens.area","Right.VentralDC","Right.vessel","Right.choroid.plexus","X5th.Ventricle",
              "WM.hypointensities","Left.WM.hypointensities","Right.WM.hypointensities","non.WM.hypointensities",
              "Left.non.WM.hypointensities","Right.non.WM.hypointensities","Optic.Chiasm","CC_Posterior","CC_Mid_Posterior",
              "CC_Central","CC_Mid_Anterior","CC_Anterior","BrainSegVol","BrainSegVolNotVent","BrainSegVolNotVentSurf",
              "lhCortexVol","rhCortexVol","CortexVol","lhCorticalWhiteMatterVol","rhCorticalWhiteMatterVol","CorticalWhiteMatterVol",
              "SubCortGrayVol","TotalGrayVol","SupraTentorialVol","SupraTentorialVolNotVent","SupraTentorialVolNotVentVox",
              "MaskVol","BrainSegVol.to.eTIV","MaskVol.to.eTIV","lhSurfaceHoles","rhSurfaceHoles","SurfaceHoles",
              "EstimatedTotalIntraCranialVol")

  snames <- c(snames, additionalFields)

  return(data[,which(names(data) %in% snames)])
}

#' Extract Cortical Fields
#'
#' This command takes a data frame as input and extracts all the cortical fields (cortical volumes, cortical thicknesses, cortical surface areas, standard deviations of cortical thicknesses) from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The cortical fields
#' @examples
#' data <- generaterandomsubjects()
#' extract.cortical(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.cortical <- function(data, additionalFields = c()) {

  cnames <- c("lh.bankssts.thickness","lh.caudalanteriorcingulate.thickness","lh.caudalmiddlefrontal.thickness",
              "lh.cuneus.thickness","lh.entorhinal.thickness","lh.fusiform.thickness","lh.inferiorparietal.thickness",
              "lh.inferiortemporal.thickness","lh.isthmuscingulate.thickness","lh.lateraloccipital.thickness",
              "lh.lateralorbitofrontal.thickness","lh.lingual.thickness","lh.medialorbitofrontal.thickness",
              "lh.middletemporal.thickness","lh.parahippocampal.thickness","lh.paracentral.thickness",
              "lh.parsopercularis.thickness","lh.parsorbitalis.thickness","lh.parstriangularis.thickness",
              "lh.pericalcarine.thickness","lh.postcentral.thickness","lh.posteriorcingulate.thickness",
              "lh.precentral.thickness","lh.precuneus.thickness","lh.rostralanteriorcingulate.thickness",
              "lh.rostralmiddlefrontal.thickness","lh.superiorfrontal.thickness","lh.superiorparietal.thickness",
              "lh.superiortemporal.thickness","lh.supramarginal.thickness","lh.frontalpole.thickness",
              "lh.temporalpole.thickness","lh.transversetemporal.thickness","lh.insula.thickness","lh.MeanThickness.thickness",
              "rh.bankssts.thickness","rh.caudalanteriorcingulate.thickness","rh.caudalmiddlefrontal.thickness",
              "rh.cuneus.thickness","rh.entorhinal.thickness","rh.fusiform.thickness","rh.inferiorparietal.thickness",
              "rh.inferiortemporal.thickness","rh.isthmuscingulate.thickness","rh.lateraloccipital.thickness",
              "rh.lateralorbitofrontal.thickness","rh.lingual.thickness","rh.medialorbitofrontal.thickness",
              "rh.middletemporal.thickness","rh.parahippocampal.thickness","rh.paracentral.thickness",
              "rh.parsopercularis.thickness","rh.parsorbitalis.thickness","rh.parstriangularis.thickness",
              "rh.pericalcarine.thickness","rh.postcentral.thickness","rh.posteriorcingulate.thickness",
              "rh.precentral.thickness","rh.precuneus.thickness","rh.rostralanteriorcingulate.thickness",
              "rh.rostralmiddlefrontal.thickness","rh.superiorfrontal.thickness","rh.superiorparietal.thickness",
              "rh.superiortemporal.thickness","rh.supramarginal.thickness","rh.frontalpole.thickness",
              "rh.temporalpole.thickness","rh.transversetemporal.thickness","rh.insula.thickness",
              "rh.MeanThickness.thickness","lh.bankssts.thicknessstd","lh.caudalanteriorcingulate.thicknessstd",
              "lh.caudalmiddlefrontal.thicknessstd","lh.cuneus.thicknessstd","lh.entorhinal.thicknessstd",
              "lh.fusiform.thicknessstd","lh.inferiorparietal.thicknessstd","lh.inferiortemporal.thicknessstd",
              "lh.isthmuscingulate.thicknessstd","lh.lateraloccipital.thicknessstd","lh.lateralorbitofrontal.thicknessstd",
              "lh.lingual.thicknessstd","lh.medialorbitofrontal.thicknessstd","lh.middletemporal.thicknessstd",
              "lh.parahippocampal.thicknessstd","lh.paracentral.thicknessstd","lh.parsopercularis.thicknessstd",
              "lh.parsorbitalis.thicknessstd","lh.parstriangularis.thicknessstd","lh.pericalcarine.thicknessstd",
              "lh.postcentral.thicknessstd","lh.posteriorcingulate.thicknessstd","lh.precentral.thicknessstd",
              "lh.precuneus.thicknessstd","lh.rostralanteriorcingulate.thicknessstd","lh.rostralmiddlefrontal.thicknessstd",
              "lh.superiorfrontal.thicknessstd","lh.superiorparietal.thicknessstd","lh.superiortemporal.thicknessstd",
              "lh.supramarginal.thicknessstd","lh.frontalpole.thicknessstd","lh.temporalpole.thicknessstd",
              "lh.transversetemporal.thicknessstd","lh.insula.thicknessstd","rh.bankssts.thicknessstd",
              "rh.caudalanteriorcingulate.thicknessstd","rh.caudalmiddlefrontal.thicknessstd","rh.cuneus.thicknessstd",
              "rh.entorhinal.thicknessstd","rh.fusiform.thicknessstd","rh.inferiorparietal.thicknessstd",
              "rh.inferiortemporal.thicknessstd","rh.isthmuscingulate.thicknessstd","rh.lateraloccipital.thicknessstd",
              "rh.lateralorbitofrontal.thicknessstd","rh.lingual.thicknessstd","rh.medialorbitofrontal.thicknessstd",
              "rh.middletemporal.thicknessstd","rh.parahippocampal.thicknessstd","rh.paracentral.thicknessstd",
              "rh.parsopercularis.thicknessstd","rh.parsorbitalis.thicknessstd","rh.parstriangularis.thicknessstd",
              "rh.pericalcarine.thicknessstd","rh.postcentral.thicknessstd","rh.posteriorcingulate.thicknessstd",
              "rh.precentral.thicknessstd","rh.precuneus.thicknessstd","rh.rostralanteriorcingulate.thicknessstd",
              "rh.rostralmiddlefrontal.thicknessstd","rh.superiorfrontal.thicknessstd","rh.superiorparietal.thicknessstd",
              "rh.superiortemporal.thicknessstd","rh.supramarginal.thicknessstd","rh.frontalpole.thicknessstd",
              "rh.temporalpole.thicknessstd","rh.transversetemporal.thicknessstd","rh.insula.thicknessstd","lh.bankssts.area",
              "lh.caudalanteriorcingulate.area","lh.caudalmiddlefrontal.area","lh.cuneus.area","lh.entorhinal.area",
              "lh.fusiform.area","lh.inferiorparietal.area","lh.inferiortemporal.area","lh.isthmuscingulate.area",
              "lh.lateraloccipital.area","lh.lateralorbitofrontal.area","lh.lingual.area","lh.medialorbitofrontal.area",
              "lh.middletemporal.area","lh.parahippocampal.area","lh.paracentral.area","lh.parsopercularis.area",
              "lh.parsorbitalis.area","lh.parstriangularis.area","lh.pericalcarine.area","lh.postcentral.area",
              "lh.posteriorcingulate.area","lh.precentral.area","lh.precuneus.area","lh.rostralanteriorcingulate.area",
              "lh.rostralmiddlefrontal.area","lh.superiorfrontal.area","lh.superiorparietal.area","lh.superiortemporal.area",
              "lh.supramarginal.area","lh.frontalpole.area","lh.temporalpole.area","lh.transversetemporal.area",
              "lh.insula.area","lh.WhiteSurfArea.area","rh.bankssts.area","rh.caudalanteriorcingulate.area",
              "rh.caudalmiddlefrontal.area","rh.cuneus.area","rh.entorhinal.area","rh.fusiform.area","rh.inferiorparietal.area",
              "rh.inferiortemporal.area","rh.isthmuscingulate.area","rh.lateraloccipital.area","rh.lateralorbitofrontal.area",
              "rh.lingual.area","rh.medialorbitofrontal.area","rh.middletemporal.area","rh.parahippocampal.area",
              "rh.paracentral.area","rh.parsopercularis.area","rh.parsorbitalis.area","rh.parstriangularis.area",
              "rh.pericalcarine.area","rh.postcentral.area","rh.posteriorcingulate.area","rh.precentral.area",
              "rh.precuneus.area","rh.rostralanteriorcingulate.area","rh.rostralmiddlefrontal.area","rh.superiorfrontal.area",
              "rh.superiorparietal.area","rh.superiortemporal.area","rh.supramarginal.area","rh.frontalpole.area",
              "rh.temporalpole.area","rh.transversetemporal.area","rh.insula.area","rh.WhiteSurfArea.area","lh.bankssts.volume",
              "lh.caudalanteriorcingulate.volume","lh.caudalmiddlefrontal.volume","lh.cuneus.volume","lh.entorhinal.volume",
              "lh.fusiform.volume","lh.inferiorparietal.volume","lh.inferiortemporal.volume","lh.isthmuscingulate.volume",
              "lh.lateraloccipital.volume","lh.lateralorbitofrontal.volume","lh.lingual.volume","lh.medialorbitofrontal.volume",
              "lh.middletemporal.volume","lh.parahippocampal.volume","lh.paracentral.volume","lh.parsopercularis.volume",
              "lh.parsorbitalis.volume","lh.parstriangularis.volume","lh.pericalcarine.volume","lh.postcentral.volume",
              "lh.posteriorcingulate.volume","lh.precentral.volume","lh.precuneus.volume","lh.rostralanteriorcingulate.volume",
              "lh.rostralmiddlefrontal.volume","lh.superiorfrontal.volume","lh.superiorparietal.volume",
              "lh.superiortemporal.volume","lh.supramarginal.volume","lh.frontalpole.volume","lh.temporalpole.volume",
              "lh.transversetemporal.volume","lh.insula.volume","rh.bankssts.volume","rh.caudalanteriorcingulate.volume",
              "rh.caudalmiddlefrontal.volume","rh.cuneus.volume","rh.entorhinal.volume","rh.fusiform.volume",
              "rh.inferiorparietal.volume","rh.inferiortemporal.volume","rh.isthmuscingulate.volume",
              "rh.lateraloccipital.volume","rh.lateralorbitofrontal.volume","rh.lingual.volume","rh.medialorbitofrontal.volume",
              "rh.middletemporal.volume","rh.parahippocampal.volume","rh.paracentral.volume","rh.parsopercularis.volume",
              "rh.parsorbitalis.volume","rh.parstriangularis.volume","rh.pericalcarine.volume","rh.postcentral.volume",
              "rh.posteriorcingulate.volume","rh.precentral.volume","rh.precuneus.volume","rh.rostralanteriorcingulate.volume",
              "rh.rostralmiddlefrontal.volume","rh.superiorfrontal.volume","rh.superiorparietal.volume",
              "rh.superiortemporal.volume","rh.supramarginal.volume","rh.frontalpole.volume","rh.temporalpole.volume",
              "rh.transversetemporal.volume","rh.insula.volume")


  cnames <- c(cnames, c(additionalFields))

  return(data[,which(names(data) %in% cnames)])
}

#' Extract Cortical Thicknesses
#'
#' This command takes a data frame as input and extracts all the cortical thicknesses from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The cortical thicknesses
#' @examples
#' data <- generaterandomsubjects()
#' extract.corticalthicknesses(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.corticalthicknesses <- function(data, additionalFields = c()) {
  Out <- extract.cortical(data, additionalFields)
  #Field ends in thickness
  Out <- Out[,union(which(grepl("\\w*thickness\\b", names(Out))), which(names(Out) %in% additionalFields))]
  return(Out)
}

#' Extract Cortical Thickness Standard Deviations
#'
#' This command takes a data frame as input and extracts all the cortical thickness standard deviations from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The cortical thickness standard deviations
#' @examples
#' data <- generaterandomsubjects()
#' extract.corticalthicknessstddevs(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.corticalthicknessstddevs <- function(data, additionalFields = c()) {
  Out <- extract.cortical(data, additionalFields)
  #Field ends in thicknessstd
  Out <- Out[,union(which(grepl("\\w*thicknessstd\\b", names(Out))), which(names(Out) %in% additionalFields))]
  return(Out)
}

#' Extract Cortical Volumes
#'
#' This command takes a data frame as input and extracts all the cortical volumes from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The cortical volumes
#' @examples
#' data <- generaterandomsubjects()
#' extract.corticalvolumes(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.corticalvolumes <- function(data, additionalFields = c()) {
  Out <- extract.cortical(data, additionalFields)
  #Field ends in volume
  Out <- Out[,union(which(grepl("\\w*volume\\b", names(Out))), which(names(Out) %in% additionalFields))]
  return(Out)
}

#' Extract Cortical Surface Areas
#'
#' This command takes a data frame as input and extracts all the cortical surface areas from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The cortical surface areas
#' @examples
#' data <- generaterandomsubjects()
#' extract.corticalsurfaceareas(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.corticalsurfaceareas <- function(data, additionalFields = c()) {
  Out <- extract.cortical(data, additionalFields)
  #Field ends in area
  Out <- Out[,union(which(grepl("\\w*area\\b", names(Out))), which(names(Out) %in% additionalFields))]
  return(Out)
}

#' Extract Cortical Thicknesses
#'
#' This command takes a data frame as input and extracts all the cortical thicknesses from this data frame and any other fields specified by the user. Note that the MRIs must be processed with the 'hippocampal-subfields' flag when 'Freesurfer' is invoked.
#'
#' @param data The subject data to extract from
#' @param additionalFields Any additional fields to extract data from
#' @return The cortical thicknesses
#' @examples
#' data <- generaterandomsubjects()
#' extract.corticalthicknesses(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.volumes <- function(data, additionalFields = c()) {
  return(cbind(extract.hippocampalvolumes(data), extract.corticalvolumes(data), extract.subcorticalvolumes(data)))
}


#' Create Subject Distribution Table
#'
#' Creates a data frame with the distributions of the subjects age and gender grouped by the field 'targetClassName', providing an overview of the subjects. Note: requires an 'Age' and 'Gender' column.
#'
#' @param data The subject data to create the table from
#' @param targetClassName The name of the field to group the data by
#' @return The subject distribution table in a data frame
#' @examples
#' all <- generaterandomsubjects(1000)
#' all$Age <- stats::runif(1000,50,80)
#' all <- addrandomgender(all)
#' all <- addrandomdiagnosis(all)
#' subjectDistributionTable(all, "Diagnosis")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
subjectDistributionTable <- function(data, targetClassName) {

  data[,targetClassName] <- as.factor(data[,targetClassName])

  if (!"Age" %in% names(data))
    stop("\"Age\" column does not exist")
  if (!"Gender" %in% names(data))
    stop("\"Gender\" column does not exist")
  if (!"Diagnosis" %in% names(data))
    stop("\"Diagnosis\" column does not exist")

  numLevels <- length(levels(data[,targetClassName]))
  subdata <- data.frame(matrix(nrow = numLevels, ncol = 5))
  names(subdata) <- c(targetClassName, "#Subjects", "%Male", "AgeMean", "AgeSD")
  subdata[,1] <- levels(data[,targetClassName])

  subdata <- t(apply(subdata, 1,
                     function(x) {
                       x[2] <- nrow(data[data[,targetClassName] == x[1],])
                       return(x)
                     }))

  subdata <- t(apply(subdata, 1,
                     function(x) {
                       x[3] <- nrow(data[data[,targetClassName] == x[1] & data$Gender == "Male",])/nrow(data[data[,targetClassName] == x[1],])*100
                       return(x)
                     }))

  subdata <- t(apply(subdata, 1,
                     function(x) {
                       x[4] <- mean(data[data[,targetClassName] == x[1],"Age"])
                       return(x)
                     }))

  subdata <- t(apply(subdata, 1,
                     function(x) {
                       x[5] <- stats::sd(data[data[,targetClassName] == x[1],"Age"])
                       return(x)
                     }))
  return(subdata)
}

#' Convert Subject Distribution Table To LaTeX
#'
#' Converts a subject distribution table created using subjectDistributionTable() into text which can be used in the typesetting language LaTeX. The table created can have its caption and label specified using the respective function arguments. The decimal point rounding can be specified by the function argument roundDP.
#'
#' @param subjectDistributionTable The subject distribution table created using subjectDistributionTable()
#' @param caption The caption to give the table in LaTeX
#' @param label The label to give the table in LaTeX
#' @param roundDP The number of decimal places to round the numbers to on the table
#' @return The LaTeX code representing the subject distribution table
#' @examples
#' all <- generaterandomsubjects(1000)
#' all$Age <- stats::runif(1000,50,80)
#' all <- addrandomgender(all)
#' all <- addrandomdiagnosis(all)
#' sdt <- subjectDistributionTable(all, "Diagnosis")
#' subjectDistributionTableToLatex(subjectDistributionTable = sdt,
#'                                 caption="Subject Distribution Table",
#'                                 label="table:SDT", roundDP=1)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
subjectDistributionTableToLatex <- function(subjectDistributionTable, caption = "Placeholder Caption", label = "table:SubjectDistributionTable", roundDP = 1) {
  df <- subjectDistributionTable

  texLines <- apply(df, 1, function(x) { x[6] <- paste(x[1], "&", x[2], "&", round(as.numeric(x[3]),roundDP),"&",round(as.numeric(x[4]),roundDP)," ${\\pm}$ ", round(as.numeric(x[5]),roundDP+1), "\\\\ \\hline", sep = "");})
  texLines <- paste(texLines, collapse="\n   ")

  tex <- paste("\\begin{table}[]
               \\centering
               \\caption{",caption,"}
               \\label{",label,"}
               \\begin{tabular}{|l|l|l|l|}
               \\hline
               \\textbf{",colnames(df)[1],"} & \\textbf{\\#Subjects} & \\textbf{\\%Male} & \\textbf{Age} ${(\\mu \\pm \\sigma)}$ \\\\ \\hline\n   ",texLines,"
               \\end{tabular}
               \\end{table}", sep = "")

  return(tex)
               }

.getdatestring <- function() {
  return(format(Sys.time(), "%Y%m%d%H%m%s"))
}

#' 'Freesurfer' Import (Serialise)
#'
#' Calls fsimport() and once that data frame is created it is serialised to the file specified by serialisationLocation. However, if this file already exists when the function is run, then it will unserialise the file instead of calling fsimport(). This saves constant running of 'Freesurfer' scripts when running code thus saving execution time for code to run.
#'
#' @param subjectDir The directory containing the subject subdirectories
#' @param serialisationLocation The location where the serialised file is saved to and loaded from
#' @param fields The field groups to use, see fsimport() for more details
#' @param verbose Whether to log the 'Freesurfer' scripts to the R console
#' @return The subject data processed from 'Freesurfer'
#' @examples
#' \dontrun{
#' setfshome("/Applications/freesurfer")
#' fsimport.serialise("~/Subjects", serialisationLocation = "~/data.rds")
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
fsimport.serialise <- function(subjectDir, serialisationLocation, fields = c("lh.cortical.thickness", "rh.cortical.thickness","lh.cortical.volume", "rh.cortical.volume", "lh.cortical.thickness.std", "rh.cortical.thickness.std","lh.cortical.area", "rh.cortical.area", "lh.subcortical", "rh.subcortical", "lh.hippocampal", "rh.hippocampal"), verbose) {

  #does the serialised file exist?
  if (file.exists(serialisationLocation))
    return(readRDS(serialisationLocation))

  df <- fsimport(subjectDir, fields, verbose)
  saveRDS(df, serialisationLocation)

  return(df)
}

#' ADNI Set Files
#'
#' Points the location of two files required to merge the (baseline) Diagnosis, Age, Gender and MMSE for ADNI subjects with the post-processed 'Freesurfer' data
#'
#' @param dxsumLocation The filepath to DXSUM_PDXCONV_ADNIALL.csv
#' @param adnimergeLocation The filepath to ADNIMERGE.csv
#' @examples
#' adni.setfiles("DXSUM_PDXCONV_ADNIALL.csv", "ADNIMERGE.csv")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
adni.setfiles <- function(dxsumLocation, adnimergeLocation) {
  options("rsurfer-DXSUM"=dxsumLocation)
  options("rsurfer-ADNIMERGE"=adnimergeLocation)
}

#' CAD Dementia Set Files
#'
#' Points to the location of the two files required to merged the Diagnosis, Age and Gender for CAD Dementia subjects with the post-processed CAD Dementia data
#'
#' Data can be accessed: \url{https://caddementia.grand-challenge.org/}
#'
#' @param trainLocation The filepath to train.txt
#' @param testLocation The filepath to test.txt
#' caddementia.setfiles("train.txt","test.txt")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
caddementia.setfiles <- function(trainLocation, testLocation) {
  options("rsurfer-CADD.Train"=trainLocation)
  options("rsurfer-CADD.Test"=testLocation)
}

#' IXI Set File
#'
#' Points to the location of the two files required to merge the Diagnosis, Age and Gender for IXI subjects with the post-processed IXI data
#'
#' Data can be accessed: \url{http://brain-development.org/ixi-dataset/}
#'
#' Requires a modification of IXI.xls
#'
#' @param location The filepath to IXI.csv
#' ixi.setfile("IXI.csv")
#' @author Fabio Cappello
#' @export
ixi.setfile <- function(location) {
  options("IXI-csv-path"=location)
}

#' 'IXI' Merge Study Information With 'Freesurfer' Output
#'
#' Merges the external 'IXI' study data (specified with 'IXI' Set Files) with the data generated by 'Freesurfer'
#'
#' Data can be accessed: \url{http://brain-development.org/ixi-dataset/}
#'
#' Requires a modification of IXI.xls
#'
#' @param ixi.mri.data The data frame containing the data generated by 'Freesurfer' of the 'IXI' subjects
#' @param verbose Whether to log information to the console
#' @author Fabio Cappello, Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
ixi.mergewithfreesurferoutput <- function(ixi.mri.data, verbose=T) {
  csv.file <- getOption("IXI-csv-path")

  if(is.null(csv.file)) {
    stop("Please use ixi.setfile(csv.path) to set the location of the demographic information")
  }

  ixi.demographic.data <- utils::read.csv(csv.file)
  ixi.age.sex.data <- ixi.demographic.data[, c(1,2,12)]

  #rename columns
  names(ixi.age.sex.data)[which(names(ixi.age.sex.data) == "AGE")] <- "Age"
  names(ixi.age.sex.data)[which(names(ixi.age.sex.data) == "SEX_ID1m2f")] <- "Gender"

  ixi.age.sex.data$Gender[ixi.age.sex.data$Gender==1] <- "Male"
  ixi.age.sex.data$Gender[ixi.age.sex.data$Gender==2] <- "Female"

  ixi.mri.data$Id <- as.numeric(substr(rownames(ixi.mri.data), 4, 6))

  output <- merge(x=ixi.mri.data, y=ixi.age.sex.data, by.x="Id", by.y="IXI_ID", all.x=TRUE)

  output<- output[,!(colnames(output) %in% c("Row.names") )]

  missing.age.indices = which( is.na(output[,"Age"]) )
  missing.sex.indices = which( is.na(output[, "Gender"]) )

  missing.info.indices = union(missing.age.indices, missing.sex.indices)

  missing.info.id = output[missing.info.indices,]$Id
  missing.age.id = output[missing.age.indices,]$Id
  missing.sex.id = output[missing.sex.indices,]$Id

  if(verbose && length(missing.info.indices) > 0){
    warning(paste("Missing age data for subject", missing.age.id, "\n"))
    warning(paste("Missing gender data for subject", missing.sex.id, "\n"))
  }

  output <- output[-missing.info.indices,]

  return(output)
}

#' CAD Dementia Print File Locations
#'
#' Prints the location of two files required to merge the Diagnosis, Age, and Gender for CAD Dementia subjects with the post-processed 'Freesurfer' data
#'
#' @examples
#' caddementia.setfiles("train.txt", "test.txt")
#' caddementia.printfilelocations()
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
caddementia.printfilelocations <- function() {
  print(paste("train is located:", getOption("rsurfer-CADD.Train")))
  print(paste("test is located:", getOption("rsurfer-CADD.Test")))
}

#' CAD Dementia Merge With 'Freesurfer' Output
#'
#' Merges the Diagnosis, Age, and Gender for CAD Dementia subjects with the post-processed 'Freesurfer' data, note that the rownames of the input data frame must be the Subject IDs i.e. train_vumc_011 of the CAD Dementia data
#'
#' Test data Diagnoses are returned as NAs
#' @param df The data frame of data imported using fsimport()
#' @return The input data frame merged with Age, Gender and Diagnosis
#' @examples
#' \dontrun{
#' setfshome("/Applications/freesurfer")
#' df <- fsimport.serialise("~/CADDementiaSubjects/", "~/CADSubjects.rds", verbose = T)
#' caddementia.setfiles("train.txt", "test.txt")
#' df <- caddementia.mergewithfreesurferoutput(df)
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
caddementia.mergewithfreesurferoutput <- function(df) {

  .format_gender <- function(num) {
    if (num == 0) {
      return("Female")
    } else {
      return("Male")
    }
    return(NA)
  }

  .format_diagnosis <- function(num) {
    if (num == 0) {
      return("HC")
    } else if (num == 1) {
      return("MCI")
    } else if (num == 2) {
      return("AD")
    }
    return(NA)
  }

  cad.train <- utils::read.table(file=getOption("rsurfer-CADD.Train"), header=T)
  cad.test <- utils::read.table(file=getOption("rsurfer-CADD.Test"), header=T)
  cad.test$output <- NA

  cad.all <- rbind(cad.train, cad.test)

  df$Age <- NA
  df$Gender <- NA
  df$Diagnosis <- NA

  for (row in 1:nrow(df)) {
    subjectName <- rownames(df)[row]

    cadRow <- cad.all[which(cad.all$ID == subjectName),]
    df[row,]$Age <- cadRow$Age
    df[row,]$Gender <- .format_gender(cadRow$Sex)
    df[row,]$Diagnosis <- .format_diagnosis(cadRow$output)
  }

  return(df)
}

#' ADNI Print File Locations
#'
#' Prints the location of two files required to merge the (baseline) Diagnosis, Age, Gender and MMSE for ADNI subjects with the post-processed 'Freesurfer' data
#'
#' @examples
#' adni.setfiles("DXSUM_PDXCONV_ADNIALL.csv", "ADNIMERGE.csv")
#' adni.printfilelocations()
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
adni.printfilelocations <- function() {
  print(paste("DXSUM is located:", getOption("rsurfer-DXSUM")))
  print(paste("ADNIMERGE is located:", getOption("rsurfer-ADNIMERGE")))
}



#' ADNI Merge With 'Freesurfer' Output
#'
#' Merges the (baseline) Diagnosis, Age, Gender and MMSE for ADNI subjects with the post-processed 'Freesurfer' data, note that the rownames of the input data frame must be the Subject IDs i.e. 141_S_4232 of the ADNI database
#'
#' @param df The data frame of data imported using fsimport()
#' @return The input data frame merged with Age, Gender, Diagnosis and MMSE
#' @examples
#' \dontrun{
#' setfshome("/Applications/freesurfer")
#' df <- fsimport.serialise("~/CADDementiaSubjects/", "~/CADSubjects.rds", verbose = T)
#' adni.setfiles("DXSUM_PDXCONV_ADNIALL.csv", "ADNIMERGE.csv")
#' df <- adni.mergewithfreesurferoutput(df)
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
adni.mergewithfreesurferoutput <- function(df) {

  if (is.null(getOption("rsurfer-DXSUM")) || is.null(getOption("rsurfer-ADNIMERGE"))) {
    stop("Please use adni.setfiles() to set the location of the ADNI files")
  }

  dxsum <- utils::read.csv(getOption("rsurfer-DXSUM"))
  adnimerge <- utils::read.csv(getOption("rsurfer-ADNIMERGE"))

  #baseline scans only
  adnimerge <- adnimerge[which(adnimerge$VISCODE == "bl"),]

  hcvals <- c(1,7,9)
  mcivals <- c(2,4,8)
  advals <- c(3,5,6)

  dxsum$DXBASELINE <- NA
  for (row in 1:nrow(dxsum)) {
    if (dxsum$VISCODE[row] == "bl" || dxsum$VISCODE2[row] == "bl") {
      if (dxsum$Phase[row] == "ADNI1") {
        dxsum$DXBASELINE[row] <- dxsum$DXCURREN[row]
      } else if (dxsum$Phase[row] == "ADNIGO" || dxsum$Phase[row] == "ADNI2") {
        if (dxsum$DXCHANGE[row] %in% hcvals) {
          dxsum$DXBASELINE[row] <- 1
        } else if (dxsum$DXCHANGE[row] %in% mcivals) {
          dxsum$DXBASELINE[row] <- 2
        } else if (dxsum$DXCHANGE[row] %in% advals) {
          dxsum$DXBASELINE[row] <- 3
        } else {
          dxsum$DXBASELINE[row] <- NA
        }
      }
    }
  }
  dxsum$DXBASELINE <- as.factor(dxsum$DXBASELINE)
  dxsum <- dxsum[-which(is.na(dxsum$DXBASELINE)),]

  rownames(dxsum) <- dxsum$RID
  rownames(adnimerge) <- adnimerge$RID

  #left join dxsum and adnimerge
  adnimerge <- merge(dxsum, adnimerge, by = "RID", all = T)

  #get diagnosis, gender, MMSE and age from adnimerge
  rownames(adnimerge) <- adnimerge$PTID
  adnimerge <- adnimerge[,which(names(adnimerge) %in% c("PTID","AGE","PTGENDER", "MMSE", "DXBASELINE"))]
  adnimerge[-which(is.na(adnimerge$DXBASELINE)),]
  adnimerge$DXBASELINE <- as.numeric(adnimerge$DXBASELINE)

  names(adnimerge) <- c("Diagnosis", "PTID","Age","Gender","MMSE")

  #join adnimerge with freesurfer data
  #adnimerge <- adnimerge[which(rownames(adnimerge) %in% rownames(df)),]
  df$PTID <- rownames(df)
  #left join dxsum and adnimerge
  df <- merge(df,adnimerge, by='PTID', all.x = T)
  rownames(df) <- df$PTID
  df$PTID <- NULL

  df$Diagnosis <- as.factor(df$Diagnosis)
  levels(df$Diagnosis) <- c("HC","MCI","AD")

  return(df)
}

#' 'Freesurfer' Import (List Fields)
#'
#' This function assumes all the subdirectories of subjectDir are subjects who have been processed in 'Freesurfer'. In then runs various 'Freesurfer' scripts to extract the specified fields into an R data frame.
#'
#' @details
#'
#' Lists the field groups which can be imported from the subject are:
#' The specified fields can be:
#'
#' lh.cortical.thickness = left hemisphere cortical thicknesses
#' rh.cortical.thickness = right hemisphere cortical thicknesses
#' lh.cortical.volume = left hemisphere cortical volumes
#' rh.cortical.volume = right hemisphere cortical volumes
#' lh.cortical.thickness.std = left hemisphere cortical thickness standard deviations
#' rh.cortical.thickness.std = right hemisphere cortical thickness standard deviations
#' lh.cortical.area = left hemisphere cortical surface areas
#' rh.cortical.area = right hemisphere cortical surface areas
#' lh.subcortical = left hemisphere subcortical volumes
#' rh.subcortical = right hemisphere subcortical volumes
#' lh.hippocampal = left hemisphere hippocampal volumes
#' rh.hippocampal = right hemisphere hippocampal volumes
#'
#' @examples
#' fsimport.listfields()
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
fsimport.listfields <- function() {
  return(c("lh.cortical.thickness", "rh.cortical.thickness","lh.cortical.volume", "rh.cortical.volume", "lh.cortical.thickness.std",
           "rh.cortical.thickness.std","lh.cortical.area", "rh.cortical.area", "lh.subcortical", "rh.subcortical", "lh.hippocampal",
           "rh.hippocampal"))
}

#' 'Freesurfer' Import (Serialise)
#'
#' This function assumes all the subdirectories of subjectDir are subjects who have been processed in 'Freesurfer'. In then runs various 'Freesurfer' scripts to extract the specified fields into an R data frame.
#'
#' @param subjectDir The directory containing the subject subdirectories
#' @param fields The field groups to use, see fsimport() for more details
#' @param verbose Whether to log the 'Freesurfer' scripts to the R console
#' @return The subject data processed from 'Freesurfer'
#' @details
#'
#' The field groups which can be imported from the subject are:
#' The specified fields can be:
#'
#' lh.cortical.thickness = left hemisphere cortical thicknesses
#' rh.cortical.thickness = right hemisphere cortical thicknesses
#' lh.cortical.volume = left hemisphere cortical volumes
#' rh.cortical.volume = right hemisphere cortical volumes
#' lh.cortical.thickness.std = left hemisphere cortical thickness standard deviations
#' rh.cortical.thickness.std = right hemisphere cortical thickness standard deviations
#' lh.cortical.area = left hemisphere cortical surface areas
#' rh.cortical.area = right hemisphere cortical surface areas
#' lh.subcortical = left hemisphere subcortical volumes
#' rh.subcortical = right hemisphere subcortical volumes
#' lh.hippocampal = left hemisphere hippocampal volumes
#' rh.hippocampal = right hemisphere hippocampal volumes
#'
#' By default all of the above fields are included. For the hippocampal volumes, the subjects must have been processed with the "hippo-subfields" when 'Freesurfer' was invoked.
#' @examples
#' \dontrun{
#' setfshome("/Applications/freesurfer")
#' fsimport("/Users/alex/Desktop/Subjects")
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
fsimport <- function(subjectDir, fields = fsimport.listfields(), verbose = F) {

  if (is.null(getfshome()))
    stop('getfshome() not set, please use the function setfshome')

  if (grepl(" ", subjectDir))
    stop("Please use a directory/file for subjectDir without any spaces")

  checkHippo <- F
  if ("lh.hippocampal" %in% fields || "rh.hippocampal" %in% fields)
    checkHippo <- T

  if (fsdirectorycheck(subjectDir, checkHippo) > 0)
    stop("Errors found in your subject subdirectories, please fix them")

  subjectDirs <- list.dirs(subjectDir,recursive=F)

  #handle subcortical fields
  if ("lh.subcortical" %in% fields && "rh.subcortical" %in% fields) {
    fields <- fields[-c(which(fields == "lh.subcortical"),which(fields == "rh.subcortical"))]
    fields <- c(fields, "lh+rh.subcortical")
  }

  #handle hippocampal fields
  if ("lh.hippocampal" %in% fields && "rh.hippocampal" %in% fields) {
    fields <- fields[-c(which(fields == "lh.hippocampal"),which(fields == "rh.hippocampal"))]
    fields <- c(fields, "lh+rh.hippocampal")
  }

  #columns required
  colsReq <- 0

  fs_df <- NULL

  for (field in fields) {

    allSubs <- paste(basename(subjectDirs), collapse=" -s ")

    if (grepl("cortical",field) > 0 && grepl("subcortical",field) == 0) {
      if (field == "lh.cortical.thickness") {
        outputFile <- paste(subjectDir, "/lh.cortical.thickness.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi lh --meas thickness --tablefile ", outputFile, sep = "")
      } else if (field == "rh.cortical.thickness") {
        outputFile <- paste(subjectDir, "/rh.cortical.thickness.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi rh --meas thickness --tablefile ", outputFile, sep = "")
      } else if (field == "lh.cortical.area") {
        outputFile <- paste(subjectDir, "/lh.cortical.area.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi lh --meas area --tablefile ", outputFile, sep = "")
      } else if (field == "rh.cortical.area") {
        outputFile <- paste(subjectDir, "/rh.cortical.area.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi rh --meas area --tablefile ", outputFile, sep = "")
      } else if (field == "lh.cortical.thickness.std") {
        outputFile <- paste(subjectDir, "/lh.cortical.thickness.std.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi lh --meas thicknessstd --tablefile ", outputFile, sep = "")
      } else if (field == "rh.cortical.thickness.std") {
        outputFile <- paste(subjectDir, "/rh.cortical.thickness.std.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi rh --meas thicknessstd --tablefile ", outputFile, sep = "")
      } else if (field == "lh.cortical.volume") {
        outputFile <- paste(subjectDir, "/lh.cortical.volume.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi lh --meas volume --tablefile ", outputFile, sep = "")
      } else if (field == "rh.cortical.volume") {
        outputFile <- paste(subjectDir, "/rh.cortical.volume.", .getdatestring(), sep="")
        cmd <- paste(getfshome(), "/bin/","aparcstats2table -s ", allSubs, " --hemi rh --meas volume --tablefile ", outputFile, sep = "")
      }

      Sys.setenv("SUBJECTS_DIR"=subjectDir)

      if (verbose) {
        system(cmd)
      } else {
        system(cmd, ignore.stdout = T, ignore.stderr = F)
      }

      df <- utils::read.table(outputFile, header=T)
      colnames(df)[1] <- "SubjectID"
      rownames(df) <- df$SubjectID
      df$SubjectID <- NULL
      names(df) <- stringr::str_replace_all(names(df),"_",".")

      file.remove(outputFile)

      if (is.null(fs_df)) {
        fs_df <- df
      } else {
        fs_df <- cbind(fs_df, df)
      }
    } else if (grepl("hippocampal",field) > 0) {

      Sys.setenv("FREESURFER_HOME"=getfshome())
      Sys.setenv("SUBJECTS_DIR"=subjectDir)

      if (verbose) {
        system("source $FREESURFER_HOME/SetUpFreeSurfer.sh && cd $SUBJECTS_DIR && cp $FREESURFER_HOME/bin/kvlQuantifyHippocampalSubfieldSegmentations.sh $PWD && source kvlQuantifyHippocampalSubfieldSegmentations.sh")
      } else {
        system("source $FREESURFER_HOME/SetUpFreeSurfer.sh && cd $SUBJECTS_DIR && cp $FREESURFER_HOME/bin/kvlQuantifyHippocampalSubfieldSegmentations.sh $PWD && source kvlQuantifyHippocampalSubfieldSegmentations.sh", ignore.stdout=T, ignore.stderr=F)
      }

      file_to_read_left <- "nonPartialVolumeStatsLeft.txt"
      if (!file.exists(paste(subjectDir,file_to_read_left,sep="/")))
        file_to_read_left <- "volumeStats_left.txt"

      file_to_read_right <- "nonPartialVolumeStatsRight.txt"
      if (!file.exists(paste(subjectDir,file_to_read_right,sep="/")))
        file_to_read_right <- "volumeStats_right.txt"

      if (field == "lh.hippocampal") {
        df <- utils::read.table(paste(subjectDir,file_to_read_left,sep="/"),header=T)
        rownames(df) <- df[,1]
        df$volume_in_number_of_voxels <- NULL
        names(df)[1] <- "Left.Hippocampus.HS"
      } else if (field == "rh.hippocampal") {
        df <- utils::read.table(paste(subjectDir,file_to_read_right,sep="/"),header=T)
        rownames(df) <- df[,1]
        df$volume_in_number_of_voxels <- NULL
        names(df)[1] <- "Right.Hippocampus.HS"
      } else if (field == "lh+rh.hippocampal") {
        dflh <- utils::read.table(paste(subjectDir,file_to_read_left,sep="/"),header=T)
        rownames(dflh) <- dflh[,1]
        dflh$volume_in_number_of_voxels <- NULL
        names(dflh)[1] <- "Left.Hippocampus.HS"
        dfrh <- utils::read.table(paste(subjectDir,file_to_read_right,sep="/"),header=T)
        rownames(dfrh) <- dfrh[,1]
        dfrh$volume_in_number_of_voxels <- NULL
        names(dfrh)[1] <- "Right.Hippocampus.HS"
        df <- cbind(dflh,dfrh)
      }

      file.remove(paste(subjectDir,"nonPartialVolumeStatsLeft.txt",sep="/"))
      file.remove(paste(subjectDir,"nonPartialVolumeStatsRight.txt",sep="/"))
      file.remove(paste(subjectDir,"kvlQuantifyHippocampalSubfieldSegmentations.sh",sep="/"))

      names(df) <- stringr::str_replace_all(names(df),"_",".")

      if (is.null(fs_df)) {
        fs_df <- df
      } else {
        fs_df <- cbind(fs_df, df)
      }

    } else if (grepl("subcortical",field) > 0) {
      outputFile <- paste(subjectDir, "/subcortical", .getdatestring(), sep="")
      cmd <- paste(getfshome(),"/bin/asegstats2table --skip --inputs ",sep="")
      cmd <- paste(cmd, paste(subjectDirs, collapse = "/stats/aseg.stats "), "/stats/aseg.stats", sep="")
      cmd <- paste(cmd, " --meas volume --tablefile ", outputFile, sep="")

      Sys.setenv("SUBJECTS_DIR"=subjectDir)
      if (verbose) {
        system(cmd)
      } else {
        system(cmd, ignore.stdout = T, ignore.stderr = F)
      }

      df <- utils::read.table(outputFile, header=T)
      df$Measure.volume <- NULL
      rownames(df) <- basename(subjectDirs)

      file.remove(outputFile)

      #if we are working with ONLY the left hemisphere or ONLY the right hemisphere
      #we need to remove some data
      if ("lh.subcortical" %in% fields) {
        df <- df[,-c(19:32, 36, 39, 50, 53, 64)]
      } else if ("rh.subcortical" %in% fields) {
        df <- df[,-c(1:8,12,13,15:18,35,38,49,52,63)]
      }

      if (is.null(fs_df)) {
        fs_df <- df
      } else {
        fs_df <- cbind(fs_df, df)
      }
    }
  }

  return(fs_df)
}

#' 'Freesurfer' Subject Directory Check
#'
#' Crawls through the subdirectory of subjects looking for missing file that would affect the fsimport() process
#'
#' @param subjectDir The directory containing the subject subdirectories
#' @param checkForHippocampalSubfieldsError Whether to check to missing files which are to do with the "hippo-subfields" flag being called
#' @return The number of errors found
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
fsdirectorycheck <- function(subjectDir, checkForHippocampalSubfieldsError=T) {



  error_count <- 0
  dirs <- list.dirs(subjectDir, recursive = F)
  for (dir in dirs) {

    hippocampalSegmentationFiles <- c("posterior_Left-Hippocampus.mgz", "posterior_left_presubiculum.mgz", "posterior_left_CA1.mgz",
                                      "posterior_left_CA2_3.mgz", "posterior_left_fimbria.mgz", "posterior_left_subiculum.mgz", "posterior_left_CA4_DG.mgz",
                                      "posterior_left_hippocampal_fissure.mgz")

    subdirs <- list.dirs(dir, recursive = F)

    mri_found <- F
    hip_found <- F

    for (subdir in subdirs) {
      if (basename(subdir) == "mri") {
        mri_found <- T

        if (checkForHippocampalSubfieldsError) {
          mgzfiles <- list.files(subdir)
          for (mgzfile in mgzfiles) {
            mgzfile <- basename(mgzfile)
            if (mgzfile %in% hippocampalSegmentationFiles) {
              hippocampalSegmentationFiles <- hippocampalSegmentationFiles[-which(hippocampalSegmentationFiles == mgzfile)]
            }
          }

          if (length(hippocampalSegmentationFiles) > 0) {
            for (missingFile in hippocampalSegmentationFiles) {
              cat(paste("No \"", missingFile, "\" found in subject \"", basename(dir),"\"\n",sep=""))
              error_count <- error_count + 1
            }
          }
        }
      }
    }

    if (!mri_found) {
      cat(paste("No \"mri\" dir found in subject \"", basename(dir),"\"\n",sep=""))
      error_count <- error_count + 1
    }
  }

  return(error_count)
}

#' Test Importing
#'
#' Calls fsimport() with different parameters on the subjects in subjectDir to test it is working correctly. Note: requires the subjects to be processed with the "hippocampal-subfields" flag.
#'
#' @param subjectTestDir The directory containing the subject subdirectories
#' @examples
#' \dontrun{
#' test.importing("/Users/alex/Desktop/Subjects")
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
test.importing <- function(subjectTestDir) {
  stopifnot(ncol(fsimport(subjectTestDir, fields="lh.cortical.area")) == 35)
  stopifnot(ncol(fsimport(subjectTestDir, fields="rh.cortical.area")) == 35)
  stopifnot(ncol(fsimport(subjectTestDir, fields=c("lh.cortical.area", "rh.cortical.area"))) == 70)
  stopifnot(ncol(fsimport(subjectTestDir, fields="lh.cortical.thickness")) == 35)
  stopifnot(ncol(fsimport(subjectTestDir, fields="rh.cortical.thickness")) == 35)
  stopifnot(ncol(fsimport(subjectTestDir, fields=c("lh.cortical.thickness", "rh.cortical.thickness"))) == 70)
  stopifnot(ncol(fsimport(subjectTestDir, fields="lh.cortical.thickness.std")) == 34)
  stopifnot(ncol(fsimport(subjectTestDir, fields="rh.cortical.thickness.std")) == 34)
  stopifnot(ncol(fsimport(subjectTestDir, fields=c("lh.cortical.thickness.std", "rh.cortical.thickness.std"))) == 68)
  stopifnot(ncol(fsimport(subjectTestDir, fields="lh.cortical.volume")) == 34)
  stopifnot(ncol(fsimport(subjectTestDir, fields="rh.cortical.volume")) == 34)
  stopifnot(ncol(fsimport(subjectTestDir, fields=c("lh.cortical.volume", "rh.cortical.volume"))) == 68)
  stopifnot(ncol(fsimport(subjectTestDir, fields="lh.subcortical")) == 47)
  stopifnot(ncol(fsimport(subjectTestDir, fields="rh.subcortical")) == 47)
  stopifnot(ncol(fsimport(subjectTestDir, fields=c("lh.subcortical", "rh.subcortical"))) == 66)
  stopifnot(ncol(fsimport(subjectTestDir, fields="lh.hippocampal")) == 8)
  stopifnot(ncol(fsimport(subjectTestDir, fields="rh.hippocampal")) == 8)
  stopifnot(ncol(fsimport(subjectTestDir, fields=c("lh.hippocampal", "rh.hippocampal"))) == 16)
  stopifnot(ncol(fsimport(subjectTestDir)) == 358)
}

#' Test Normalisation
#'
#' Tests the normalisation functions are working correctly on randomly generated data.
#'
#' @examples
#' \dontrun{
#' test.normalisation()
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
test.normalisation <- function() {
  set.seed(1)
  num <- 1000
  all <- generaterandomsubjects(num)
  all <- addrandomgender(all)
  all <- addrandomdiagnosis(all)

  #Proportional - easy to verify
  stopifnot(normalise(all,"normalisation.proportional", "corticalvolumes")[1:num,"lh.cuneus.volume"] == all[1:num,"lh.cuneus.volume"]/all[1:num,"EstimatedTotalIntraCranialVol"])
  stopifnot(normalise(all,"normalisation.proportional", "corticalareas")[1:num,"lh.cuneus.area"] == all[1:num,"lh.cuneus.area"]/all[1:num,"EstimatedTotalIntraCranialVol"])
  stopifnot(normalise(all,"normalisation.proportional", "corticalareastsa")[1:num,"lh.cuneus.area"] == all[1:num,"lh.cuneus.area"]/all[1:num,"all$lh.WhiteSurfArea.area"])
  stopifnot(normalise(all,"normalisation.proportional", "corticalareastsa")[1:num,"rh.cuneus.area"] == all[1:num,"rh.cuneus.area"]/all[1:num,"all$rh.WhiteSurfArea.area"])
  stopifnot(normalise(all,"normalisation.proportional", "corticalthicknesses")[1:num,"lh.cuneus.thickness"] == all[1:num,"lh.cuneus.thickness"]/all[1:num,"EstimatedTotalIntraCranialVol"])
  stopifnot(normalise(all, "normalisation.proportional", "subcortical")[1:num, "Optic.Chiasm"] == all[1:num, "Optic.Chiasm"]/all[1:num,"EstimatedTotalIntraCranialVol"])
  stopifnot(normalise(all, "normalisation.proportional", "hippocampal")[1:num, "right.CA1"] == all[1:num, "right.CA1"]/all[1:num, "EstimatedTotalIntraCranialVol"])

  #For the others, just check they work
  nms <- c("normalisation.residual", "normalisation.residualgender",
           "normalisation.covariate",
           "normalisation.powerproportion")
  fsets <- c("corticalvolumes","corticalareas","corticalareastsa","corticalthicknesses","corticalthicknessesmct","subcortical","hippocampal")
  for (nm in nms) {
    for (fset in fsets) {
      n <- normalise(all,nm, fset)
    }
  }

  #For HC Only we must provide train and test data
  nms <- c("normalisation.residualhconly", "normalisation.residualgenderhconly", "normalisation.covariatehconly")
  train <- all[1:nrow(all)/2,]
  test <- all[(nrow(all)/2 + 1):nrow(all),]
  for (nm in nms) {
    for (fset in fsets) {
      n <- normalise(all,nm, fset, train,test)
    }
  }
}


#' Test Field Extraction
#'
#' Tests the field extraction functions are working correctly on imported data from subjectDir. Note: requires the subjects to be processed with the "hippocampal-subfields" flag.
#'
#' @param subjectTestDir The directory containing the subject subdirectories
#' @examples
#' \dontrun{
#' test.fieldextraction("/Users/alex/Desktop/Subjects")
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
test.fieldextraction <- function(subjectTestDir) {
  all <- fsimport(subjectTestDir)
  stopifnot(ncol(extract.hippocampalvolumes(all)) == 16)
  stopifnot(ncol(extract.subcorticalvolumes(all)) == 66)
  stopifnot(ncol(extract.cortical(all)) == 276)
  stopifnot(ncol(extract.corticalsurfaceareas(all)) == 70)
  stopifnot(ncol(extract.corticalthicknesses(all)) == 70)
  stopifnot(ncol(extract.corticalvolumes(all)) == 68)
  stopifnot(ncol(extract.volumes(all)) == 150)
}

#' Add Random Gender
#'
#' This function will add a random gender to every subject. It works by appending a Gender column to the input data frame 'all'. Half of the diagnosis will be Male and the other half will be Female.
#'
#' @param all The data frame to add a gender column to
#' @return The input data frame with a Gender column added
#' @examples
#' data <- generaterandomsubjects()
#' addrandomgender(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
addrandomgender <- function(all) {
  if ("Gender" %in% names(all))
    return(all)

  rnd <- stats::runif(nrow(all),0,1) <= 0.5
  rnd[which(rnd == T)] <- "Male"
  rnd[which(rnd == F)] <- "Female"

  rnd <- as.factor(rnd)

  all$Gender <- rnd

  return(all)
}

#' Add Random Diagnosis
#'
#' This function will add a random diagnosis to every subject. It works by appending a Diagnosis column to the input data frame 'all'. Half of the diagnosis will be 'HC' (healthy control) and the other half will be 'AD' (Alzheimer's Disease).
#'
#' @param all The data frame to add a Diagnosis column to
#' @return The input data frame with a Diagnosis column added
#' @examples
#' data <- generaterandomsubjects()
#' addrandomdiagnosis(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
addrandomdiagnosis <- function(all) {
  if ("Diagnosis" %in% names(all))
    return(all)

  rnd <- stats::runif(nrow(all),0,1) <= 0.5
  rnd[which(rnd == T)] <- "HC"
  rnd[which(rnd == F)] <- "AD"

  rnd <- as.factor(rnd)

  all$Diagnosis <- rnd

  return(all)
}

#' Generate Random Subjects
#'
#' Generate a data frame of random subjects whose fields match what would be imported from 'Freesurfer. This function is used for testing.
#'
#' @param num The number of subjects to generate
#' @return The generated subjects
#' @examples
#' generaterandomsubjects(num = 500)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
generaterandomsubjects <- function(num = 40) {
  #Used for testing
  df <- data.frame(matrix(ncol=358,nrow=num))
  names(df) <- c("lh.bankssts.thickness","lh.caudalanteriorcingulate.thickness","lh.caudalmiddlefrontal.thickness",
                 "lh.cuneus.thickness","lh.entorhinal.thickness","lh.fusiform.thickness","lh.inferiorparietal.thickness",
                 "lh.inferiortemporal.thickness","lh.isthmuscingulate.thickness","lh.lateraloccipital.thickness",
                 "lh.lateralorbitofrontal.thickness","lh.lingual.thickness","lh.medialorbitofrontal.thickness",
                 "lh.middletemporal.thickness","lh.parahippocampal.thickness","lh.paracentral.thickness",
                 "lh.parsopercularis.thickness","lh.parsorbitalis.thickness","lh.parstriangularis.thickness",
                 "lh.pericalcarine.thickness","lh.postcentral.thickness","lh.posteriorcingulate.thickness",
                 "lh.precentral.thickness","lh.precuneus.thickness","lh.rostralanteriorcingulate.thickness",
                 "lh.rostralmiddlefrontal.thickness","lh.superiorfrontal.thickness","lh.superiorparietal.thickness",
                 "lh.superiortemporal.thickness","lh.supramarginal.thickness","lh.frontalpole.thickness",
                 "lh.temporalpole.thickness","lh.transversetemporal.thickness","lh.insula.thickness","lh.MeanThickness.thickness",
                 "rh.bankssts.thickness","rh.caudalanteriorcingulate.thickness","rh.caudalmiddlefrontal.thickness",
                 "rh.cuneus.thickness","rh.entorhinal.thickness","rh.fusiform.thickness","rh.inferiorparietal.thickness",
                 "rh.inferiortemporal.thickness","rh.isthmuscingulate.thickness","rh.lateraloccipital.thickness",
                 "rh.lateralorbitofrontal.thickness","rh.lingual.thickness","rh.medialorbitofrontal.thickness",
                 "rh.middletemporal.thickness","rh.parahippocampal.thickness","rh.paracentral.thickness",
                 "rh.parsopercularis.thickness","rh.parsorbitalis.thickness","rh.parstriangularis.thickness",
                 "rh.pericalcarine.thickness","rh.postcentral.thickness","rh.posteriorcingulate.thickness","rh.precentral.thickness",
                 "rh.precuneus.thickness","rh.rostralanteriorcingulate.thickness","rh.rostralmiddlefrontal.thickness",
                 "rh.superiorfrontal.thickness","rh.superiorparietal.thickness","rh.superiortemporal.thickness",
                 "rh.supramarginal.thickness","rh.frontalpole.thickness","rh.temporalpole.thickness",
                 "rh.transversetemporal.thickness","rh.insula.thickness","rh.MeanThickness.thickness","lh.bankssts.volume",
                 "lh.caudalanteriorcingulate.volume","lh.caudalmiddlefrontal.volume","lh.cuneus.volume","lh.entorhinal.volume",
                 "lh.fusiform.volume","lh.inferiorparietal.volume","lh.inferiortemporal.volume","lh.isthmuscingulate.volume",
                 "lh.lateraloccipital.volume","lh.lateralorbitofrontal.volume","lh.lingual.volume","lh.medialorbitofrontal.volume",
                 "lh.middletemporal.volume","lh.parahippocampal.volume","lh.paracentral.volume","lh.parsopercularis.volume",
                 "lh.parsorbitalis.volume","lh.parstriangularis.volume","lh.pericalcarine.volume","lh.postcentral.volume",
                 "lh.posteriorcingulate.volume","lh.precentral.volume","lh.precuneus.volume","lh.rostralanteriorcingulate.volume",
                 "lh.rostralmiddlefrontal.volume","lh.superiorfrontal.volume","lh.superiorparietal.volume",
                 "lh.superiortemporal.volume","lh.supramarginal.volume","lh.frontalpole.volume","lh.temporalpole.volume",
                 "lh.transversetemporal.volume","lh.insula.volume","rh.bankssts.volume","rh.caudalanteriorcingulate.volume",
                 "rh.caudalmiddlefrontal.volume","rh.cuneus.volume","rh.entorhinal.volume","rh.fusiform.volume",
                 "rh.inferiorparietal.volume","rh.inferiortemporal.volume","rh.isthmuscingulate.volume","rh.lateraloccipital.volume",
                 "rh.lateralorbitofrontal.volume","rh.lingual.volume","rh.medialorbitofrontal.volume","rh.middletemporal.volume",
                 "rh.parahippocampal.volume","rh.paracentral.volume","rh.parsopercularis.volume","rh.parsorbitalis.volume",
                 "rh.parstriangularis.volume","rh.pericalcarine.volume","rh.postcentral.volume","rh.posteriorcingulate.volume",
                 "rh.precentral.volume","rh.precuneus.volume","rh.rostralanteriorcingulate.volume","rh.rostralmiddlefrontal.volume",
                 "rh.superiorfrontal.volume","rh.superiorparietal.volume","rh.superiortemporal.volume","rh.supramarginal.volume",
                 "rh.frontalpole.volume","rh.temporalpole.volume","rh.transversetemporal.volume","rh.insula.volume",
                 "lh.bankssts.thicknessstd","lh.caudalanteriorcingulate.thicknessstd","lh.caudalmiddlefrontal.thicknessstd",
                 "lh.cuneus.thicknessstd","lh.entorhinal.thicknessstd","lh.fusiform.thicknessstd","lh.inferiorparietal.thicknessstd",
                 "lh.inferiortemporal.thicknessstd","lh.isthmuscingulate.thicknessstd","lh.lateraloccipital.thicknessstd",
                 "lh.lateralorbitofrontal.thicknessstd","lh.lingual.thicknessstd","lh.medialorbitofrontal.thicknessstd",
                 "lh.middletemporal.thicknessstd","lh.parahippocampal.thicknessstd","lh.paracentral.thicknessstd",
                 "lh.parsopercularis.thicknessstd","lh.parsorbitalis.thicknessstd","lh.parstriangularis.thicknessstd",
                 "lh.pericalcarine.thicknessstd","lh.postcentral.thicknessstd","lh.posteriorcingulate.thicknessstd",
                 "lh.precentral.thicknessstd","lh.precuneus.thicknessstd","lh.rostralanteriorcingulate.thicknessstd",
                 "lh.rostralmiddlefrontal.thicknessstd","lh.superiorfrontal.thicknessstd","lh.superiorparietal.thicknessstd",
                 "lh.superiortemporal.thicknessstd","lh.supramarginal.thicknessstd","lh.frontalpole.thicknessstd",
                 "lh.temporalpole.thicknessstd","lh.transversetemporal.thicknessstd","lh.insula.thicknessstd",
                 "rh.bankssts.thicknessstd","rh.caudalanteriorcingulate.thicknessstd","rh.caudalmiddlefrontal.thicknessstd",
                 "rh.cuneus.thicknessstd","rh.entorhinal.thicknessstd","rh.fusiform.thicknessstd","rh.inferiorparietal.thicknessstd",
                 "rh.inferiortemporal.thicknessstd","rh.isthmuscingulate.thicknessstd","rh.lateraloccipital.thicknessstd",
                 "rh.lateralorbitofrontal.thicknessstd","rh.lingual.thicknessstd","rh.medialorbitofrontal.thicknessstd",
                 "rh.middletemporal.thicknessstd","rh.parahippocampal.thicknessstd","rh.paracentral.thicknessstd",
                 "rh.parsopercularis.thicknessstd","rh.parsorbitalis.thicknessstd","rh.parstriangularis.thicknessstd",
                 "rh.pericalcarine.thicknessstd","rh.postcentral.thicknessstd","rh.posteriorcingulate.thicknessstd",
                 "rh.precentral.thicknessstd","rh.precuneus.thicknessstd","rh.rostralanteriorcingulate.thicknessstd",
                 "rh.rostralmiddlefrontal.thicknessstd","rh.superiorfrontal.thicknessstd","rh.superiorparietal.thicknessstd",
                 "rh.superiortemporal.thicknessstd","rh.supramarginal.thicknessstd","rh.frontalpole.thicknessstd",
                 "rh.temporalpole.thicknessstd","rh.transversetemporal.thicknessstd","rh.insula.thicknessstd","lh.bankssts.area",
                 "lh.caudalanteriorcingulate.area","lh.caudalmiddlefrontal.area","lh.cuneus.area","lh.entorhinal.area",
                 "lh.fusiform.area","lh.inferiorparietal.area","lh.inferiortemporal.area","lh.isthmuscingulate.area",
                 "lh.lateraloccipital.area","lh.lateralorbitofrontal.area","lh.lingual.area","lh.medialorbitofrontal.area",
                 "lh.middletemporal.area","lh.parahippocampal.area","lh.paracentral.area","lh.parsopercularis.area",
                 "lh.parsorbitalis.area","lh.parstriangularis.area","lh.pericalcarine.area","lh.postcentral.area",
                 "lh.posteriorcingulate.area","lh.precentral.area","lh.precuneus.area","lh.rostralanteriorcingulate.area",
                 "lh.rostralmiddlefrontal.area","lh.superiorfrontal.area","lh.superiorparietal.area","lh.superiortemporal.area",
                 "lh.supramarginal.area","lh.frontalpole.area","lh.temporalpole.area","lh.transversetemporal.area","lh.insula.area",
                 "lh.WhiteSurfArea.area","rh.bankssts.area","rh.caudalanteriorcingulate.area","rh.caudalmiddlefrontal.area",
                 "rh.cuneus.area","rh.entorhinal.area","rh.fusiform.area","rh.inferiorparietal.area","rh.inferiortemporal.area",
                 "rh.isthmuscingulate.area","rh.lateraloccipital.area","rh.lateralorbitofrontal.area","rh.lingual.area",
                 "rh.medialorbitofrontal.area","rh.middletemporal.area","rh.parahippocampal.area","rh.paracentral.area",
                 "rh.parsopercularis.area","rh.parsorbitalis.area","rh.parstriangularis.area","rh.pericalcarine.area",
                 "rh.postcentral.area","rh.posteriorcingulate.area","rh.precentral.area","rh.precuneus.area",
                 "rh.rostralanteriorcingulate.area","rh.rostralmiddlefrontal.area","rh.superiorfrontal.area",
                 "rh.superiorparietal.area","rh.superiortemporal.area","rh.supramarginal.area","rh.frontalpole.area",
                 "rh.temporalpole.area","rh.transversetemporal.area","rh.insula.area","rh.WhiteSurfArea.area",
                 "Left.Lateral.Ventricle","Left.Inf.Lat.Vent","Left.Cerebellum.White.Matter","Left.Cerebellum.Cortex",
                 "Left.Thalamus.Proper","Left.Caudate","Left.Putamen","Left.Pallidum","X3rd.Ventricle","X4th.Ventricle",
                 "Brain.Stem","Left.Hippocampus","Left.Amygdala","CSF","Left.Accumbens.area","Left.VentralDC","Left.vessel",
                 "Left.choroid.plexus","Right.Lateral.Ventricle","Right.Inf.Lat.Vent","Right.Cerebellum.White.Matter",
                 "Right.Cerebellum.Cortex","Right.Thalamus.Proper","Right.Caudate","Right.Putamen","Right.Pallidum",
                 "Right.Hippocampus","Right.Amygdala","Right.Accumbens.area","Right.VentralDC","Right.vessel",
                 "Right.choroid.plexus","X5th.Ventricle","WM.hypointensities","Left.WM.hypointensities","Right.WM.hypointensities",
                 "non.WM.hypointensities","Left.non.WM.hypointensities","Right.non.WM.hypointensities","Optic.Chiasm",
                 "CC_Posterior","CC_Mid_Posterior","CC_Central","CC_Mid_Anterior","CC_Anterior","BrainSegVol","BrainSegVolNotVent",
                 "BrainSegVolNotVentSurf","lhCortexVol","rhCortexVol","CortexVol","lhCorticalWhiteMatterVol",
                 "rhCorticalWhiteMatterVol","CorticalWhiteMatterVol","SubCortGrayVol","TotalGrayVol","SupraTentorialVol",
                 "SupraTentorialVolNotVent","SupraTentorialVolNotVentVox","MaskVol","BrainSegVol.to.eTIV","MaskVol.to.eTIV",
                 "lhSurfaceHoles","rhSurfaceHoles","SurfaceHoles","EstimatedTotalIntraCranialVol","Left.Hippocampus.HS",
                 "left.presubiculum","left.CA1","left.CA2.3","left.fimbria","left.subiculum","left.CA4.DG",
                 "left.hippocampal.fissure","Right.Hippocampus.HS","right.presubiculum","right.CA1","right.CA2.3","right.fimbria",
                 "right.subiculum","right.CA4.DG","right.hippocampal.fissure")

  for (j in 1:ncol(df)) {
    if (names(df)[j] != "EstimatedTotalIntraCranialVol") {
      df[,j] <- stats::runif(num,1000,3000)
    } else {
      df[,j] <- stats::runif(num,100000,200000)
    }
  }


  return(df)
}

.addExtraFields <- function(NormalisationFunction, TargetField) {
  ExtraFields <- c()

  if (NormalisationFunction %in% c(".normalisation.covariatehconly",".normalisation.residualgender",".normalisation.residualgenderhconly",".normalisation.covariate",
                                   ".normalisation.powerproportiongender", ".normalisation.powerproportiongenderhconly")) {
    #print("Adding Gender")
    ExtraFields <- c(ExtraFields, "Gender")
  }
  if (NormalisationFunction %in% c(".normalisation.covariatehconly",".normalisation.residualhconly",".normalisation.residualgenderhconly", ".normalisation.powerproportionhconly",".prototype")) {
    #print("Adding Diagnosis")
    ExtraFields <- c(ExtraFields, "Diagnosis")
  }
  #if (NormalisationFunction %in% c(".prototype")) {
  #  ExtraFields <- c(ExtraFields, get.opposite.hemisphere.measurement(TargetField))
  #}

  return(ExtraFields)
}


# Proportional Normalisation Methods ----


.normalisation.proportional <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField

  normalisedFields <- data[,which(names(data) != NormalisationField)] / data[,NormalisationField]
  data <- cbind(normalisedFields,data[,NormalisationField])
  names(data)[ncol(data)] <- NormalisationField
  return(data)
}

.normalisation.proportional.multiplybymeanICV <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField

  normalisedFields <- data[,which(names(data) != NormalisationField)] / data[,NormalisationField]
  normalisedFields <- normalisedFields * mean(data[,NormalisationField])
  data <- cbind(normalisedFields,data[,NormalisationField])
  names(data)[ncol(data)] <- NormalisationField
  return(data)
}

# Residual Normalisation Methods ----

.normalisation.residual <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField

  for (i in 1:ncol(data)) {
    if (names(data)[i] != NormalisationField) {
      if (class(data[,i]) != "factor") {
        y <- data[,i]
        x <- data[,NormalisationField]
        fit <- stats::lm(y~x)
        #data[,i] <- predict(fit, data.frame(x=data[,NormalisationField]))
        data[,i] <- data[,i] - fit$coefficients[2] * (data[,NormalisationField] - mean(data[,NormalisationField]))
      }
    }
  }

  return(data)
}


.normalisation.residualhconly <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField

  for (i in 1:ncol(data)) {
    if (names(data)[i] != NormalisationField) {
      if (class(data[,i]) != "factor") {
        y <- data[data$Diagnosis == "HC",i]
        x <- data[data$Diagnosis == "HC",NormalisationField]
        fit <- stats::lm(y~x)
        #data[,i] <- predict(fit, data.frame(x=data[,NormalisationField]))
        data[,i] <- data[,i] - fit$coefficients[2] * (data[,NormalisationField] - mean(data[,NormalisationField]))
      }
    }
  }

  return(data)
}

.normalisation.covariate <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField
  for (i in 1:ncol(data)) {
    if (names(data)[i] != NormalisationField) {
      if (class(data[,i]) != "factor") {
        y <- data[,i]
        x <- data[,NormalisationField]
        x2 <- as.numeric(data$Gender)
        fit <- stats::lm(y~x+x2)
        #data[,i] <- predict(fit, data.frame(x=data[,NormalisationField], x2=as.numeric(Data$Gender)))
        data[,i] <- data[,i] - fit$coefficients[2] * (data[,NormalisationField] - mean(data[,NormalisationField])) -
          fit$coefficients[3] * (as.numeric(data$Gender) - mean(as.numeric(data$Gender)))
      }
    }
  }
  return(data)
}

.normalisation.covariatehconly <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField
  for (i in 1:ncol(data)) {
    if (names(data)[i] != NormalisationField) {
      if (class(data[,i]) != "factor") {
        y <- TrainData[TrainData$Diagnosis == "HC",i]
        x <- TrainData[TrainData$Diagnosis == "HC",NormalisationField]
        x2 <- as.numeric(TrainData[TrainData$Diagnosis == "HC","Gender"])
        fit <- stats::lm(y~x+x2)
        data[,i] <- data[,i] - fit$coefficients[2] * (data[,NormalisationField] - mean(data[,NormalisationField])) -
          fit$coefficients[3] * (as.numeric(data$Gender) - mean(as.numeric(data$Gender)))
      }
    }
  }
  return(data)
}

.icvn.cortical <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {
  if (BypassFieldChecking) {
    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  CVs <- extract.corticalvolumes(Data, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TrainData <- extract.corticalvolumes(TrainData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TestData <- extract.corticalvolumes(TestData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  CVs <- NormalisationFunction(CVs, TrainData = TrainData, TestData = TestData)
  DataWithoutCVs <- Data[,which(!names(Data) %in% names(CVs))]
  return(cbind(DataWithoutCVs, CVs))
}


.normalisation.residualgender <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {

  Data <- DataSubsetIncludingNormalisationField

  Data1 <- Data[Data$Gender == "Male",]
  Data2 <- Data[Data$Gender == "Female",]

  Data1 <- .normalisation.residual(Data1, NormalisationField)
  Data2 <- .normalisation.residual(Data2, NormalisationField)

  #Data1 and Data2 need to be in the same order as Data
  Data[order(rownames(Data)),] <- rbind(Data1,Data2)[order(rownames(rbind(Data1,Data2))),]

  return(Data)
}

.normalisation.residualgenderhconly <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {

  Data <- DataSubsetIncludingNormalisationField

  Data1 <- Data[Data$Gender == "Male",]
  Data2 <- Data[Data$Gender == "Female",]

  Data1 <- .normalisation.residualhconly(Data1, NormalisationField, TrainData)
  Data2 <- .normalisation.residualhconly(Data2, NormalisationField, TrainData)

  #Data1 and Data2 need to be in the same order as Data
  Data[order(rownames(Data)),] <- rbind(Data1,Data2)[order(rownames(rbind(Data1,Data2))),]

  return(Data)
}

.normalisation.powerproportion <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField

  for (i in 1:ncol(data)) {
    if (names(data)[i] != NormalisationField) {
      if (class(data[,i]) != "factor") {
        y <- data[,i]
        x <- data[,NormalisationField]

        fit <- tryCatch({stats::lm(log(y)~log(x))},
                        error = function(e){return(NULL)})
        if (!is.null(fit))
          data[,i] <- data[,i] / (data[,NormalisationField]^fit$coefficients[2])
      }
    }
  }

  return(data)
}

.normalisation.powerproportiongender <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {

  Data <- DataSubsetIncludingNormalisationField

  Data1 <- Data[Data$Gender == "Male",]
  Data2 <- Data[Data$Gender == "Female",]

  Data1 <- .normalisation.powerproportion(Data1, NormalisationField)
  Data2 <- .normalisation.powerproportion(Data2, NormalisationField)

  #Data1 and Data2 need to be in the same order as Data
  Data[order(rownames(Data)),] <- rbind(Data1,Data2)[order(rownames(rbind(Data1,Data2))),]

  return(Data)
}

.normalisation.powerproportiongenderhconly <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {

  Data <- DataSubsetIncludingNormalisationField

  Data1 <- Data[Data$Gender == "Male",]
  Data2 <- Data[Data$Gender == "Female",]

  Data1 <- .normalisation.powerproportionhconly(Data1, NormalisationField, TrainData, TestData)
  Data2 <- .normalisation.powerproportionhconly(Data2, NormalisationField, TrainData, TestData)

  #Data1 and Data2 need to be in the same order as Data
  Data[order(rownames(Data)),] <- rbind(Data1,Data2)[order(rownames(rbind(Data1,Data2))),]

  return(Data)
}

.normalisation.powerproportionhconly <- function(DataSubsetIncludingNormalisationField, NormalisationField = c("EstimatedTotalIntraCranialVol"), TrainData = NULL, TestData = NULL) {
  data <- DataSubsetIncludingNormalisationField

  for (i in 1:ncol(data)) {
    if (names(data)[i] != NormalisationField) {
      if (class(data[,i]) != "factor") {
        y <- data[data$Diagnosis == "HC",i]
        x <- data[data$Diagnosis == "HC",NormalisationField]

        fit <- tryCatch({stats::lm(log(y)~log(x))},
                        error = function(e){return(NULL)})
        if (!is.null(fit))
          data[,i] <- data[,i] / (data[,NormalisationField]^fit$coefficients[2])
      }
    }
  }

  return(data)
}


# Normalisatoin Field Type methods ----


.icvn.subcortical <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {
  if (BypassFieldChecking) {
    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  SVs <- extract.subcorticalvolumes(Data, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TrainData <- extract.subcorticalvolumes(TrainData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TestData <- extract.subcorticalvolumes(TestData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  SVs <- NormalisationFunction(SVs, TrainData = TrainData, TestData = TestData)
  DataWithoutSVs <- Data[,which(!names(Data) %in% names(SVs))]
  return(cbind(DataWithoutSVs, SVs))
}

.icvn.hippocampal <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {
  if (BypassFieldChecking) {
    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  HVs <- extract.hippocampalvolumes(Data, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TrainData <- extract.hippocampalvolumes(TrainData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TestData <- extract.hippocampalvolumes(TestData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  HVs <- NormalisationFunction(HVs, TrainData = TrainData, TestData = TestData)
  DataWithoutHVs <- Data[,which(!names(Data) %in% names(HVs))]
  return(cbind(DataWithoutHVs, HVs))
}

.icvn.corticalareas <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {

  if (BypassFieldChecking) {

    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  CAs <- extract.corticalsurfaceareas(Data, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TrainData <- extract.corticalsurfaceareas(TrainData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TestData <- extract.corticalsurfaceareas(TestData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  CAs <- NormalisationFunction(CAs, TrainData = TrainData, TestData = TestData)
  DataWithoutCAs <- Data[,which(!names(Data) %in% names(CAs))]
  return(cbind(DataWithoutCAs, CAs))
}

.icvn.corticalthicknesses <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {
  if (BypassFieldChecking) {
    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  CTs <- extract.corticalthicknesses(Data, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TrainData <- extract.corticalthicknesses(TrainData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TestData <- extract.corticalthicknesses(TestData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  CTs <- NormalisationFunction(CTs, TrainData = TrainData, TestData = TestData)
  DataWithoutCTs <- Data[,which(!names(Data) %in% names(CTs))]
  return(cbind(DataWithoutCTs, CTs))
}

.icvn.corticalthicknessstds <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {
  if (BypassFieldChecking) {
    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  CTs <- extract.corticalthicknessstddevs(Data, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TrainData <- extract.corticalthicknessstddevs(TrainData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  TestData <- extract.corticalthicknessstddevs(TestData, additionalFields = c("EstimatedTotalIntraCranialVol", ExtraFields))
  CTs <- NormalisationFunction(CTs, TrainData = TrainData, TestData = TestData)
  DataWithoutCTs <- Data[,which(!names(Data) %in% names(CTs))]
  return(cbind(DataWithoutCTs, CTs))
}

.tsan.corticalareas <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {
  if (BypassFieldChecking) {
    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  LCAs <- extract.corticalsurfaceareas(Data[,union(which(gdata::startsWith(names(Data), "lh")), which(names(Data) %in% ExtraFields))], additionalFields = union(c("lh.WhiteSurfArea.area"), ExtraFields))
  TrainLCAs <- extract.corticalsurfaceareas(TrainData[,union(which(gdata::startsWith(names(TrainData), "lh")), which(names(TrainData) %in% ExtraFields))], additionalFields = union(c("lh.WhiteSurfArea.area"), ExtraFields))
  TestLCAs <- extract.corticalsurfaceareas(TestData[,union(which(gdata::startsWith(names(TestData), "lh")), which(names(TestData) %in% ExtraFields))], additionalFields = union(c("lh.WhiteSurfArea.area"), ExtraFields))
  LCAs <- NormalisationFunction(LCAs, NormalisationField = "lh.WhiteSurfArea.area", TrainData = TrainLCAs, TestData = TestLCAs)
  RCAs <- extract.corticalsurfaceareas(Data[,union(which(gdata::startsWith(names(Data), "rh")), which(names(Data) %in% ExtraFields))], additionalFields = union(c("rh.WhiteSurfArea.area"), ExtraFields))
  TrainRCAs <- extract.corticalsurfaceareas(TrainData[,union(which(gdata::startsWith(names(TrainData), "rh")), which(names(TrainData) %in% ExtraFields))], additionalFields = union(c("rh.WhiteSurfArea.area"), ExtraFields))
  TestRCAs <- extract.corticalsurfaceareas(TestData[,union(which(gdata::startsWith(names(TestData), "rh")), which(names(TestData) %in% ExtraFields))], additionalFields = union(c("rh.WhiteSurfArea.area"), ExtraFields))
  RCAs <- NormalisationFunction(RCAs, NormalisationField = "rh.WhiteSurfArea.area", TrainData = TrainRCAs, TestData = TestRCAs)
  CAs <- cbind(LCAs,RCAs)
  DataWithoutCAs <- Data[,which(!names(Data) %in% names(CAs))]
  return(cbind(DataWithoutCAs, CAs))
}

.mctn.corticalthicknesses <- function(Data, NormalisationFunction, TrainData = NULL, TestData = NULL, BypassFieldChecking = F) {
  if (BypassFieldChecking) {
    NormalisationFunction <- eval(parse(text = NormalisationFunction))
    suppressWarnings(nData <- NormalisationFunction(Data, TrainData = TrainData, TestData = TestData))
    nData$Gender <- Data$Gender
    nData$Diagnosis <- Data$Diagnosis
    return(nData)
  }

  ExtraFields <- .addExtraFields(NormalisationFunction)
  NormalisationFunction <- eval(parse(text = NormalisationFunction))

  LCTs <- extract.corticalthicknesses(Data[,union(which(gdata::startsWith(names(Data), "lh")), which(names(Data) %in% ExtraFields))], additionalFields = union(c("lh.MeanThickness.thickness"), ExtraFields))
  TrainLCTs <- extract.corticalthicknesses(TrainData[,union(which(gdata::startsWith(names(TrainData), "lh")), which(names(TrainData) %in% ExtraFields))], additionalFields = union(c("lh.MeanThickness.thickness"), ExtraFields))
  TestLCTs <- extract.corticalthicknesses(TestData[,union(which(gdata::startsWith(names(TestData), "lh")), which(names(TestData) %in% ExtraFields))], additionalFields = union(c("lh.MeanThickness.thickness"), ExtraFields))
  LCTs <- NormalisationFunction(LCTs, NormalisationField = "lh.MeanThickness.thickness", TrainData = TrainLCTs, TestData = TestLCTs)
  RCTs <- extract.corticalthicknesses(Data[,union(which(gdata::startsWith(names(Data), "rh")), which(names(Data) %in% ExtraFields))], additionalFields = union(c("rh.MeanThickness.thickness"), ExtraFields))
  TrainRCTs <- extract.corticalthicknesses(TrainData[,union(which(gdata::startsWith(names(TrainData), "rh")), which(names(TrainData) %in% ExtraFields))], additionalFields = union(c("rh.MeanThickness.thickness"), ExtraFields))
  TestRCTs <- extract.corticalthicknesses(TestData[,union(which(gdata::startsWith(names(TestData), "rh")), which(names(TestData) %in% ExtraFields))], additionalFields = union(c("rh.MeanThickness.thickness"), ExtraFields))
  RCTs <- NormalisationFunction(RCTs, NormalisationField = "rh.MeanThickness.thickness", TrainData = TrainRCTs, TestData = TestRCTs)
  CTs <- cbind(LCTs,RCTs)
  DataWithoutCTs <- Data[,which(!names(Data) %in% names(CTs))]
  return(cbind(DataWithoutCTs, CTs))
}

#' List Normalisation Methods
#'
#' Lists all the available normalisation methods
#'
#' @return A list of the normalisation methods
#' @examples
#' normalise.listmethods()
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
normalise.listmethods <- function() {
  return (c(".normalisation.proportional", ".normalisation.residual", ".normalisation.residualhconly", ".normalisation.residualgender", ".normalisation.residualgenderhconly",
            ".normalisation.covariate", ".normalisation.covariatehconly", ".normalisation.powerproportion", ".normalisation.powerproportionhconly",
            ".normalisation.powerproportiongender", ".normalisation.powerproportiongenderhconly", ".normalisation.proportional.multiplybymeanICV"))
}

#' List Normalisation Field Groups
#'
#' Lists all the available field groups that the normalisation can operate on
#'
#' @return A list of the normalisation field groups
#' @examples
#' normalise.listfieldgroups()
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
normalise.listfieldgroups <- function() {
  return (c("corticalvolumes", "subcortical","hippocampal","corticalareas","corticalthicknesses", "corticalthicknessstds","corticalareastsa", "corticalthicknessesmct"))
}

#' Normalise
#'
#' Performs ICV (intracranial volume) normalisation on a data frame of imported subjects in data.
#'
#' @param data The subject data to normalise
#' @param normalisationFunction The normalisation function to use, see details for further information.
#' @param fieldType The field set to normalise, see details for further information.
#' @param trainData Data to train on, required for 'hconly' normalisation methods
#' @param testData Unseen data, required for 'hconly' normalisation methods
#' @return The normalised data
#' @examples
#' data <- generaterandomsubjects()
#' addrandomgender(data)
#' @details
#' Performs ICV (intracranial volume) normalisation on a data frame of imported subjects in data. The normalisationFunction specifies which normalisation method to use:
#'
#' normalisation.proportional = proportional ICV normalisation, the volumes of each subject are divided by their ICV
#'
#' normalisation.residual = residual ICV normalisation, a linear regression model is built for each volume using the ICV as a predictor
#'
#' normalisation.residualgender = residual ICV normalisation with a gender split, similar to residual ICV normalisation, except a separate linear regression model is built for Males and Females
#'
#' normalisation.residualhconly = residual ICV normalisation creating a regression model based on healthy control patients only
#'
#' The fieldType can be:
#'
#' corticalvolumes = Normalise cortical volumes by ICV
#'
#' subcortical = Normalise subcortical volumes by ICV
#'
#' hippocampal = Normalise hippocampal volumes by ICV
#'
#' corticalareas = Normalise cortical areas by ICV
#'
#' corticalthicknesses = Normalise cortical thicknesses by ICV
#'
#' corticalthicknessstds = Normalise cortical thicknesses standard deviations by ICV
#'
#' corticalareastsa = Normalise cortical areas by total surface area
#'
#' corticalthicknessesmct = Normalise cortical thicknesses by mean cortical thickness
#'
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
normalise <- function(data, normalisationFunction, fieldType = "all", trainData = NULL, testData = NULL) {
  if (!is.character(normalisationFunction)) {
    stop("NormalisationFunction must be a string")
    return(NULL)
  }

  if (substr(normalisationFunction,1,1) != ".") {
    normalisationFunction <- paste(".", normalisationFunction, sep="")
  }

  #Check normalisation method exists
  if (!normalisationFunction %in% c(normalise.listmethods())) {

    #Check for a prototype method
    if (is.na(stringr::str_extract(".prototype","prototype"))) {
      stop(paste("Normalisation Function doesn't exist: ", normalisationFunction, "\nAvailable methods are: ", paste(normalise.listmethods(), collapse=", "), sep =""))
      return(NULL)
    }
  }

  #Check field type exists
  if (fieldType != "all") {
    if (!fieldType %in% normalise.listfieldgroups()) {
      if (!fieldType %in% names(data)) {
        stop(paste("Field group doesn't exist so they cannot be normalised: ", fieldType, " \nAvailable field groups are: ", paste(normalise.listfieldgroups(),collapse=", "), sep =""))
        return(NULL)
      }
    }
  }

  #Check for Gender column
  if (normalisationFunction %in% c(".normalisation.residualgender", ".normalisation.residualgenderhconly", ".normalisation.covariate",
                                   ".normalisation.covariatehconly", ".normalisation.powerproportiongender")) {
    if (!"Gender" %in% names(data)) {
      stop(paste("Data requires 'Gender' column containing Male or Female for this normalisation method (", normalisationFunction, ")",sep=""))
      return(NULL)
    }

    if (class(data$Gender) != "factor") {
      stop("Data Gender column must be a factor")
      return(NULL)
    }
  }

  #Check for Diagnosis column
  if (normalisationFunction %in% c(".prototype",".normalisation.residualhconly", ".normalisation.residualgenderhconly", ".normalisation.covariatehconly", ".normalisation.powerproportionhconly")) {
    if (!"Diagnosis" %in% names(data)){
      stop(paste("Data requires 'Diagnosis' column containing at least two HC patients for this normalisation method (", normalisationFunction, ")", sep=""))
      return(NULL)
    }

    if (class(data$Diagnosis) != "factor") {
      stop("Data Diagnosis column must be a factor")
      return(NULL)
    }
  }

  #If HCOnly, TrainData must be provided
  if (normalisationFunction %in% c(".normalisation.residualhconly", ".normalisation.residualgenderhconly", ".normalisation.covariatehconly", ".normalisation.powerproportionhconly")) {
    if (is.null(trainData)) {
      stop("TrainData must be provided with a HCOnly normalisation function")
      return(NULL)
    }
  }

  FieldType <- tolower(fieldType)
  if (FieldType == "corticalvolumes")
    return(.icvn.cortical(data, normalisationFunction,trainData, testData))
  if (FieldType == "subcortical")
    return(.icvn.subcortical(data, normalisationFunction,trainData, testData))
  if (FieldType == "hippocampal")
    return(.icvn.hippocampal(data, normalisationFunction,trainData, testData))
  if (FieldType == "corticalareas")
    return (.icvn.corticalareas(data, normalisationFunction,trainData, testData))
  if (FieldType == "corticalthicknesses")
    return(.icvn.corticalthicknesses(data, normalisationFunction,trainData, testData))
  if (FieldType == "corticalthicknessstds")
    return(.icvn.corticalthicknessstds(data, normalisationFunction, trainData, testData))
  if (FieldType == "corticalareastsa")
    return(.tsan.corticalareas(data, normalisationFunction,trainData, testData))
  if (FieldType == "corticalthicknessesmct")
    return(.mctn.corticalthicknesses(data, normalisationFunction,trainData, testData))

  if (fieldType == "all") {
    #Recursively normalise everything
    data <- normalise(data, normalisationFunction, "corticalvolumes", trainData, testData)
    data <- normalise(data, normalisationFunction, "subcortical", trainData, testData)
    data <- normalise(data, normalisationFunction, "hippocampal", trainData, testData)
    data <- normalise(data, normalisationFunction, "corticalareas", trainData, testData)
    data <- normalise(data, normalisationFunction, "corticalthicknesses", trainData, testData)
    data <- normalise(data, normalisationFunction, "corticalthicknessstds", trainData, testData)
    return(data)
  }

  #FieldType = single column name
  if (fieldType %in% names(data)) {
    data <- data[,c(fieldType,"Gender","Diagnosis","EstimatedTotalIntraCranialVol")]
    if (is.corticalvolume(fieldType)) {
      return(.icvn.cortical(data, normalisationFunction,trainData, testData, BypassFieldChecking = T)[,1])
    } else if (is.corticalarea(fieldType)) {
      return(.icvn.corticalareas(data, normalisationFunction,trainData, testData, BypassFieldChecking = T)[,1])
    } else if (is.corticalthickness(fieldType)) {
      return(.icvn.corticalthicknesses(data, normalisationFunction,trainData, testData, BypassFieldChecking = T)[,1])
    } else if (is.subcorticalvolume(fieldType)) {
      return(.icvn.subcortical(data, normalisationFunction,trainData, testData, BypassFieldChecking = T)[,1])
    } else if (is.hippocampalvolume(fieldType)) {
      return(.icvn.hippocampal(data, normalisationFunction,trainData, testData, BypassFieldChecking = T)[,1])
    } else if (is.corticalthicknessstd(fieldType)) {
      return(.icvn.corticalthicknessstds(data, normalisationFunction,trainData, testData, BypassFieldChecking = T)[,1])
    } else {
      stop(paste("Cannot normalise",fieldType))
    }
  }

  stop("Failed")
  return(NULL)
}

#' Is Feature Cortical
#'
#' Checks whether a feature is a cortical measurement
#'
#' @param fieldName The field name of the feature to check is a cortical measurement
#' @return Whether the feature is cortical
#' @examples
#' is.cortical("lh.bankssts.thickness")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
is.cortical <- function(fieldName) {
  return(fieldName %in% c("lh.bankssts.thickness","lh.caudalanteriorcingulate.thickness","lh.caudalmiddlefrontal.thickness",
                          "lh.cuneus.thickness","lh.entorhinal.thickness","lh.fusiform.thickness","lh.inferiorparietal.thickness",
                          "lh.inferiortemporal.thickness","lh.isthmuscingulate.thickness","lh.lateraloccipital.thickness",
                          "lh.lateralorbitofrontal.thickness","lh.lingual.thickness","lh.medialorbitofrontal.thickness",
                          "lh.middletemporal.thickness","lh.parahippocampal.thickness","lh.paracentral.thickness",
                          "lh.parsopercularis.thickness","lh.parsorbitalis.thickness","lh.parstriangularis.thickness",
                          "lh.pericalcarine.thickness","lh.postcentral.thickness","lh.posteriorcingulate.thickness",
                          "lh.precentral.thickness","lh.precuneus.thickness","lh.rostralanteriorcingulate.thickness",
                          "lh.rostralmiddlefrontal.thickness","lh.superiorfrontal.thickness","lh.superiorparietal.thickness",
                          "lh.superiortemporal.thickness","lh.supramarginal.thickness","lh.frontalpole.thickness",
                          "lh.temporalpole.thickness","lh.transversetemporal.thickness","lh.insula.thickness","lh.MeanThickness.thickness",
                          "rh.bankssts.thickness","rh.caudalanteriorcingulate.thickness","rh.caudalmiddlefrontal.thickness",
                          "rh.cuneus.thickness","rh.entorhinal.thickness","rh.fusiform.thickness","rh.inferiorparietal.thickness",
                          "rh.inferiortemporal.thickness","rh.isthmuscingulate.thickness","rh.lateraloccipital.thickness",
                          "rh.lateralorbitofrontal.thickness","rh.lingual.thickness","rh.medialorbitofrontal.thickness",
                          "rh.middletemporal.thickness","rh.parahippocampal.thickness","rh.paracentral.thickness",
                          "rh.parsopercularis.thickness","rh.parsorbitalis.thickness","rh.parstriangularis.thickness",
                          "rh.pericalcarine.thickness","rh.postcentral.thickness","rh.posteriorcingulate.thickness",
                          "rh.precentral.thickness","rh.precuneus.thickness","rh.rostralanteriorcingulate.thickness",
                          "rh.rostralmiddlefrontal.thickness","rh.superiorfrontal.thickness","rh.superiorparietal.thickness",
                          "rh.superiortemporal.thickness","rh.supramarginal.thickness","rh.frontalpole.thickness",
                          "rh.temporalpole.thickness","rh.transversetemporal.thickness","rh.insula.thickness",
                          "rh.MeanThickness.thickness","lh.bankssts.thicknessstd","lh.caudalanteriorcingulate.thicknessstd",
                          "lh.caudalmiddlefrontal.thicknessstd","lh.cuneus.thicknessstd","lh.entorhinal.thicknessstd",
                          "lh.fusiform.thicknessstd","lh.inferiorparietal.thicknessstd","lh.inferiortemporal.thicknessstd",
                          "lh.isthmuscingulate.thicknessstd","lh.lateraloccipital.thicknessstd","lh.lateralorbitofrontal.thicknessstd",
                          "lh.lingual.thicknessstd","lh.medialorbitofrontal.thicknessstd","lh.middletemporal.thicknessstd",
                          "lh.parahippocampal.thicknessstd","lh.paracentral.thicknessstd","lh.parsopercularis.thicknessstd",
                          "lh.parsorbitalis.thicknessstd","lh.parstriangularis.thicknessstd","lh.pericalcarine.thicknessstd",
                          "lh.postcentral.thicknessstd","lh.posteriorcingulate.thicknessstd","lh.precentral.thicknessstd",
                          "lh.precuneus.thicknessstd","lh.rostralanteriorcingulate.thicknessstd","lh.rostralmiddlefrontal.thicknessstd",
                          "lh.superiorfrontal.thicknessstd","lh.superiorparietal.thicknessstd","lh.superiortemporal.thicknessstd",
                          "lh.supramarginal.thicknessstd","lh.frontalpole.thicknessstd","lh.temporalpole.thicknessstd",
                          "lh.transversetemporal.thicknessstd","lh.insula.thicknessstd","rh.bankssts.thicknessstd",
                          "rh.caudalanteriorcingulate.thicknessstd","rh.caudalmiddlefrontal.thicknessstd","rh.cuneus.thicknessstd",
                          "rh.entorhinal.thicknessstd","rh.fusiform.thicknessstd","rh.inferiorparietal.thicknessstd",
                          "rh.inferiortemporal.thicknessstd","rh.isthmuscingulate.thicknessstd","rh.lateraloccipital.thicknessstd",
                          "rh.lateralorbitofrontal.thicknessstd","rh.lingual.thicknessstd","rh.medialorbitofrontal.thicknessstd",
                          "rh.middletemporal.thicknessstd","rh.parahippocampal.thicknessstd","rh.paracentral.thicknessstd",
                          "rh.parsopercularis.thicknessstd","rh.parsorbitalis.thicknessstd","rh.parstriangularis.thicknessstd",
                          "rh.pericalcarine.thicknessstd","rh.postcentral.thicknessstd","rh.posteriorcingulate.thicknessstd",
                          "rh.precentral.thicknessstd","rh.precuneus.thicknessstd","rh.rostralanteriorcingulate.thicknessstd",
                          "rh.rostralmiddlefrontal.thicknessstd","rh.superiorfrontal.thicknessstd","rh.superiorparietal.thicknessstd",
                          "rh.superiortemporal.thicknessstd","rh.supramarginal.thicknessstd","rh.frontalpole.thicknessstd",
                          "rh.temporalpole.thicknessstd","rh.transversetemporal.thicknessstd","rh.insula.thicknessstd","lh.bankssts.area",
                          "lh.caudalanteriorcingulate.area","lh.caudalmiddlefrontal.area","lh.cuneus.area","lh.entorhinal.area",
                          "lh.fusiform.area","lh.inferiorparietal.area","lh.inferiortemporal.area","lh.isthmuscingulate.area",
                          "lh.lateraloccipital.area","lh.lateralorbitofrontal.area","lh.lingual.area","lh.medialorbitofrontal.area",
                          "lh.middletemporal.area","lh.parahippocampal.area","lh.paracentral.area","lh.parsopercularis.area",
                          "lh.parsorbitalis.area","lh.parstriangularis.area","lh.pericalcarine.area","lh.postcentral.area",
                          "lh.posteriorcingulate.area","lh.precentral.area","lh.precuneus.area","lh.rostralanteriorcingulate.area",
                          "lh.rostralmiddlefrontal.area","lh.superiorfrontal.area","lh.superiorparietal.area","lh.superiortemporal.area",
                          "lh.supramarginal.area","lh.frontalpole.area","lh.temporalpole.area","lh.transversetemporal.area",
                          "lh.insula.area","lh.WhiteSurfArea.area","rh.bankssts.area","rh.caudalanteriorcingulate.area",
                          "rh.caudalmiddlefrontal.area","rh.cuneus.area","rh.entorhinal.area","rh.fusiform.area","rh.inferiorparietal.area",
                          "rh.inferiortemporal.area","rh.isthmuscingulate.area","rh.lateraloccipital.area","rh.lateralorbitofrontal.area",
                          "rh.lingual.area","rh.medialorbitofrontal.area","rh.middletemporal.area","rh.parahippocampal.area",
                          "rh.paracentral.area","rh.parsopercularis.area","rh.parsorbitalis.area","rh.parstriangularis.area",
                          "rh.pericalcarine.area","rh.postcentral.area","rh.posteriorcingulate.area","rh.precentral.area",
                          "rh.precuneus.area","rh.rostralanteriorcingulate.area","rh.rostralmiddlefrontal.area","rh.superiorfrontal.area",
                          "rh.superiorparietal.area","rh.superiortemporal.area","rh.supramarginal.area","rh.frontalpole.area",
                          "rh.temporalpole.area","rh.transversetemporal.area","rh.insula.area","rh.WhiteSurfArea.area","lh.bankssts.volume",
                          "lh.caudalanteriorcingulate.volume","lh.caudalmiddlefrontal.volume","lh.cuneus.volume","lh.entorhinal.volume",
                          "lh.fusiform.volume","lh.inferiorparietal.volume","lh.inferiortemporal.volume","lh.isthmuscingulate.volume",
                          "lh.lateraloccipital.volume","lh.lateralorbitofrontal.volume","lh.lingual.volume","lh.medialorbitofrontal.volume",
                          "lh.middletemporal.volume","lh.parahippocampal.volume","lh.paracentral.volume","lh.parsopercularis.volume",
                          "lh.parsorbitalis.volume","lh.parstriangularis.volume","lh.pericalcarine.volume","lh.postcentral.volume",
                          "lh.posteriorcingulate.volume","lh.precentral.volume","lh.precuneus.volume","lh.rostralanteriorcingulate.volume",
                          "lh.rostralmiddlefrontal.volume","lh.superiorfrontal.volume","lh.superiorparietal.volume",
                          "lh.superiortemporal.volume","lh.supramarginal.volume","lh.frontalpole.volume","lh.temporalpole.volume",
                          "lh.transversetemporal.volume","lh.insula.volume","rh.bankssts.volume","rh.caudalanteriorcingulate.volume",
                          "rh.caudalmiddlefrontal.volume","rh.cuneus.volume","rh.entorhinal.volume","rh.fusiform.volume",
                          "rh.inferiorparietal.volume","rh.inferiortemporal.volume","rh.isthmuscingulate.volume",
                          "rh.lateraloccipital.volume","rh.lateralorbitofrontal.volume","rh.lingual.volume","rh.medialorbitofrontal.volume",
                          "rh.middletemporal.volume","rh.parahippocampal.volume","rh.paracentral.volume","rh.parsopercularis.volume",
                          "rh.parsorbitalis.volume","rh.parstriangularis.volume","rh.pericalcarine.volume","rh.postcentral.volume",
                          "rh.posteriorcingulate.volume","rh.precentral.volume","rh.precuneus.volume","rh.rostralanteriorcingulate.volume",
                          "rh.rostralmiddlefrontal.volume","rh.superiorfrontal.volume","rh.superiorparietal.volume",
                          "rh.superiortemporal.volume","rh.supramarginal.volume","rh.frontalpole.volume","rh.temporalpole.volume",
                          "rh.transversetemporal.volume","rh.insula.volume"))
}

#' Is Feature Cortical Volume
#'
#' Checks whether a feature is a cortical volume
#'
#' @param fieldName The field name of the feature to check is a cortical volume
#' @return Whether the feature is a cortical volume
#' @examples
#' is.corticalvolume("lh.bankssts.volume")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
is.corticalvolume <- function(fieldName) {
  return(is.cortical(fieldName) && grepl("\\w*volume\\b", fieldName))
}

#' Is Feature Cortical Area
#'
#' Checks whether a feature is a cortical area
#'
#' @param fieldName The field name of the feature to check is a cortical area
#' @return Whether the feature is a cortical area
#' @examples
#' is.corticalarea("lh.bankssts.area")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
is.corticalarea <- function(fieldName) {
  return(is.cortical(fieldName) && grepl("\\w*area\\b", fieldName))
}

#' Is Feature Cortical Thickness
#'
#' Checks whether a feature is a cortical thickness
#'
#' @param fieldName The field name of the feature to check is a cortical thickness
#' @return Whether the feature is a cortical thickness
#' @examples
#' is.corticalthickness("lh.bankssts.thickness")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
is.corticalthickness <- function(fieldName) {
  return(is.cortical(fieldName) && grepl("\\w*thickness\\b", fieldName))
}

#' Is Feature Cortical Thickness Standard Deviation
#'
#' Checks whether a feature is a cortical thickness standard deviation
#'
#' @param fieldName The field name of the feature to check is a cortical thickness standard deviation
#' @return Whether the feature is a cortical thickness standard deviation
#' @examples
#' is.corticalthicknessstd("lh.bankssts.thicknessstd")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
is.corticalthicknessstd <- function(fieldName) {
  return(is.cortical(fieldName) && grepl("\\w*thicknessstd\\b", fieldName))
}

#' Is Feature Subcortical Volume
#'
#' Checks whether a feature is a subcortical volume
#'
#' @param fieldName The field name of the feature to check is a subcortical volume
#' @return Whether the feature is a subcortical volume
#' @examples
#' is.subcorticalvolume("Brain.Stem")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
is.subcorticalvolume <- function(fieldName) {
  return(fieldName %in% c("Left.Lateral.Ventricle","Left.Inf.Lat.Vent","Left.Cerebellum.White.Matter","Left.Cerebellum.Cortex",
                          "Left.Thalamus.Proper","Left.Caudate","Left.Putamen","Left.Pallidum","X3rd.Ventricle","X4th.Ventricle",
                          "Brain.Stem","Left.Hippocampus","Left.Amygdala","CSF","Left.Accumbens.area","Left.VentralDC","Left.vessel",
                          "Left.choroid.plexus","Right.Lateral.Ventricle","Right.Inf.Lat.Vent","Right.Cerebellum.White.Matter",
                          "Right.Cerebellum.Cortex","Right.Thalamus.Proper","Right.Caudate","Right.Putamen","Right.Pallidum","Right.Hippocampus",
                          "Right.Amygdala","Right.Accumbens.area","Right.VentralDC","Right.vessel","Right.choroid.plexus","X5th.Ventricle",
                          "WM.hypointensities","Left.WM.hypointensities","Right.WM.hypointensities","non.WM.hypointensities",
                          "Left.non.WM.hypointensities","Right.non.WM.hypointensities","Optic.Chiasm","CC_Posterior","CC_Mid_Posterior",
                          "CC_Central","CC_Mid_Anterior","CC_Anterior","BrainSegVol","BrainSegVolNotVent","BrainSegVolNotVentSurf",
                          "lhCortexVol","rhCortexVol","CortexVol","lhCorticalWhiteMatterVol","rhCorticalWhiteMatterVol","CorticalWhiteMatterVol",
                          "SubCortGrayVol","TotalGrayVol","SupraTentorialVol","SupraTentorialVolNotVent","SupraTentorialVolNotVentVox",
                          "MaskVol","BrainSegVol.to.eTIV","MaskVol.to.eTIV","lhSurfaceHoles","rhSurfaceHoles","SurfaceHoles",
                          "EstimatedTotalIntraCranialVol"))
}

#' Is Feature Hippocampal Volume
#'
#' Checks whether a feature is a hippocampal volume
#'
#' @param fieldName The field name of the feature to check is a hippocampal volume
#' @return Whether the feature is a hippocampal volume
#' @examples
#' is.hippocampalvolume("right.fimbria")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
is.hippocampalvolume <- function(fieldName) {
  return(fieldName %in% c("Left.Hippocampus.HS", "left.presubiculum", "left.CA1", "left.CA2.3", "left.fimbria", "left.subiculum", "left.CA4.DG",
                          "left.hippocampal.fissure", "Right.Hippocampus.HS", "right.presubiculum", "right.CA1", "right.CA2.3",
                          "right.fimbria", "right.subiculum", "right.CA4.DG", "right.hippocampal.fissure"))
}

#' Extract Features By Group Name
#'
#' Extracts features in group specified by a string, groups that can be used are in normalise.listfieldgroups()
#'
#' @param data Subject data to extract from
#' @param fieldGroupName The group field name to extract
#' @param additionalFields Any additional fields to extract
#' @return The extracted fields
#' @examples
#' data <- generaterandomsubjects()
#' extract.byname(data, "subcortical")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
extract.byname <- function(data, fieldGroupName, additionalFields = c()) {
  if (fieldGroupName == "corticalvolumes")
    return(extract.corticalvolumes(data, additionalFields = additionalFields))
  if (fieldGroupName == "subcortical")
    return(extract.subcorticalvolumes(data, additionalFields = additionalFields))
  if (fieldGroupName == "hippocampal")
    return(extract.hippocampalvolumes(data, additionalFields = additionalFields))
  if (fieldGroupName == "corticalareas" || fieldGroupName == "corticalareastsa")
    return(extract.corticalsurfaceareas(data, additionalFields = additionalFields))
  if (fieldGroupName == "corticalthicknesses" || fieldGroupName == "corticalthicknessesmct")
    return(extract.corticalthicknesses(data, additionalFields = additionalFields))
  if (fieldGroupName == "corticalthicknessstds")
    return(extract.corticalthicknessstddevs(data, additionalFields = additionalFields))

  stop(paste("fieldGroupName doesn't exist (", fieldGroupName, ")"))
  return(NULL)
}

#' Get Field Group of Feature
#'
#' Given the name of a feature, this function gets what type of field group it belongs to, i.e. subcortical volume
#'
#' @param name Name of the feature
#' @param method The type of field groups that are returned
#' method = 1: {hippocampal, subcortical, corticalthicknessstds, corticalareas, corticalthicknesses, corticalvolumes}
#' method = 2: {volume, area, thickness, thicknessstd}
#' @return The field group the name belongs to
#' @examples
#' getfieldgroup("left.CA1", method = 1)
#' getfieldgroup("left.CA1", method = 2)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
getfieldgroup <- function(name, method = 1) {
  if (method == 1) {
    if (is.hippocampalvolume(name))
      return("hippocampal")
    if (is.subcorticalvolume(name))
      return("subcortical")
    if (is.corticalthicknessstd(name))
      return("corticalthicknessstds")
    if (is.corticalarea(name))
      return("corticalareas")
    if (is.corticalthickness(name))
      return("corticalthicknesses")
    if (is.corticalvolume(name))
      return("corticalvolumes")
  } else if (method == 2) {
    if (is.hippocampalvolume(name) || is.subcorticalvolume(name) || is.corticalvolume(name))
      return("volume")
    if (is.corticalthicknessstd(name))
      return("thicknessstd")
    if (is.corticalarea(name))
      return("area")
    if (is.corticalthickness(name))
      return("thickness")
  }
  return(NULL)
}

#' Is Feature Created by 'Freesurfer'
#'
#' Given the name of a feature, this function returns whether it was a feature generated the by 'Freesurfer' processing stream
#'
#' @param name Name of the feature
#' @return Whether the feature was created by 'Freesurfer'
#' @examples
#' isfsfeature("left.CA1")
#' isfsfeature("Age")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
isfsfeature <- function(name) {
  return(!is.null(getfieldgroup(name)))
}

#' Export Data For KNIME
#'
#' Exports the 'Freesurfer' imported data frame to a CSV readable by the software KNIME, it will assign two extra rows to the input dataframe:
#' field_group_1 which classifies the columns as S = {hippocampal, subcortical, corticalthicknessstds, corticalareas, corticalthicknesses, corticalvolumes}
#' field_group_2 which classifies the columns as S = {volume, area, thickness, thicknessstd}
#' And the data for the columns will be in folder/field_group_*/{S}.csv
#'
#' @param df The data frame to export
#' @param folder The folder to export to
#' @param additionalFields Vector of column names which should be added to each individual file created
#' @examples
#' \dontrun{
#' export.forKNIME(df, "/Users/alex/KNIMEData/", c("Age","MMSE"))
#' }
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
export.forKNIME <- function(df, folder, additionalFields = c("Gender","Age","Diagnosis")) {

  file_ext <- ".csv"

  if (!file.exists(folder))
    dir.create(folder)

  for (m in 1:2) {
    fg_dir <- paste(folder,"/field_group_", m, sep = "")
    if (file.exists(fg_dir))
      unlink(fg_dir, recursive=TRUE)

    dir.create(fg_dir)

    if (m == 1) {
      df.h <- extract.hippocampalvolumes(df, additionalFields)
      h_file <- paste(fg_dir, "/hippocampal",file_ext, sep="")
      utils::write.table(x = df.h, file = h_file)

      df.s <- extract.subcorticalvolumes(df, additionalFields)
      s_file <- paste(fg_dir, "/subcortical",file_ext, sep="")
      utils::write.table(x = df.s, file = s_file)

      df.cts <- extract.corticalthicknessstddevs(df, additionalFields)
      cts_file <- paste(fg_dir, "/corticalthicknessstds",file_ext, sep="")
      utils::write.table(x = df.cts, file = cts_file)

      df.ct <- extract.corticalthicknesses(df, additionalFields)
      ct_file <- paste(fg_dir, "/corticalthicknesses",file_ext, sep="")
      utils::write.table(x = df.ct, file = ct_file)

      df.ca <- extract.corticalsurfaceareas(df, additionalFields)
      ca_file <- paste(fg_dir, "/corticalareas",file_ext, sep="")
      utils::write.table(x = df.ca, file = ca_file)

      df.cv <- extract.corticalvolumes(df, additionalFields)
      cv_file <- paste(fg_dir, "/corticalvolumes",file_ext, sep="")
      utils::write.table(x = df.cv, file = cv_file)
    }

    if (m == 2) {

      df.h <- extract.volumes(df, additionalFields)
      h_file <- paste(fg_dir, "/hippocampal",file_ext, sep="")
      utils::write.table(x = df.h, file = h_file)


      df.cts <- extract.corticalthicknessstddevs(df, additionalFields)
      cts_file <- paste(fg_dir, "/corticalthicknessstds",file_ext, sep="")
      utils::write.table(x = df.cts, file = cts_file)


      df.ct <- extract.corticalthicknesses(df, additionalFields)
      ct_file <- paste(fg_dir, "/corticalthicknesses",file_ext, sep="")
      utils::write.table(x = df.ct, file = ct_file)

      df.ca <- extract.corticalsurfaceareas(df, additionalFields)
      ca_file <- paste(fg_dir, "/corticalareas",file_ext, sep="")
      utils::write.table(x = df.ca, file = ca_file)

    }
  }
}

#' Get Names of Volumes
#'
#' Gets the names of all the features that are volumes
#'
#' @param data Your subject data frame, if you have removed any columns from your data frame, the function will only return the volumes in this data frame. If this parameter is NULL then a new data frame will be randomly generated
#' @param excludeFields A vector of volume names to exclude, to exclude nothing set this parameter to NULL
#' @return A vector of the names of all the features which are volumes
#' @examples
#' getnamesofvolumes()
#' getnamesofvolumes(NULL, NULL)
#' getnamesofvolumes(NULL, excludeFields = c("left.CA1", "EstimatedTotalIntraCranialVol"))
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
getnamesofvolumes <- function(data = NULL, excludeFields = c("EstimatedTotalIntraCranialVol")) {
  if (is.null(data))
    data <- generaterandomsubjects(1)

  vol_names <- names(extract.volumes(data))

  if (is.null(excludeFields) || length(excludeFields) == 0)
    return(vol_names)

  return(vol_names[which(!vol_names %in% excludeFields)])
}

#' Get Names of Areas
#'
#' Gets the names of all the features that are areas
#'
#' @param data Your subject data frame, if you have removed any columns from your data frame, the function will only return the areas in this data frame. If this parameter is NULL then a new data frame will be randomly generated
#' @param excludeFields A vector of areas names to exclude, to exclude nothing set this parameter to NULL
#' @return A vector of the names of all the features which are areas
#' @examples
#' getnamesofareas()
#' getnamesofareas(NULL, NULL)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
getnamesofareas <- function(data = NULL, excludeFields = NULL) {
  if (is.null(data))
    data <- generaterandomsubjects(1)

  vol_names <- names(extract.corticalsurfaceareas(data))

  if (is.null(excludeFields) || length(excludeFields) == 0)
    return(vol_names)

  return(vol_names[which(!vol_names %in% excludeFields)])
}

#' Get Names of Thicknesses
#'
#' Gets the names of all the features that are thicknesses
#'
#' @param data Your subject data frame, if you have removed any columns from your data frame, the function will only return the thicknesses in this data frame. If this parameter is NULL then a new data frame will be randomly generated
#' @param excludeFields A vector of thickness names to exclude, to exclude nothing set this parameter to NULL
#' @return A vector of the names of all the features which are thicknesses
#' @examples
#' getnamesofthicknesses()
#' getnamesofthicknesses(NULL, NULL)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
getnamesofthicknesses <- function(data = NULL, excludeFields = NULL) {
  if (is.null(data))
    data <- generaterandomsubjects(1)

  vol_names <- names(extract.corticalthicknesses(data))

  if (is.null(excludeFields) || length(excludeFields) == 0)
    return(vol_names)

  return(vol_names[which(!vol_names %in% excludeFields)])
}

#' Get Opposite Hemisphere Measurement
#'
#' Given a left hemisphere measurement, will return the equivalent measure on the right hemisphere. If there is no equivalent feature, NULL will be returned
#'
#' @param name The name of the feature to return the corresponding measurement of the opposite hemisphere of
#' @param verbose If a corresponding feature doesn't exist, a message will be printed if this is true
#' @param verbose_warn If a corresponding feature doesn't exist, a message will be printed as a warning if this is true
#' @return Name of the feature on the other hemisphere (or NULL if the feature does not exist)
#' @examples
#' get.opposite.hemisphere.measurement("Right.vessel")
#' get.opposite.hemisphere.measurement("lhCortexVol")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
get.opposite.hemisphere.measurement <- function(name, verbose = F, verbose_warn = F) {
  if (gdata::startsWith(name,"lh"))
    return(stringr::str_replace(name,"lh","rh"))
  if (gdata::startsWith(name,"rh"))
    return(stringr::str_replace(name,"rh","lh"))
  if (gdata::startsWith(name,"left"))
    return(stringr::str_replace(name,"left","right"))
  if (gdata::startsWith(name,"right"))
    return(stringr::str_replace(name,"right","left"))
  if (gdata::startsWith(name,"Left"))
    return(stringr::str_replace(name,"Left","Right"))
  if (gdata::startsWith(name,"Right"))
    return(stringr::str_replace(name,"Right","Left"))

  if (verbose)
    print(paste(name,"doesn't have an opposite measurement"))

  if (verbose_warn)
    warning(paste(name,"doesn't have an opposite measurement"))

  return(NULL)
}

#' Get Hemisphere Side
#'
#' Given the name of a feature, will return a string as to whether it belongs to the left or the right hemisphere. If it belongs to neither, it is assumed that the feature is central
#'
#' @param name The name of the feature to return the hemisphere of
#' @return The side of the hemisphere the feature belongs to ("left" or "right"). If it belongs to neither of these, "central" is returned
#' @examples
#' get.hemisphere.side("Right.vessel")
#' get.hemisphere.side("lhCortexVol")
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
get.hemisphere.side <- function(name) {
  if (startsWith(name, "lh") || startsWith(name, "left") || startsWith(name, "Left")) {
    return("left")
  } else if (startsWith(name, "rh") || startsWith(name, "right") || startsWith(name, "Right")) {
    return("right")
  }
  return("central")
}
getHemisphereSide <- function(name) {
  .Deprecated("get.hemisphere.side", "rsurfer")
  return(get.hemisphere.side(name))
}

#' Eliminate Abnormal Rows
#'
#' Will remove rows from a data frame of data generated by 'Freesurfer' that are abnormal - such as they have values of NA in them
#'
#' @param data The data frame to remove abnormal rows from
#' @param verbose Whether to print a log of what was removed
#' @return The data frame with abnormal rows removed
#' @examples
#' data <- generaterandomsubjects(10)
#' eliminateabnormalities.rows(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
eliminateabnormalities.rows <- function(data, verbose=T){
  ab_rows = searchforabnormalities.rows(data, verbose=verbose)
  if(length(ab_rows > 0) ){
    data <- data[-ab_rows,]
  }
  return(data)
}

#' Eliminate Abnormal Columns
#'
#' Will remove columns from a data frame of data generated by 'Freesurfer' that are abnormal - columns where all values are zero
#'
#' @param data The data frame to remove abnormal columns from
#' @param verbose Whether to print a log of what was removed
#' @return The data frame with abnormal columns removed
#' @examples
#' data <- generaterandomsubjects()
#' eliminateabnormalities.cols(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
eliminateabnormalities.cols <- function(data, verbose=T){
  ab.cols <- searchforabnormalities.cols(data, verbose)
  if(length(ab.cols) > 0){
    data <- data[, -ab.cols]
  }
  return(data)
}

#' Eliminate Abnormal Rows and Columns
#'
#' Will remove rows from a data frame of data generated by 'Freesurfer' that are abnormal - such as they have values of NA in them; will also remove columns
#'
#' @param data The data frame to remove abnormalities from
#' @param verbose Whether to print a log of what was removed
#' @return The data frame with abnormalities removed
#' @examples
#' data <- generaterandomsubjects()
#' eliminateabnormalities.rows(data)
#' @author Alexander Luke Spedding, \email{alexspedding271@gmail.com}
#' @export
eliminateabnormalities <- function(data, verbose=F) {
  data <- eliminateabnormalities.rows(data, verbose)
  data <- eliminateabnormalities.cols(data, verbose)
  return(data)
}


#' @import stringr gdata
suppressMessages(library(stringr))
suppressMessages(library(gdata))



# suppressMessages(library(plyr))
# suppressMessages(library(grid))
# suppressMessages(library(data.table))
# suppressMessages(library(glmnet))
#    devtools::use_package("stringr")
#    devtools::use_package("gdata")
#devtools::use_package("plyr")
#devtools::use_package("grid")
#devtools::use_package("data.table")
#devtools::use_package("glmnet")
