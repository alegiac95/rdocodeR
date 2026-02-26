#' RDoC Term-to-Domain Reference
#'
#' Returns the internal RDoC term list used by [rdoc_decode()].
#'
#' @return A data frame with columns `Domain` and `Term`.
#' @export
rdoc_terms_reference <- function() {
  data.frame(
    Domain = c(
      "AR", "AR", "CS", "CS", "CS", "CS", "CS", "CS", "CS",
      "CS", "CS", "CS", "CS", "CS", "CS", "CS", "CS", "CS",
      "CS", "NV", "NV", "NV", "PV", "PV", "PV", "PV", "PV",
      "SP", "SP", "SP", "SP", "SS", "SS", "SS", "SS", "SS", "SS"
    ),
    Term = c(
      "Arousal", "Circadian Rythm", "Attention",
      "CC - Goal Selection (Goal)", "CC - Goal Selection (Selection)",
      "CC - Performance Monitoring", "CC - Response Selection",
      "CC - Inhibition/Suppression", "Declarative Memory",
      "Declarative Memory/WM", "Language", "Perception - Auditory",
      "Perception - Multimodal", "Perception - Olfactory", "Perception - Somatosensory",
      "Perception - Visual", "WM - Interference Control", "WM - Active Maintenance",
      "WM - Flexible Updating", "Acute Threat ('Fear')", "Loss", "Potential Threat ('Anxiety')",
      "RL - Reward Prediction Error", "RL - Probabilistic and Reinforcement Learning",
      "Reward Anticipation", "Reward Responsivness", "Reward Valuation",
      "Affiliation and Attachement", "Animacy Perception", "Action Perception",
      "Social Communication", "Agency and Ownership", "Motor Execution",
      "Motor Actions", "Motor Inhibition and Termination", "Motor Initiation",
      "Innate Motor Patterns"
    ),
    stringsAsFactors = FALSE
  )
}

#' Read Example RDoC Correlation Data
#'
#' Loads the bundled TSV example data with columns `Domain`, `Term`, `r`, and `p`.
#'
#' @return A data frame.
#' @export
rdoc_example_data <- function() {
  path <- system.file(
    "extdata",
    "methylphenidate_mean_RDoC_absolute-values.tsv",
    package = "rdocodeR"
  )
  if (identical(path, "")) {
    stop("Bundled example file is missing from this installation.", call. = FALSE)
  }
  utils::read.delim(path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
}

#' Path to Bundled RDoC Term Maps
#'
#' Returns the path of the bundled `rdoc_terms.fsavg6.lh.rh.rds` file.
#'
#' @return Absolute file path to the bundled term map RDS file.
#' @export
rdoc_terms_file <- function() {
  path <- system.file("extdata", "rdoc_terms.fsavg6.lh.rh.rds", package = "rdocodeR")
  if (identical(path, "")) {
    stop("Bundled RDoC term map file is missing from this installation.", call. = FALSE)
  }
  path
}
