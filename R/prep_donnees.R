#' Importation et préparation des données liées à l'évaluation de stock de flétan de l'Atlantique
#'
#' @param dir_sauvegarde_locale chemin vers la sauvegarde locale des données importées
#'
#' @returns
#' @export
#'
#' @examples
prep_donnees <- function(dir_sauvegarde_locale) {
  ## ziff
  ziff <- lireZiff(
    no_espece = 130,
    dir_sauvegarde_locale = dir_sauvegarde_locale
  )

  ## débarquements
  ## gestion des pêches
  ## ziff
  temp <- calculer_deb_ziff(subset(ziff, opano %in% c('4R', '4S', '4T')))
  ## opano

  ## relevés au chalut MPO

  ## relevé à la palangre

  ## relevés sentinelle

  ## échantillonneurs à quai

  ## observateurs en mer

  ## retours de marquage

  ## lectures d'âge
}
