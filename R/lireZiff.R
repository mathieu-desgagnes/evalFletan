#' Lit les fichiers ZIFF
#'
#' `lireZiff()` lit les fichiers `Zonal Interchange File Format` (ziff) préparés par la DAISS et disponbles sur le réseau interne, qui regroupe par année les données ziff des 4 régions
#' de l'est canadien, pour toutes les espèces.
#' Certaines variables sont ajusté pour des besoins de standardisation avec d'autres jeux de données.
#'
#' Certains codes d'espèce fréquement utilisés: 100(morue), 120(sébaste sp.), 117(sébaste atlantique/du nord), 118(sébaste acadien/Rose),
#'     130(flétan atlantique), 144(turbot), 140(plie can), 142(plie grise), 141(limande), 143(plie rouge), 702(crevette)
#' Pour une liste détaillée des codes d'espèce, voir /DCQCIMLNA01A/BD_Peches/Ziff/Documentation/Codes_espèces.xlsx
#' Les catégories de longueur utilisent des borne inférieurs inclusives et des bornes supérieurs exclusives, avec une tolérence de 0.01cm.
#' Ainsi, "plus grand que 85cm" devient >=84.99, la tolérence permettant d'éviter des comportements imprévisibles (et parfois incompris...) de la fonction.
#'
#' Certains utilisateurs suggèrent d'installer les fichiers .txt localement pour accélérer le processus.
#'
#' @param no_espece un code STRAP de l'espèce pour laquelle les informations sont recherchées (voir Details)
#' @param annees un vecteur des années à considérer
#' @param dir_input chemin du répertoire à utiliser pour lire les données telles que produites par la DAISS.
#'
#' @importFrom  data.table fread
#'
#' @return
#'
lireZiff <- function(
  no_espece = 130,
  annees = NULL,
  dir_input = file.path(
    '//ent.dfo-mpo.ca',
    'dfo-mpo',
    'GROUP',
    'QUE',
    'Reg_Shares',
    'DFO',
    'science',
    'DAISS',
    'BD_Peches',
    'Ziff',
    'Version_totale'
  )
) {
  ##
  fichiers <- list.files(
    path = dir_input,
    pattern = '^zif_version_totale_'
  )
  if (all(is.na(fichiers))) {
    stop('Aucun fichier nommé "zif_version_totale" dans le dossier.')
  }
  fichiers.annees <- gsub(".*totale_([0-9]{4}).*", "\\1", fichiers)
  ##
  ## sélectionner les années
  if (!is.null(annees)) {
    if (!is.numeric(annees)) {
      stop("L'argument 'année' doit être un vecteur numérique.")
    }
    fichiers <- fichiers[fichiers.annees %in% annees]
    fichiers.annees <- fichiers.annees[fichiers.annees %in% annees]
  }
  fichiers.preliminaires <- grepl("totale_[0-9]{4}PR", fichiers)
  ##
  ## indiquer s'il y a des fichiers péliminaires
  if (any(fichiers.preliminaires)) {
    annees_pr <- fichiers.annees[fichiers.preliminaires]
    message(paste0(
      ## warning(paste0(
      "ATTENTION : Les données pour ",
      paste(annees_pr, collapse = ", "),
      " sont PRÉLIMINAIRES (PR)"
    ))
  }
  fichiers
}
