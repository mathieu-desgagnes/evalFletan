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
#' @param dirInput chemin du répertoire à utiliser pour lire les données telles que produites par la DAISS.
#' @param noEspece un code STRAP de l'espèce pour laquelle les informations sont recherchées (voir Details)
#'
#' @importFrom  data.table fread
#'
#' @return
#'
lireZiff <- function(noEspece=130,
                     annees=NULL,
                     dirInput=file.path('//ent.dfo-mpo.ca','dfo-mpo','GROUP','QUE','Reg_Shares','DFO','science','DAISS','BD_Peches','Ziff','Version_totale')
                     ){
  ##
  fichiers <- list.files(path=dirInput, pattern='^zif_version_totale_', ignore.case=TRUE)
  if(all(is.na(fichiers))) stop('Aucun fichier nommé "zif_version_totale" dans le dossier.')
  fichiers.annees <- gsub(".*totale_([0-9]{4}).*", "\\1", fichiers)
  ##
  ## sélectionner les années
  if(!is.null(annees)){
    if(!is.numeric(annees)) stop("L'argument 'année' doit être un vecteur numérique.")
    lesquels <- which(fichiers.annees %in% annees)
  }else{
    lesquels <- seq_along(fichiers)
  }
  fichiers <- fichiers[lesquels]
  fichiers.annees <- fichiers.annees[lesquels]
  fichiers.preliminaires <- grepl("totale_[0-9]{4}PR", fichiers)
  ##
  ## indiquer s'il y a des fichiers péliminaires
  if(any(fichiers.preliminaires)){
    annees_pr <- fichiers.annees[fichiers.preliminaires]
    warning(paste0(
      "ATTENTION : Les données pour ",
      paste(annees_pr, collapse = ", "),
      " sont PRÉLIMINAIRES (PR)"
    ))
  }


}


    ys <- sapply(fichiers, function(x){y <- gsub(".*totale_(.+).csv", "\\1", x)})
    ys <- cbind(start = as.numeric(substring(ys, 1, 4)),
                end   =  as.numeric(substring(ys, 5, 8)))
    id <- apply(ys, 1, function(z) any(sapply(year, function(x) x %in% z[1]:z[2])))
    files <- files[id]
  }

    files <- tibble(fichier = basename(files)) %>%
      mutate(an = as.numeric(gsub("\\D", "", fichier))) %>%
      filter(an %in% ans) %>%
      pull(fichier)
    files <- paste0(dirIN, files)
  }



}
