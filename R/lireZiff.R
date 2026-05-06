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
#' Cette version de la fonction offre la possibilité d'enregistrer localement le resultat de la lecture, et par la suite ne lire que les fichier mis à jour.
#'
#' Note: À faire, valider si les opano sont correct pour toutes les années. Valider les "4Ru" si on met dans "4R"
#'
#' @param no_espece un codes STRAP de l'espèce capturée par la pêche. NULL importe toutes les espèces capturées.
#' @param no_espece_visee un codes STRAP de l'espèce visée par la pêche. NULL importe toutes les espèces visées.
#' @param annees un vecteur des années à considérer.
#' @param dir_input chemin du répertoire à utiliser pour lire les données telles que produites par la DAISS.
#' @param dir_sauvegarde_locale chemin du répertoire de sauvegarde local, si désiré. Permet de réduire le temps de lecture en ne lisant que les fichiers nécessitant une mise à jour.
#'
#' @importFrom  data.table fread rbindlist
#' @import  lubridate
#'
#' @return
#'
lireZiff <- function(
  no_espece = 130,
  no_espece_visee = NULL,
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
  ),
  dir_sauvegarde_locale = NULL
) {
  ##
  if (!dir.exists(paths = dir_sauvegarde_locale)) {
    dir_sauvegarde_locale <- NULL
    message(
      "Le dossier de sauvegarde locale n'existe pas. Les fichiers lus ne seront pas conservés."
    )
  }
  ##
  ## sélectionner les fichiers
  fichiers <- list.files(
    path = dir_input,
    pattern = '^zif_version_totale_'
  )
  if (all(is.na(fichiers))) {
    stop('Aucun fichier nommé "zif_version_totale" dans le dossier.')
  }
  fichiers.annees <- gsub(".*totale_([0-9]{4}).*", "\\1", fichiers)
  ##
  if (!is.null(annees)) {
    if (!is.numeric(annees)) {
      stop("L'argument 'année' doit être un vecteur numérique.")
    }
    if (!any(fichiers.annees %in% annees)) {
      stop("Aucun fichier ne correspond à l'argument 'année'.")
    }
    fichiers <- fichiers[fichiers.annees %in% annees]
    fichiers.annees <- fichiers.annees[fichiers.annees %in% annees]
  }
  fichiers.preliminaires <- grepl("totale_[0-9]{4}PR", fichiers)
  ##
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
  ##
  ##
  ## lire les fichiers un à un
  ## l'idée ici est de vérifier si un dossier local est utilisé pour conserver une version déjà lue des fichiers ziff, et
  ## ne relire que les fichiers qui nécessitent une mise à jour par rapport à leur version locale
  ziff.init <- list()
  for (i.an in seq_along(fichiers)) {
    nom_local <- paste0(
      'ziff',
      fichiers.annees[i.an],
      '_esp',
      noEspece,
      '.RData'
    )
    ##
    ## vérifier l'existence d'un fichier local
    besoin_update <- !file.exists(file.path(dir_sauvegarde_locale, nom_local))
    if (!besoin_update) {
      # si le fichier existe, comparer les dates pour savoir si mise à jour nécessaire
      temps.fReseau <- file.info(file.path(dir_input, fichiers[i]))$mtime
      temps.fLocal <- file.info(file.path(
        dir_sauvegarde_locale,
        nom_local
      ))$mtime
      besoin_update <- is.na(temp.fReseau) ||
        is.na(temp.fLocal) ||
        (temps.fReseau > temps.fLocal)
    }
    ##
    ## lire le ficher local si approprié, sinon lire le fichier réseau, polir celui et possiblement l'enregistrer localement
    if (!besoin_update) {
      message(paste0(fichiers[i.an], ": lecture depuis le cache local."))
      ziff.temp <- readRDS(file.path(
        dir_sauvegarde_locale,
        paste0(nom_local, '.rds')
      ))
    } else {
      message(paste0(fichiers[i.an], ": lecture réseau..."))
      ziff.temp <- data.table::fread(
        file = file.path(dir_input, fichiers[i.an]),
        sep = ';',
        header = TRUE,
        stringsAsFactors = FALSE,
        data.table = FALSE
      )
      ## sélectionner l'espèce
      condition1 <- if (is.null(no_espece)) {
        rep(TRUE, nrow(ziff.temp))
      } else {
        ziff.temp$cod_esp %in% no_espece
      }
      condition2 <- if (is.null(no_espece_visee)) {
        rep(TRUE, nrow(ziff.temp))
      } else {
        ziff.temp$prespvis %in% no_espece_visee
      }
      ziff.temp <- ziff.temp[condition1 & condition2, ]
      ##
      ## ajouter la source des données
      ziff.temp$source <- fichiers[i.an] # source des données
      ##
      # dates
      ziff.temp$date_cap <- lubridate::ymd(ziff.temp$date_cap)
      ziff.temp$date_deb <- lubridate::ymd(ziff.temp$date_deb)
      ziff.temp$annee <- with(
        ziff.temp,
        ifelse(
          is.na(date_cap),
          lubridate::year(date_deb),
          lubridate::year(date_cap)
        )
      )
      if (!is.null(annees)) {
        ziff.temp <- ziff.temp[ziff.temp$annee %in% annees, ]
      }
      ziff.temp$mois_cap <- lubridate::month(ziff.temp$date_cap)
      ziff.temp$mois_deb <- lubridate::month(ziff.temp$date_deb)
      ziff.temp$mois <- with(
        ziff.temp,
        ifelse(is.na(mois_cap), mois_deb, mois_cap)
      )
      ziff.temp$jour_cap <- lubridate::day(ziff.temp$date_cap)
      ziff.temp$jour_deb <- lubridate::day(ziff.temp$date_deb)
      ziff.temp$jour <- with(
        ziff.temp,
        ifelse(is.na(jour_cap), jour_deb, jour_cap)
      )
      ziff.temp$trim_cap <- lubridate::quarter(ziff.temp$date_cap)
      ziff.temp$trim_deb <- lubridate::quarter(ziff.temp$date_deb)
      ziff.temp$trim <- with(
        ziff.temp,
        ifelse(is.na(trim_cap), trim_deb, trim_cap)
      )
      ziff.temp$annee_gestion <- ziff.temp$annee
      ziff.temp[
        which(ziff.temp$annee > 1999 & ziff.temp$mois <= 4),
        'annee_gestion'
      ] <- ziff.temp[
        which(ziff.temp$annee > 1999 & ziff.temp$mois <= 4),
        'annee'
      ] -
        1
      ziff.temp[
        which(
          ziff.temp$annee > 1999 & ziff.temp$mois == 5 & ziff.temp$jour < 15
        ),
        'annee_gestion'
      ] <-
        ziff.temp[
          which(
            ziff.temp$annee > 1999 & ziff.temp$mois == 5 & ziff.temp$jour < 15
          ),
          'annee'
        ] -
        1

      ## mise en forme de l'opano
      ## table(ziff.temp$opano, useNA='always')
      ## table(ziff.temp$div, useNA='always')
      ziff.temp[ziff.temp$opano %in% c("", "XXX"), 'opano'] <- NA
      ziff.temp[ziff.temp$div %in% c("", "XXX", "XXXX"), 'div'] <- NA
      ziff.temp$opano <- trimws(toupper(ziff.temp$opano))
      ziff.temp$div <- trimws(toupper(ziff.temp$div))

      ##
      ## continuer ici à polir les données
      ##

      if (dir.exists(dir_sauvegarde_locale)) {
        message(paste0(fichiers[i.an], ": mise à jour du cache local."))
        saveRDS(
          ziff.temp,
          file.path(dir_sauvegarde_locale, paste0(nom_local, '.rds')),
          compress = TRUE
        )
      }
    }
    ziff.init[[i.an]] <- ziff.temp
  }
  ##
  ziff <- data.table::rbindlist(ziff.init, fill = TRUE)
}
