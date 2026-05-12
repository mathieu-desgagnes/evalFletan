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
#' @param ne_pas_mettre_a_jour_version_locale si TRUE, utiliser la version présente à `dir_sauvegarde_locale` sans mettre à jour les fichier. Utile pour reproduire des calculs antérieurs.
#'
#' @importFrom  data.table fread rbindlist
#' @importFrom  readxl read_excel
#' @import  lubridate
#'
#' @return
#'
lireZiff <- function(
  no_espece = NULL,
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
  dir_sauvegarde_locale = NULL,
  ne_pas_mettre_a_jour_version_locale = FALSE
) {
  ##
  if (is.null(no_espece) && is.null(no_espece_visee)) {
    stop(
      'Au moins une espèce capturée et/ou une espèce visée doit être indiquée.'
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
      '_espCapt-',
      if (is.null(no_espece) || length(no_espece) == 0) {
        "NA"
      } else {
        paste(no_espece, collapse = "-")
      },
      '_espVis-',
      if (is.null(no_espece_visee) || length(no_espece_visee) == 0) {
        "NA"
      } else {
        paste(no_espece_visee, collapse = "-")
      }
    )
    ##
    ##
    ## déterminer si des versions locales existent et s'il y a besoin de mise à jour
    exist_dossier_v_locale <- !is.null(dir_sauvegarde_locale) &&
      dir.exists(paths = dir_sauvegarde_locale)
    exist_fichier_v_locale <- exist_dossier_v_locale &&
      file.exists(file.path(
        dir_sauvegarde_locale,
        paste0(nom_local, '.rds')
      ))
    besoin_maj <- TRUE
    if (!exist_fichier_v_locale) {
      message(
        paste("Il n'y a pas de version locale du fichier", fichiers[i.an])
      )
    } else {
      temps.f_reseau <- file.info(file.path(dir_input, fichiers[i.an]))$mtime
      temps.f_local <- file.info(file.path(
        dir_sauvegarde_locale,
        paste0(nom_local, '.rds')
      ))$mtime
      besoin_maj <- is.na(temps.f_reseau) ||
        is.na(temps.f_local) ||
        (temps.f_reseau > temps.f_local)
    }
    ##
    ##
    ## lire le ficher local si approprié, sinon lire le fichier réseau, polir celui et possiblement l'enregistrer localement
    if (
      (ne_pas_mettre_a_jour_version_locale && exist_fichier_v_locale) ||
        !besoin_maj
    ) {
      message(paste0(
        fichiers[i.an],
        ": aucune mise à jour nécessaire, lecture depuis le cache local."
      ))
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
        stringsAsFactors = FALSE
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

      ## calcul des provinces d'attache
      ## table(floor(ziff.temp$port_att/10000), useNA='always')
      ziff.temp$prov_att <- c(
        'Inconnu',
        'N-É',
        'N-B',
        'IPE',
        'QC',
        'T-N',
        rep(NA, 2)
      )[floor(ziff.temp$port_att / 10000) + 1]
      ## table(ziff.temp$annee, ziff.temp$prov_att, useNA='always')
      ## table(floor(ziff.temp$port_deb/10000), useNA='always')
      ziff.temp$prov_deb <- c(
        'Inconnu',
        'N-É',
        'N-B',
        'IPE',
        'QC',
        'T-N',
        rep(NA, 2)
      )[floor(ziff.temp$port_deb / 10000) + 1]
      ## table(ziff.temp$prov_deb, useNA='always')

      ## uniformiser mesure de quantité débarqué a Kilogramme (dans la forme débarquée)
      ## table(ziff.temp$un_mes, useNA='always')
      ziff.temp$pd_deb[ziff.temp$un_mes == 'P'] <- ziff.temp$pd_deb[
        ziff.temp$un_mes == 'P'
      ] *
        0.453592
      ziff.temp$pd_deb_kg[ziff.temp$un_mes %in% c('', 'U')] <- NA
      ziff.temp$un_mes[ziff.temp$un_mes == 'P'] <- 'KfromP'
      ## table(ziff.temp$un_mes, useNA='always')

      ## ajouter les noms des espèces et engins en anglais, francais et latin
      if (
        !exists(file.path(
          dir_input,
          'Documentation',
          'Dictionnaire_ZIF_en_cours.xlsx'
        ))
      ) {
        message(
          "Le dictionnaires des noms d'espèces et d'engin n'est pas disponible."
        )
      } else {
        ## Espèces débarquées
        espece <- readxl::read_excel(
          path = file.path(
            dir_input,
            'Documentation',
            'Dictionnaire_ZIF_en_cours.xlsx'
          ),
          sheet = 'Espece',
          col_names = TRUE
        )
        espece <- espece[, !(names(espece) %in% 'Remarques')]
        names(espece) <- c("cod_esp", "cod_esp_en", "cod_esp_fr", "cod_esp_lat")
        ziff.temp <- merge(ziff.temp, espece, by = "cod_esp", all.x = TRUE)

        ## Espèces visées
        names(espece) <- c(
          "prespvis",
          "prespvis_en",
          "prespvis_fr",
          "prespvis_lat"
        )
        ziff.temp <- merge(ziff.temp, espece, by = "prespvis", all.x = TRUE)

        ## Espèces principales
        names(espece) <- c(
          "prespcap",
          "prespcap_en",
          "prespcap_fr",
          "prespcap_lat"
        )
        ziff.temp <- merge(ziff.temp, espece, by = "prespcap", all.x = TRUE)
        rm(espece)

        ## Engins
        engin <- readxl::read_excel(
          path = file.path(
            dir_input,
            'Documentation',
            'Dictionnaire_ZIF_en_cours.xlsx'
          ),
          sheet = 'Engins',
          col_names = TRUE
        )
        engin <- engin[, 1:3]
        names(engin) <- c("engin", "engin_fr", "engin_en")
        ziff.temp <- merge(ziff.temp, engin, by = "engin", all.x = TRUE)
        rm(engin)
      }
      ziff.temp$catEngin <- categorieEngin(ziff.temp$engin)

      ## sauvegarde locale si approprié
      if (
        !ne_pas_mettre_a_jour_version_locale &&
          exist_dossier_v_locale &&
          besoin_maj
      ) {
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
  ziff <- as.data.frame(data.table::rbindlist(
    ziff.init,
    fill = TRUE
  ))
}


#' Détermine une catégorie d'engin de pêche selon les numéros d'engins.
#'
#' @param x un vecteur de numéros d'engins à classifier en catégories
#'
#' @returns une table en trois colonnes des catégories d'engin, soit le nom du type d'engin, l'étiquette en francais et l'étiquette en anglais
#' @export
#'
#' @examples
categorieEngin <- function(x) {
  ## x est un vecteur de caracteres à identifier comme type d'engin
  ## table(x, useNA='ifany')
  resultat <- array(
    NA,
    dim = c(length(x), 3),
    dimnames = list(NULL, c('nom', 'etiquetteFR', 'etiquetteEN'))
  )
  for (i in seq_along(x)) {
    if (x[i] %in% c(0, 7, 71, 99, 110)) {
      resultat[i, ] <- c('autresInconnu', 'Indéterminé', 'Undetermined')
    }
    if (x[i] %in% c(11, 12, 15, 16, 19)) {
      resultat[i, ] <- c('chaluts', 'Chaluts', 'Bottom trawl')
    }
    if (x[i] %in% c(21, 22)) {
      resultat[i, ] <- c('seines', 'Seine', 'Seine')
    }
    if (x[i] %in% c(41)) {
      resultat[i, ] <- c('filetsMaillants', 'Filet maillant', 'Gill net')
    }
    if (x[i] %in% c(50, 51)) {
      resultat[i, ] <- c('palangres', 'Palangre', 'Longline')
    }
    if (x[i] %in% c(53, 55, 59)) {
      resultat[i, ] <- c('enginsManuels', 'Engins manuels', 'Manual equipment')
    }
    if (x[i] %in% c(61, 62, 67)) {
      resultat[i, ] <- c('trappes', 'Trappe', 'trap')
    }
    if (x[i] %in% c(71)) {
      resultat[i, ] <- c('dragues', 'Drague', 'Dredge')
    }
    ##
    if (x[i] %in% c('NK')) {
      resultat[i, ] <- c('autresInconnu', 'Indéterminé', 'Undetermined')
    }
    if (x[i] %in% c('OTB1', 'OTB2', 'GRL1', 'GRL2', 'TT')) {
      resultat[i, ] <- c('chaluts', 'Chaluts', 'Bottom trawl')
    }
    if (x[i] %in% c('SSC', 'SDN')) {
      resultat[i, ] <- c('seines', 'Seine', 'Seine')
    }
    if (x[i] %in% c('GNS')) {
      resultat[i, ] <- c('filetsMaillants', 'Filet maillant', 'Gill net')
    }
    if (x[i] %in% c('LLS', 'LL', 'LLD')) {
      resultat[i, ] <- c('palangres', 'Palangre', 'Longline')
    }
    if (x[i] %in% c('LX', 'LHP')) {
      resultat[i, ] <- c('enginsManuels', 'Engins manuels', 'Manual equipment')
    }
    if (x[i] %in% c('FPO')) {
      resultat[i, ] <- c('trappes', 'Trappe', 'trap')
    }
    if (x[i] %in% c(71)) resultat[i, ] <- c('dragues', 'Drague', 'Dredge')
  }
  ##
  resultat
}
