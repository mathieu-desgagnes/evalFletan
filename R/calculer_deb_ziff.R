#' Calcul les débarquements tel qu'inscrit dans les fichiers ziff
#'
#' Calculer les débarquements annee gestion, annee gestion par region, annee civile
#'
#' @param ziff data.frame() produit par la fonction `lireZiff()`
#'
#' @return ?
#'
calculer_deb_ziff <- function(ziff) {
  anneeZiff <- sort(unique(ziff$annee))
  ziff[which(ziff$regionZiff == 'L'), 'regionZiff'] <- 'Q'
  lesSources <- c('G', 'N', 'Q', 'S')
  obj <- array(
    0,
    dim = c(length(anneeZiff), 8, 5),
    dimnames = list(
      annee = NULL,
      debarquement = c(
        'annee',
        'tout',
        'toutAnneeCivile',
        'vise',
        'accessoire',
        'palangre',
        'nonPalangre',
        'visePalangre'
      ),
      source = c('GNQS', 'G', 'N', 'Q', 'S')
    )
  )
  obj[, 'annee', ] <- anneeZiff
  for (i.source in lesSources) {
    for (i.an in seq_along(anneeZiff)) {
      ziff.t <- subset(
        ziff,
        annee == anneeZiff[i.an] &
          regionZiff == i.source &
          nbpc <= 190000
      )
      if (nrow(ziff.t) > 0) {
        obj[i.an, 'toutAnneeCivile', i.source] <- sum(
          ziff.t$pds_vif,
          na.rm = TRUE
        ) /
          1000
      }
      ##
      ziff.t <- subset(ziff, annee == anneeZiff[i.an] & regionZiff == i.source)
      if (nrow(ziff.t) > 0) {
        obj[i.an, 'tout', i.source] <- sum(ziff.t$pds_vif, na.rm = TRUE) / 1000
        ziff.temp <- subset(ziff.t, prespvis == 130, select = pds_vif)
        if (nrow(ziff.temp) > 0) {
          obj[i.an, 'vise', i.source] <- sum(ziff.temp, na.rm = TRUE) / 1000
        }
        ziff.temp <- subset(
          ziff.t,
          prespvis %in%
            c(
              144,
              100,
              120,
              702,
              176,
              142,
              140,
              143,
              141,
              171,
              362,
              928,
              177,
              199,
              356,
              250,
              149,
              110
            ),
          select = pds_vif
        ) #toutes especes commerciales, sauf invertébrés
        if (nrow(ziff.temp) > 0) {
          obj[i.an, 'accessoire', i.source] <- sum(ziff.temp, na.rm = TRUE) /
            1000
        }
        ziff.temp <- subset(ziff.t, engin == 51, select = pds_vif)
        if (nrow(ziff.temp) > 0) {
          obj[i.an, 'palangre', i.source] <- sum(ziff.temp, na.rm = TRUE) / 1000
        }
        ziff.temp <- subset(
          ziff.t,
          engin %in% c(41, 12, 19, 22, 21, 59, 15, 16, 63),
          select = pds_vif
        )
        if (nrow(ziff.temp) > 0) {
          obj[i.an, 'nonPalangre', i.source] <- sum(ziff.temp, na.rm = TRUE) /
            1000
        }
        ziff.temp <- subset(
          ziff.t,
          prespvis == 130 & engin == 51,
          select = pds_vif
        )
        if (nrow(ziff.temp) > 0) {
          obj[i.an, 'visePalangre', i.source] <- sum(ziff.temp, na.rm = TRUE) /
            1000
        }
      }
    }
  }
  for (i.an in seq_along(anneeZiff)) {
    ziff.t <- subset(ziff, annee == anneeZiff[i.an])
    if (nrow(ziff.t) > 0) {
      obj[i.an, 'toutAnneeCivile', 'GNQS'] <- sum(
        ziff.t$pds_vif,
        na.rm = TRUE
      ) /
        1000
    }
    ##
    ziff.t <- subset(ziff, annee == anneeZiff[i.an])
    if (nrow(ziff.t) > 0) {
      obj[i.an, 'tout', 'GNQS'] <- sum(ziff.t$pds_vif, na.rm = TRUE) / 1000
      ziff.temp <- subset(ziff.t, prespvis == 130, select = pds_vif)
      if (nrow(ziff.temp) > 0) {
        obj[i.an, 'vise', 'GNQS'] <- sum(ziff.temp, na.rm = TRUE) / 1000
      }
      ziff.temp <- subset(
        ziff.t,
        prespvis %in%
          c(
            144,
            100,
            120,
            702,
            176,
            142,
            140,
            143,
            141,
            171,
            362,
            928,
            177,
            199,
            356,
            250,
            149,
            110
          ),
        select = pds_vif
      ) #toutes especes commerciales, sauf invertébrés
      if (nrow(ziff.temp) > 0) {
        obj[i.an, 'accessoire', 'GNQS'] <- sum(ziff.temp, na.rm = TRUE) / 1000
      }
      ziff.temp <- subset(ziff.t, engin == 51, select = pds_vif)
      if (nrow(ziff.temp) > 0) {
        obj[i.an, 'palangre', 'GNQS'] <- sum(ziff.temp, na.rm = TRUE) / 1000
      }
      ziff.temp <- subset(
        ziff.t,
        engin %in% c(41, 12, 19, 22, 21, 59, 15, 16, 63),
        select = pds_vif
      )
      if (nrow(ziff.temp) > 0) {
        obj[i.an, 'nonPalangre', 'GNQS'] <- sum(ziff.temp, na.rm = TRUE) / 1000
      }
      ziff.temp <- subset(
        ziff.t,
        prespvis == 130 & engin == 51,
        select = pds_vif
      )
      if (nrow(ziff.temp) > 0) {
        obj[i.an, 'visePalangre', 'GNQS'] <- sum(ziff.temp, na.rm = TRUE) / 1000
      }
    }
  }
  if (FALSE) {
    i.source.nom <- 'GNQS'
    plot(
      obj[, 'tout', i.source.nom],
      apply(obj[, c('vise', 'accessoire'), i.source.nom], 1, sum)
    )
    abline(a = 0, b = 1)
    text(
      obj[, 'tout', i.source.nom],
      apply(obj[, c('vise', 'accessoire'), i.source.nom], 1, sum),
      pos = 3,
      labels = obj[, 'annee', i.source.nom],
      cex = 0.5
    )
    points(obj[, 'tout', i.source.nom], obj[, c('vise'), i.source.nom], col = 2)
    abline(a = 0, b = 1)
    points(
      obj[, 'tout', i.source.nom],
      obj[, c('accessoire'), i.source.nom],
      col = 4
    )
    abline(a = 0, b = 1)
    ##
    plot(obj[, c('annee', 'vise'), i.source.nom], col = 4)
    abline(h = 0)
    points(obj[, c('annee', 'accessoire'), i.source.nom], col = 2)
    points(
      obj[, 'annee', i.source.nom],
      obj[, 'tout', i.source.nom] -
        apply(obj[, c('vise', 'accessoire'), i.source.nom], 1, sum),
      pch = 16,
      col = 1
    )
    plot(
      obj[, 'tout', i.source.nom],
      apply(obj[, c('palangre', 'nonPalangre'), i.source.nom], 1, sum)
    )
    abline(a = 0, b = 1)
    text(
      obj[, 'tout', i.source.nom],
      apply(obj[, c('palangre', 'nonPalangre'), i.source.nom], 1, sum),
      pos = 3,
      labels = obj[, 'annee', i.source.nom],
      cex = 0.5
    )
  }
  ## VALIDER SI PERTINENT DE FAIRE PAR RÉGION
  ##
  ## regionNom <- names(table(ziff$regionZiff))
  ## obj[['debarquementParRegion']] <- array(dim=c(length(anneeZiff), length(regionNom)), dimnames=list(annee=anneeZiff, region=regionNom))
  ## for(i.region in seq_along(regionNom)){
  ##     for(i.an in seq_along(anneeZiff)){
  ##         if(anneeZiff[i.an]<=1998){
  ##             obj[['debarquementParRegion']][i.an,i.region] <- sum(subset(ziff,
  ##                                                             divisionOpano%in%c('4R','4S','4T') & regionZiff==regionNom[i.region] &
  ##                                                             annee==anneeZiff[i.an],
  ##                                                         select='pds_vif'), na.rm=TRUE) / 1000
  ##         }
  ##         if(anneeZiff[i.an]==1999){
  ##             obj[['debarquementParRegion']][i.an,i.region] <- sum(subset(ziff, divisionOpano%in%c('4R','4S','4T') & regionZiff==regionNom[i.region] &
  ##                                                         (annee==anneeZiff[i.an] | (annee==anneeZiff[i.an+1]&annee==anneeZiff[i.an])),
  ##                                                         select='pds_vif'), na.rm=TRUE) / 1000
  ##         }
  ##         if(anneeZiff[i.an]>=2000){
  ##             temp <- subset(ziff, divisionOpano%in%c('4R','4S','4T') & regionZiff==regionNom[i.region] & annee==anneeZiff[i.an],
  ##                            select='pds_vif')
  ##             if(nrow(temp)>0) obj[['debarquementParRegion']][i.an,i.region] <- sum(temp, na.rm=TRUE) / 1000
  ##         }
  ##     }
  ## }
  ##
  ## pds_vif de flétan (prespvis==130) à la palangre (engin==51)
  return(obj)
}
