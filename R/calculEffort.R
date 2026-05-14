#' Selectionne les données ziff utilisables pour le calcul de l'effort
#'
#' Des sorties graphiques et des diagnostiques des données disponibles sont aussi codés dans la fonction, mais doivent encore être valorisés.
#'
#' @param ziff fichier ziff des captures de flétan contenant les division opano ciblées
#'
#' @return un data.frame des colonnes nécessaires du ziff
#'
#'
# calculerZiffPue <- function(dirOutput = 'ouput/', ziff) {
calculerZiffPue <- function(ziff) {
  ## Diane Archambeault à fournit des données à son départ. Celle-ci sont le résultat d'appels aux différentes région pour valider les valeurs d'effort
  ## Ces travaux n'ont pu être reproduits et il doit être décidé si ces données sont utilisées ou non.
  if (FALSE) {
    ziff <- data$ziff$ziff
    table(ziff$especePrincipale, ziff$annee, useNA = 'ifany')
    table(ziff$especeVisee, ziff$annee, useNA = 'ifany')
    ## si l'espèce visée est le flétan, mais que ce n'est pas l'espèce principale
    test <- subset(ziff, especeVisee == 130 & especePrincipale != 130)
    sort(table(test$especePrincipale, useNA = 'ifany'))
    ## si l'espèce visée n'est pas le flétan, mais que l'espèce principale est le flétan
    test <- subset(ziff, especeVisee != 130 & especePrincipale == 130)
    sort(table(test$especeVisee, useNA = 'ifany')) #noter le nombre élevé de "0" ici
    ##
    table(ziff$especeVisee, ziff$especePrincipale, useNA = 'ifany')
    test <- subset(
      ziff,
      especePrincipale == 130 & opano %in% c('4R', '4S', '4T')
    )
    temp <- aggregate(test$pds_vif / 1000, test['especeVisee'], FUN = sum)
    temp[order(temp$x), ]
    aggregate(
      test$pds_vif / 1000,
      test[, c('especeVisee', 'annee')],
      FUN = sum
    )
    aggregate(
      test$pds_vif / 1000,
      test[, c('especeVisee', 'annee', 'regionZiff')],
      FUN = sum
    ) #(0-S: <1988, 0-Q: <1998, 999-N: 1993, 0G: <2003)
    table(ziff$fractionDeVoyage, useNA = 'ifany')
  }
  if (FALSE) {
    #analyser "espece" vs "especeVisee" vs "especePrincipale"
    test <- subset(ziff, opano %in% c('4R', '4S', '4T'))
    temp <- aggregate(
      test$pds_vif / 1000,
      test[, c('especeVisee', 'annee')],
      FUN = sum
    )
    temp
    temp <- aggregate(
      test$pds_vif / 1000,
      test[, c('especeVisee', 'especePrincipale', 'annee')],
      FUN = sum
    )
    temp
    temp2 <- temp[order(temp$x), ]
    temp2[which(temp2$especeVisee != 130), ]
    anneeZiff <- sort(unique(ziff$annee))
    regionZiff.temp <- sort(unique(ziff$regionZiff))
    obj <- array(
      NA,
      dim = c(length(anneeZiff), length(regionZiff.temp), 2, 2),
      dimnames = list(
        annee = anneeZiff,
        region = regionZiff.temp,
        visee = c('fletan', 'autre'),
        principale = c('fletan', 'autre')
      )
    )
    for (i in seq_along(anneeZiff)) {
      for (j in seq_along(regionZiff.temp)) {
        temp <- subset(
          ziff,
          annee == anneeZiff[i] &
            regionZiff == regionZiff.temp[j] &
            opano %in% c('4R', '4S', '4T')
        )
        if (nrow(temp) > 0) {
          obj[i, j, 'fletan', 'fletan'] <- sum(
            temp[
              which(temp$especeVisee == 130 & temp$especePrincipale == 130),
              'pds_vif'
            ] /
              1000
          )
          obj[i, j, 'autre', 'fletan'] <- sum(
            temp[
              which(temp$especeVisee != 130 & temp$especePrincipale == 130),
              'pds_vif'
            ] /
              1000
          )
          obj[i, j, 'fletan', 'autre'] <- sum(
            temp[
              which(temp$especeVisee == 130 & temp$especePrincipale != 130),
              'pds_vif'
            ] /
              1000
          )
          obj[i, j, 'autre', 'autre'] <- sum(
            temp[
              which(temp$especeVisee != 130 & temp$especePrincipale != 130),
              'pds_vif'
            ] /
              1000
          )
        }
      }
    }
  }
  ## considérer à part les zéros (inconnu) dans "visée", car il peut y avoir du visé aussi => (0-S: <1988, 0-Q: <1998, 999-N: 1993, 0G: <2003)
  ## mais en gros, à partir de 2003, visée=130 couvre un très grande partie des captures, il est conseillé d'utiliser ce critère de sélection
  ##
  ## conserver les 'espèce visé = flétan' et 'espèce capturée = flétan'
  ziff.init <- subset(ziff, cod_esp == 130)
  nrow(ziff)
  nrow(ziff.init) #espèce capturée
  sommaire <- aggregate(
    ziff.init$pds_vif / 1000,
    ziff.init[, c('opano', 'annee')],
    FUN = sum
  )
  names(sommaire) <- c(head(names(sommaire), -1), 'total')
  ziff.init$cat_bat[ziff.init$lht < 35] <- '<35'
  ziff.init$cat_bat[ziff.init$lht >= 35 & ziff.init$lht < 45] <- '35a45'
  ziff.init$cat_bat[ziff.init$lht >= 45 & ziff.init$lht < 65] <- '45a65'
  ziff.init$cat_bat[ziff.init$lht >= 65] <- '65+'
  ##
  zp.init <- subset(ziff.init, prespvis == 130)
  nrow(zp.init) #espèce visée
  sommaire <- merge(
    sommaire,
    aggregate(
      zp.init$pds_vif / 1000,
      zp.init[, c('opano', 'annee')],
      FUN = sum
    ),
    all = TRUE
  )
  names(sommaire) <- c(head(names(sommaire), -1), 'vise')
  sommaire[is.na(sommaire['vise']), 'vise'] <- 0
  ## cbind(sommaire[,c('opano','annee','total')], round(sommaire$vise / sommaire$total * 100))

  ## conserver les 'palangre'
  zp.init <- subset(zp.init, engin == 51)
  nrow(zp.init) #engin = palangre
  sommaire <- merge(
    sommaire,
    aggregate(
      zp.init$pds_vif / 1000,
      zp.init[, c('opano', 'annee')],
      FUN = sum
    ),
    all = TRUE
  )
  names(sommaire) <- c(head(names(sommaire), -1), 'visePalangre')
  sommaire[is.na(sommaire['visePalangre']), 'visePalangre'] <- 0
  ## cbind(sommaire[,c('opano','annee','total')], round(sommaire$visePalangre / sommaire$total * 100))

  ## temp <- aggregate(zp.init$pds_vif/1000, by=zp.init[,c('opano','annee')], FUN=sum)
  ## temp[order(temp$x),]
  ## summary(zp.init$nb_engin)
  ##
  ## Étudier la contribution en nb_engin par région
  temp2 <- aggregate(
    zp.init$nb_engin,
    zp.init[, c('region', 'annee')],
    FUN = median,
    na.rm = TRUE
  )
  temp3 <- aggregate(
    zp.init$pds_vif / 1000,
    zp.init[, c('region', 'annee')],
    FUN = sum,
    na.rm = TRUE
  )
  temp <- merge(
    temp2,
    temp3,
    by = c('region', 'annee'),
    all = TRUE,
    suffixes = c('.nb_engin', '.pds_vif')
  )
  ## Conclusion: problème avec nb_engin provenant de S certaines annees, provenant de Q avant 2003, provenant de N, provenant de L, provenant de G <2006
  ## Solution: utiliser uniquement certaines données? (région, province, flottille, etc.)
  temp2 <- aggregate(
    zp.init$jr_peche,
    zp.init[, c('region', 'annee')],
    FUN = median,
    na.rm = TRUE
  )

  if (FALSE) {
    ## nombre d'engins
    summary(zp.init$nb_engin)
    hist(zp.init$nb_engin, breaks = seq(0, 90001, by = 50))
    hist(zp.init$nb_engin, breaks = seq(0, 90001, by = 50), xlim = c(0, 10000))
    hist(zp.init$nb_engin, breaks = seq(-0.5, 90001, by = 1), xlim = c(0, 10))
    hist(
      zp.init[zp.init$annee >= 2003, 'nb_engin'],
      breaks = seq(0, 90001, by = 50),
      xlim = c(0, 10000)
    )
  }
  ## mettre la limite à >= 50
  nrow(zp.init)
  zp.init <- subset(zp.init, is.finite(nb_engin) & nb_engin >= 50)
  nrow(zp.init)
  ## zp.init <- subset(zp.init, is.finite(nb_engin) & nb_engin>=100); nrow(zp.init)
  sommaire <- merge(
    sommaire,
    aggregate(
      zp.init$pds_vif / 1000,
      zp.init[, c('opano', 'annee')],
      FUN = sum
    ),
    all = TRUE
  )
  names(sommaire) <- c(head(names(sommaire), -1), 'visePal.plus50engin')
  sommaire[is.na(sommaire['visePal.plus50engin']), 'visePal.plus50engin'] <- 0
  ## cbind(sommaire[,c('opano','annee','total')], round(sommaire$visePal.plus50engin / sommaire$total * 100))

  ## nbHeure n'est pas retenu comme facteur dans le modèle, (de plus, les donnees des maritimes n'ont pas d'heures d'immersion valides)
  if (FALSE) {
    ## hist(zp.init$nbHeure, breaks=seq(-0.1,350,by=2), xlim=c(0,50))
    ## table(zp.init$annee, zp.init$nbHeure, useNA='always')
    ## par(mfrow=c(1,2))
    ## plot(zp.init$nbHeure, zp.init$pds_vif)
    ## plot(zp.init$nb_engin, zp.init$pds_vif)
    ## plot(zp.init$nbHeure, zp.init$nb_engin)
    ## ## hist(zp.init$jr_peche, breaks=seq(0,17,by=0.1))
    ## ## hist(zp.init$jr_mouil, breaks=seq(0,400,by=0.1), xlim=c(0,20))
    ## ## hist(zp.init$jr_mer, breaks=seq(0,1000,by=0.1), xlim=c(0,20))
    ## table(zp.init$jr_peche, zp.init$annee, useNA='ifany')
    ## table(zp.init$jr_mouil, zp.init$annee, useNA='ifany')
    ## plot(zp.init$jr_mouil, zp.init$jr_peche, xlim=c(0,15)); abline(a=0,b=1, col=2)
    ## zp.init <- subset(zp.init, is.finite(eff_hre) & eff_hre>=1); nrow(zp.init)
    ## hist(zp.init$eff_hre, breaks=seq(0,350,by=2))
    ## hist(zp.init$eff_hre, breaks=seq(-0.5,350,by=1), xlim=c(0,12))
    ## hist(zp.init$pds_vif, breaks=seq(0,10200,by=20))
    par(mfrow = c(2, 2))
    temp <- subset(zp.init, annee >= 1998)
    plot(
      aggregate(temp$nb_engin, temp['annee'], FUN = mean, na.rm = TRUE),
      ylim = c(0, 4000),
      ylab = 'nb moy, engin'
    )
    abline(h = 0, col = 'grey70')
    plot(
      aggregate(temp$eff_hre, temp['annee'], FUN = sum, na.rm = TRUE),
      ylim = c(0, 35000),
      ylab = 'nb moy, heure peche'
    )
    abline(h = 0, col = 'grey70')
    plot(
      aggregate(temp$jr_peche, temp['annee'], FUN = sum, na.rm = TRUE),
      ylim = c(0, 7500),
      ylab = 'nb moy, jour peche'
    )
    abline(h = 0, col = 'grey70')
    plot(
      aggregate(temp$jr_mouil, temp['annee'], FUN = sum, na.rm = TRUE),
      ylim = c(0, 9000),
      ylab = 'nb moy, jour mouillage'
    )
    abline(h = 0, col = 'grey70')
    ##
    plot(
      aggregate(temp$eff_hre, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      aggregate(temp$jr_peche, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      xlim = c(0, 35000),
      ylim = c(0, 7500)
    )
    text(
      x = aggregate(
        temp$eff_hre,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      )$x,
      y = aggregate(temp$jr_peche, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      labels = aggregate(
        temp$eff_hre,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      )$annee,
      pos = 3
    )
    plot(
      aggregate(temp$eff_hre, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      aggregate(temp$jr_mouil, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      xlim = c(0, 35000),
      ylim = c(0, 9000)
    )
    text(
      x = aggregate(
        temp$eff_hre,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      )$x,
      y = aggregate(
        temp$jr_mouil,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      )$x,
      labels = aggregate(
        temp$eff_hre,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      )$annee,
      pos = 3
    )
    plot(
      aggregate(temp$eff_hre, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      aggregate(temp$jr_mer, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      xlim = c(0, 35000),
      ylim = c(0, 15000)
    )
    text(
      x = aggregate(
        temp$eff_hre,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      )$x,
      y = aggregate(temp$jr_mer, temp['annee'], FUN = sum, na.rm = TRUE)$x,
      labels = aggregate(
        temp$eff_hre,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      )$annee,
      pos = 3
    )
    plot(
      aggregate(
        temp$jr_peche * temp$nb_engin / 1000,
        temp['annee'],
        FUN = sum,
        na.rm = TRUE
      ),
      ylim = c(0, 13000)
    )
    hist(temp$jr_peche, breaks = seq(0, 5, by = 0.1))
    hist(temp$jr_mer, breaks = seq(0, 1000, by = 0.1), xlim = c(0, 5))
    hist(temp$jr_mouil, breaks = seq(0, 1000, by = 0.1), xlim = c(0, 5))

    ## donc vérifier:
    ## la distribution des trois efforts
    ## l,impact sur la comparaison poidsvif/effort
    ## relation avec frn_voy
  }

  ## éliminer les effort en double (exemple: même effort, mais capture splitté entre deux acheteurs)
  apply(
    zp.init[
      apply(
        zp.init[, c(
          'codeNBPC',
          'lhtBat',
          'portAttache',
          'opano',
          'opano',
          'engin',
          'espece',
          'nb_engin',
          'eff_hre',
          'regionZiff',
          'anneeCivile',
          'province',
          'longitudeGIS',
          'latitudeGIS',
          'debMois',
          'debJour',
          'captMois',
          'captJour',
          'annee',
          'catBat'
        )],
        1,
        function(x) {
          any(is.na(x))
        }
      ),
      c(
        'codeNBPC',
        'lhtBat',
        'portAttache',
        'opano',
        'opano',
        'engin',
        'espece',
        'nb_engin',
        'eff_hre',
        'regionZiff',
        'anneeCivile',
        'province',
        'longitudeGIS',
        'latitudeGIS',
        'debMois',
        'debJour',
        'captMois',
        'captJour',
        'annee',
        'catBat'
      )
    ],
    2,
    function(x) {
      any(is.na(x))
    }
  )

  table(zp.init$regionZiff, useNA = 'ifany')
  zPue <- aggregate(
    zp.init$pds_vif,
    by = zp.init[, c(
      'codeNBPC',
      'lhtBat',
      'opano',
      'opano',
      'regionZiff',
      'nb_engin',
      'anneeCivile',
      'debMois',
      'debJour',
      'anneeCivile',
      'annee',
      'catBat'
    )],
    FUN = sum
  )
  nrow(zPue)
  ## table(zPue$pavillonEffort, zPue$annee)
  ## table(zPue$pavillonEffort, zPue$regionZiff)
  ## apply(table(zPue$pavillonEffort, zPue$regionZiff), 1, sum)
  ## tail(zPue[,c('pavillonEffort','codeNBPC','tonnnageBat','lhtBat','hp','portAttache','opano','opano','engin','espece',
  ##              'nb_engin','eff_hre','regionZiff','anneeCivile','province',
  ##              'longitudeGIS','latitudeGIS','debMois','debJour','captMois','captJour','annee')],10)
  names(zPue) <- c(head(names(zPue), -1), 'pds_vif')

  ## zPue <- subset(zPue, opano%in%c('4S','4T'))

  tail(table(zPue[, 'annee'], zPue[, 'regionZiff'], useNA = 'ifany'))
  table(zPue[, 'annee'], zPue[, 'regionZiff'], useNA = 'ifany')
  sum(zPue$pds_vif) / 1000
  sum(zPue$pds_vif) / 1000

  ## data.pue <- zPue
  ## nrow(data.pue)

  ## if(!is.null(ssZoneVisee)){
  ##     data.pue <- subset(data.pue, opano==ssZoneVisee)
  ##     nrow(data.pue)
  ## }
  ## data.pue$codeNBPC <- as.factor(data.pue$codeNBPC)
  ## data.pue$mois <- as.factor(data.pue$debMois)
  ## data.pue$annee <- as.factor(data.pue$anneeCivile)
  ## data.pue$catImm6 <- cut(data.pue$eff_hre, breaks=c(0,6,12,18,24,36,200))#seq(0,200,by=6))
  ## data.pue$catImm12 <- cut(data.pue$eff_hre, breaks=seq(0,200,by=12))
  ## data.pue$catImm18 <- cut(data.pue$eff_hre, breaks=seq(0,200,by=18))
  ## data.pue$catImm24 <- cut(data.pue$eff_hre, breaks=seq(0,96,by=24))
  ## data.pue$catBat <- as.factor(data.pue$catBat)
  ## data.pue$opano <- as.factor(data.pue$opano)
  ## data.pue$opano <- as.factor(data.pue$opano)
  ## data.pue$effort <- data.pue$nb_engin*data.pue$eff_hre
  ## if(FALSE){                          #etude des données

  ## test <- table(zPue$codeNBPC, zPue$annee, useNA='ifany')
  ## nbpc.3anOuPlus <- names(which(apply(test, 1, function(x){sum(x>0)})>2))
  ## sum(zPue$pds_vif)/1000
  ## sum(subset(zPue, codeNBPC%in%nbpc.3anOuPlus)$pds_vif) / sum(zPue$pds_vif)

  ## test <- subset(zPue, !codeNBPC%in%nbpc.3anOuPlus)

  if (FALSE) {
    par(mfrow = c(1, 1))
    temp <- aggregate(zPue$pds_vif, zPue['annee'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    pue.annee <- temp
    temp <- aggregate(ziff.init$pds_vif, ziff.init['annee'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'tot')
    tot.annee <- temp
    prop <- merge(pue.annee, tot.annee, all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    plot(
      prop$annee,
      prop$prop * 100,
      type = 'b',
      main = 'Débarquements utilisables pour calcul de PUE commerciale',
      xlab = 'Année',
      ylab = '%',
      ylim = c(0, max(prop$prop * 100))
    )
    abline(h = c(0, 3.5), col = c('grey70', 'red'))
    axis(2, at = 3.5, labels = 3.5, col = 'red', col.axis = 'red')
  }
  ## conclusion: utiliser 1998/2003 et plus
  zPue <- subset(zPue, annee >= 1998)
  ziff.init <- subset(ziff.init, annee >= 1998)
  ## zPue <- subset(zPue, annee>=2003)
  ## ziff.init <- subset(ziff.init, annee>=2003)

  if (FALSE) {
    temp <- aggregate(zPue$pds_vif, zPue['catBat'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    pue.annee <- temp
    temp <- aggregate(ziff.init$pds_vif, ziff.init['catBat'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'tot')
    tot.annee <- temp
    prop <- merge(pue.annee, tot.annee, all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    plot(
      1:4,
      prop$prop * 100,
      main = 'Débarquements utilisables pour calcul de PUE commerciale',
      xlab = 'Catégorie de bateau (pieds)',
      ylab = '%',
      axes = FALSE,
      ylim = c(0, max(prop$prop * 100))
    )
    box()
    axis(2)
    axis(1, at = 1:4, labels = prop$catBat)
    abline(h = c(0), col = c('grey70'))
  }
  ## conclusion2021: retirer les 65+
  zPue <- subset(zPue, catBat != '65+')
  ziff.init <- subset(ziff.init, catBat != '65+')

  if (FALSE) {
    temp <- aggregate(zPue$pds_vif, zPue['debMois'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    pue.annee <- temp
    temp <- aggregate(ziff.init$pds_vif, ziff.init['debMois'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'tot')
    tot.annee <- temp
    prop <- merge(pue.annee, tot.annee, all = TRUE)
    prop$prop <- prop$pue / prop$tot
    prop[is.na(prop$prop), 'prop'] <- 0
    plot(
      prop$debMois,
      prop$prop * 100,
      main = 'Débarquements utilisables pour calcul de PUE commerciale',
      xlab = 'Mois',
      ylab = '%',
      ylim = c(0, max(prop$prop * 100))
    )
    axis(1, at = prop$debMois, labels = prop$debMois)
    abline(h = c(0), col = c('grey70'))
  }
  ## conclusion2024: retirer mois 1 et 2
  ## conclusion2023: conserver tous
  ## conclusion2021: retirer mois 12
  ## zPue <- subset(zPue, debMois%in%4:11)
  ## ziff.init <- subset(ziff.init, debMois%in%4:11)
  zPue <- subset(zPue, debMois %in% 3:12)
  ziff.init <- subset(ziff.init, debMois %in% 3:12)

  if (FALSE) {
    temp <- aggregate(zPue$pds_vif, zPue['opano'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    pue.annee <- temp
    temp <- aggregate(ziff.init$pds_vif, ziff.init['opano'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'tot')
    tot.annee <- temp
    prop <- merge(pue.annee, tot.annee, all = TRUE)
    prop$prop <- prop$pue / prop$tot
    prop[is.na(prop$prop), 'prop'] <- 0
    plot(
      1:length(prop$opano),
      prop$prop * 100,
      main = 'Débarquements utilisables pour calcul de PUE commerciale',
      xlab = 'Division OPANO',
      ylab = '%',
      ylim = c(0, max(prop$prop * 100)),
      axes = FALSE
    )
    box()
    axis(2)
    axis(1, at = 1:length(prop$opano), labels = prop$opano)
    abline(h = c(0), col = c('grey70'))
    ## conclusion: aucune
    ## zPue <- subset(zPue, opano%in%c('4S','4T'))
    ## ziff.init <- subset(ziff.init, opano%in%c('4S','4T'))

    temp <- aggregate(zPue$pds_vif, zPue['opano'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    pue.annee <- temp
    temp <- aggregate(ziff.init$pds_vif, ziff.init['opano'], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'tot')
    tot.annee <- temp
    prop <- merge(pue.annee, tot.annee, all = TRUE)
    prop$prop <- prop$pue / prop$tot
    prop[is.na(prop$prop), 'prop'] <- 0
    plot(
      1:length(prop$opano),
      prop$prop * 100,
      main = 'Débarquements utilisables pour calcul de PUE commerciale',
      xlab = 'Sous-division OPANO',
      ylab = '%',
      ylim = c(0, max(prop$prop * 100)),
      axes = FALSE
    )
    box()
    axis(2)
    axis(1, at = 1:length(prop$opano), labels = prop$opano, las = 3)
    abline(h = c(0, 3.5), col = c('grey70', 'red'))
    axis(2, at = c(3.5), labels = c(3.5), col = 'red', col.axis = 'red')
    prop[order(prop$prop), ]
  }
  ## conclusion 2024: retirer c('4R','4S') -> uniquement si on utilise 'opano', pas nécessaire si on utilise 'opano'
  ## conclusion: retirer c('4R','4RU','4S','4T','4Tl') -> uniquement si on utilise 'opano', pas nécessaire si on utilise 'opano'
  ## zPue <- subset(zPue, !opano%in%c('4R','4RU','4S','4T','4Tl'))
  ## ziff.init <- subset(ziff.init, !opano%in%c('4R','4RU','4S','4T','4Tl'))

  if (FALSE) {
    ## test <- aggregate(ziff.init$pds_vif, ziff.init[,c('annee','catBat','debMois','opano')], FUN=sum); names(test) <- c(head(names(test),-1), 'tot')
    ## test2 <- aggregate(zPue$pds_vif, zPue[,c('annee','catBat','debMois','opano')], FUN=sum)
    ## names(test2) <- c(head(names(test2),-1), 'pue')
    ## test3 <- merge(test, test2, all=TRUE); test3[is.na(test3$pue),'pue'] <- 0
    ## test3$prop <- test3$pue / test3$tot
    ## pairs(xtabs(prop~annee+catBat+debMois+opano, data=test3))
    ## mosaicpairs(xtabs(x~annee+catBat+debMois+opano, data=test))
  }

  if (FALSE) {
    ## vérification visuelle des combinaisons de facteurs (par graph mosaic)
    temp <- aggregate(zPue$pds_vif, zPue[, c('catBat', 'opano')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('catBat', 'opano')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('catBat', 'opano'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ catBat + opano, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ catBat + opano, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## pairs(xtabs(prop~catBat+opano, data=prop), col=rainbow(12))

    temp <- aggregate(zPue$pds_vif, zPue[, c('debMois', 'annee')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('debMois', 'annee')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('debMois', 'annee'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ annee + debMois, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ annee + debMois, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~annee+debMois, data=prop))

    temp <- aggregate(zPue$pds_vif, zPue[, c('catBat', 'annee')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('catBat', 'annee')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('catBat', 'annee'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ annee + catBat, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ annee + catBat, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~annee+catBat, data=prop))

    temp <- aggregate(zPue$pds_vif, zPue[, c('debMois', 'catBat')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('debMois', 'catBat')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('debMois', 'catBat'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ debMois + catBat, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ debMois + catBat, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~debMois+catBat, data=prop))

    temp <- aggregate(zPue$pds_vif, zPue[, c('opano', 'annee')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('opano', 'annee')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('opano', 'annee'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ annee + opano, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ annee + opano, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~annee+opano, data=prop))

    temp <- aggregate(zPue$pds_vif, zPue[, c('opano', 'annee')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('opano', 'annee')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('opano', 'annee'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ annee + opano, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ annee + opano, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~annee+opano, data=prop))
    ## test <- aggregate(zPue$pds_vif, zPue['opano'], FUN=sum); test$x <- test$x/sum(test$x)*100; test[order(test$x),]
    ## prop$propTot <- prop$tot / sum(prop$tot) * 1000
    ## prop$propPue <- prop$pue / sum(prop$tot) * 1000
    ## prop[order(prop$propPue),]
    ## prop[order(prop$propTot),]

    temp <- aggregate(
      zPue$pds_vif,
      zPue[, c('regionZiff', 'annee')],
      FUN = sum
    )
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('regionZiff', 'annee')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('regionZiff', 'annee'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ annee + regionZiff, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ annee + regionZiff, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~annee+regionZiff, data=prop))

    temp <- aggregate(zPue$pds_vif, zPue[, c('debMois', 'opano')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('debMois', 'opano')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('debMois', 'opano'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ debMois + opano, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ debMois + opano, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~debMois+opano, data=prop))

    temp <- aggregate(zPue$pds_vif, zPue[, c('debMois', 'opano')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('debMois', 'opano')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('debMois', 'opano'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ debMois + opano, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ debMois + opano, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~debMois+opano, data=prop))

    temp <- aggregate(
      zPue$pds_vif,
      zPue[, c('debMois', 'regionZiff')],
      FUN = sum
    )
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('debMois', 'regionZiff')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('debMois', 'regionZiff'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ debMois + regionZiff, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ debMois + regionZiff, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~debMois+regionZiff, data=prop))

    temp <- aggregate(zPue$pds_vif, zPue[, c('catBat', 'opano')], FUN = sum)
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('catBat', 'opano')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('catBat', 'opano'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ catBat + opano, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ catBat + opano, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~catBat+opano, data=prop))

    temp <- aggregate(
      zPue$pds_vif,
      zPue[, c('catBat', 'regionZiff')],
      FUN = sum
    )
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('catBat', 'regionZiff')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('catBat', 'regionZiff'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ catBat + regionZiff, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ catBat + regionZiff, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~catBat+regionZiff, data=prop))

    ## temp <- aggregate(zPue$pds_vif, zPue[,c('opano','opano')], FUN=sum); names(temp) <- c(head(names(temp),-1), 'pue')
    ## temp2 <- aggregate(ziff.init$pds_vif, ziff.init[,c('opano','opano')], FUN=sum); names(temp2) <- c(head(names(temp2),-1), 'tot')
    ## prop <- merge(temp, temp2, by=c('opano','opano'), all=TRUE); prop[is.na(prop$pue),'pue'] <- 0
    ## prop$prop <- prop$pue / prop$tot
    ## par(mfrow=c(1,2))
    ## mosaicplot(xtabs(pue~opano+opano, data=prop), color=rainbow(12))
    ## mosaicplot(xtabs(tot~opano+opano, data=prop), color=rainbow(12))
    ## ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~opano+opano, data=prop))

    temp <- aggregate(
      zPue$pds_vif,
      zPue[, c('opano', 'regionZiff')],
      FUN = sum
    )
    names(temp) <- c(head(names(temp), -1), 'pue')
    temp2 <- aggregate(
      ziff.init$pds_vif,
      ziff.init[, c('opano', 'regionZiff')],
      FUN = sum
    )
    names(temp2) <- c(head(names(temp2), -1), 'tot')
    prop <- merge(temp, temp2, by = c('opano', 'regionZiff'), all = TRUE)
    prop[is.na(prop$pue), 'pue'] <- 0
    prop$prop <- prop$pue / prop$tot
    par(mfrow = c(1, 2))
    mosaicplot(
      xtabs(pue ~ opano + regionZiff, data = prop),
      color = rainbow(12),
      main = 'utilisable'
    )
    mosaicplot(
      xtabs(tot ~ opano + regionZiff, data = prop),
      color = rainbow(12),
      main = 'total'
    )
    ##
    ## par(mfrow=c(1,1))
    ## heatmap(xtabs(prop~opano+regionZiff, data=prop))

    ##         temp <- aggregate(zPue$pds_vif, zPue[,c('opano','regionZiff')], FUN=sum); names(temp) <- c(head(names(temp),-1), 'pue')
    ##         temp2 <- aggregate(ziff.init$pds_vif, ziff.init[,c('opano','regionZiff')], FUN=sum); names(temp2) <- c(head(names(temp2),-1), 'tot')
    ##         prop <- merge(temp, temp2, by=c('opano','regionZiff'), all=TRUE); prop[is.na(prop$pue),'pue'] <- 0
    ##         prop$prop <- prop$pue / prop$tot
    ## par(mfrow=c(1,1))
    ##         mosaicplot(xtabs(pue~opano+regionZiff, data=prop), color=rainbow(12))
    ##         ##
    ## par(mfrow=c(1,1))
    ##         heatmap(xtabs(prop~opano+regionZiff, data=prop))
  }
  if (FALSE) {
    ## exemples de modélisation

    zPue$pue <- zPue$pds_vif / (zPue$nb_engin / 1000)
    ## sort(zPue$pue, decreasing=TRUE)
    zPue[zPue$pue < 1, 'pue'] <- 1
    zPue$logPue <- log(zPue$pue)
    ## pairs(pue~factor(annee)+factor(debMois)+factor(catBat)+factor(opano), data=zPue)
    ## pairs(logPue~factor(annee)+factor(debMois)+factor(catBat)+factor(opano), data=zPue)
    hist(
      zPue$pue,
      breaks = seq(-0.005, 200, by = 0.01) * 1000,
      xlim = c(0, max(zPue$pue))
    )
    hist(
      zPue$pue,
      breaks = seq(-0.005, 200, by = 0.01) * 1000,
      xlim = c(-0.1, 2) * 1000
    )
    hist(
      log(zPue$pue),
      breaks = log(seq(0.00005, 200, by = 0.01) * 1000),
      xlim = c(0, log(max(zPue$pue)))
    )
    hist(zPue$logPue, breaks = seq(0, 13, by = 0.1), prob = TRUE)
    library(MASS)
    fitNorm <- fitdistr(zPue$logPue, 'normal')
    curve(
      dnorm(x, fitNorm$estimate[1], fitNorm$estimate[2]),
      col = 2,
      add = TRUE
    )
    table(zPue$opano)

    ## ======
    ## opano (2003+)

    zPue2 <- subset(zPue, annee >= 2003)
    fit2 <- lm(
      logPue ~ factor(annee) + factor(debMois) + factor(catBat) + factor(opano),
      data = zPue2
    )
    summary(fit2)
    ##
    dev.new()
    par(mfrow = c(2, 2))
    plot(fit2)
    ## zPue2[c('6844','15661','4181'),]
    ##
    par(mfrow = c(2, 2))
    termplot(fit2, zPue2, 'annee', se = TRUE)
    ##
    ## pred <- predict(fit2, type='response')
    ## plot(fit2$model[,1], pred, cex=0.8, xlab='observé', ylab='predit')
    ## abline(a=0,b=1,col=2)
    ##
    ##
    yr.ref <- 2025
    mod.variab <- c("annee", "catBat", "debMois", "opano")
    pred.grid <- zPue2[zPue2$annee == yr.ref, mod.variab]
    tmp <- rep(
      unique(zPue2$annee),
      rep(dim(pred.grid)[1], length(unique(zPue2$annee)))
    )
    for (i in 1:(length(unique(zPue2$annee)) - 1)) {
      pred.grid <- rbind(pred.grid, zPue2[zPue2$annee == yr.ref, mod.variab])
    }
    ##
    pred.grid$annee <- tmp
    pred <- predict(fit2, pred.grid, type = "response", se = TRUE)
    ##
    dev.new()
    par(mfrow = c(1, 1))
    obs.yr <- aggregate(fit2$model[, 1], list(zPue2$annee), mean)
    pred.yr <- aggregate(pred$fit, list(pred.grid$annee), mean)
    pred.yr.se.up <- aggregate(
      pred$fit + 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    pred.yr.se.lo <- aggregate(
      pred$fit - 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    plot(
      obs.yr[, 1],
      exp(obs.yr$x),
      type = "p", #ylim=c(min(pred.yr.se.lo$x),exp(max(pred.yr.se.up$x))),
      ylim = c(0, exp(max(pred.yr.se.up$x))),
      xlab = "Année",
      ylab = "PUE moyenne",
      main = 'opano 2003'
    )
    abline(h = 0, col = 'grey70')
    lines(pred.yr[, 1], exp(pred.yr$x), lty = 1)
    lines(pred.yr.se.up[, 1], exp(pred.yr.se.up$x), lty = 2)
    lines(pred.yr.se.lo[, 1], exp(pred.yr.se.lo)$x, lty = 2)

    ## ======
    ## opano (2003+)
    ## ssZéro
    ## préféré pour l'instant

    zPue2 <- subset(zPue, annee >= 2003 & pue > 1)
    fit2 <- lm(
      logPue ~ factor(annee) + factor(debMois) + factor(catBat) + factor(opano),
      data = zPue2
    )
    ##
    dev.new()
    par(mfrow = c(2, 2))
    plot(fit2)
    ## zPue2[c('6858','4015','4195'),]
    ##
    par(mfrow = c(2, 2))
    termplot(fit2, zPue2, 'annee', se = TRUE)
    ##
    pred <- predict(fit2, type = 'response')
    ## plot(fit2$model[,1], pred, cex=0.8, xlab='observé', ylab='predit')
    ## abline(a=0,b=1, col=2)
    ##
    ##
    yr.ref <- 2020
    mod.variab <- c("annee", "catBat", "debMois", "opano")
    pred.grid <- zPue2[zPue2$annee == yr.ref, mod.variab]
    tmp <- rep(
      unique(zPue2$annee),
      rep(dim(pred.grid)[1], length(unique(zPue2$annee)))
    )
    for (i in 1:(length(unique(zPue2$annee)) - 1)) {
      pred.grid <- rbind(pred.grid, zPue2[zPue2$annee == yr.ref, mod.variab])
    }
    ##
    pred.grid$annee <- tmp
    pred <- predict(fit2, pred.grid, type = "response", se = TRUE)
    ##
    ## dev.new()
    png(
      file = file.path(dirOutput, 'fr', 'pueComm.png'),
      height = 6,
      width = 8,
      units = 'in',
      res = 300
    )
    par(mfrow = c(1, 1))
    obs.yr <- aggregate(fit2$model[, 1], list(zPue2$annee), mean)
    pred.yr <- aggregate(pred$fit, list(pred.grid$annee), mean)
    pred.yr.se.up <- aggregate(
      pred$fit + 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    pred.yr.se.lo <- aggregate(
      pred$fit - 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    plot(
      obs.yr[, 1],
      exp(obs.yr$x),
      type = "p", #ylim=c(min(pred.yr.se.lo$x),exp(max(pred.yr.se.up$x))),
      ylim = c(0, exp(max(c(obs.yr$x, pred.yr.se.up$x)))),
      ## xlab="Année", ylab="PUE moyenne", main='opano 2003 ssZéro'); abline(h=0, col='grey70')
      xlab = "Année",
      ylab = "PUE moyenne",
      main = 'PUE commerciale'
    )
    abline(h = 0, col = 'grey70')
    lines(pred.yr[, 1], exp(pred.yr$x), lty = 1)
    lines(pred.yr.se.up[, 1], exp(pred.yr.se.up$x), lty = 2)
    lines(pred.yr.se.lo[, 1], exp(pred.yr.se.lo)$x, lty = 2)
    temp <- cbind(
      annee = pred.yr[, 1],
      moy = exp(pred.yr$x),
      se.up = exp(pred.yr.se.up$x),
      se.lo = exp(pred.yr.se.lo)$x
    )
    dev.off()
    write.csv2(temp, file = file.path(dirOutput, 'pueComm.csv'))

    ## ======
    ## opano (4ST)
    ##

    zPue2 <- subset(zPue, opano %in% c('4S', '4T') & annee >= 2003)
    fit2 <- lm(
      logPue ~ factor(annee) + factor(debMois) + factor(catBat) + factor(opano),
      data = zPue2
    )
    summary(fit2)
    ##
    dev.new()
    par(mfrow = c(2, 2))
    plot(fit2)
    ## zPue2[c('6844','15661','4181'),]
    ##
    par(mfrow = c(2, 2))
    termplot(fit2, zPue2, 'annee', se = TRUE)
    ##
    ## pred <- predict(fit2, type='response')
    ## plot(fit2$model[,1], pred, cex=0.8, xlab='observé', ylab='predit')
    ## abline(a=0,b=1,col=2)
    ##
    ##
    yr.ref <- 2022
    mod.variab <- c("annee", "catBat", "debMois", "opano")
    pred.grid <- zPue2[zPue2$annee == yr.ref, mod.variab]
    tmp <- rep(
      unique(zPue2$annee),
      rep(dim(pred.grid)[1], length(unique(zPue2$annee)))
    )
    for (i in 1:(length(unique(zPue2$annee)) - 1)) {
      pred.grid <- rbind(pred.grid, zPue2[zPue2$annee == yr.ref, mod.variab])
    }
    ##
    pred.grid$annee <- tmp
    pred <- predict(fit2, pred.grid, type = "response", se = TRUE)
    ##
    dev.new()
    par(mfrow = c(1, 1))
    obs.yr <- aggregate(fit2$model[, 1], list(zPue2$annee), mean)
    pred.yr <- aggregate(pred$fit, list(pred.grid$annee), mean)
    pred.yr.se.up <- aggregate(
      pred$fit + 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    pred.yr.se.lo <- aggregate(
      pred$fit - 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    plot(
      obs.yr[, 1],
      exp(obs.yr$x),
      type = "p", #ylim=c(min(pred.yr.se.lo$x),exp(max(pred.yr.se.up$x))),
      ylim = c(0, exp(max(pred.yr.se.up$x))),
      xlab = "Année",
      ylab = "PUE moyenne",
      main = 'opano 4ST 2003'
    )
    abline(h = 0, col = 'grey70')
    lines(pred.yr[, 1], exp(pred.yr$x), lty = 1)
    lines(pred.yr.se.up[, 1], exp(pred.yr.se.up$x), lty = 2)
    lines(pred.yr.se.lo[, 1], exp(pred.yr.se.lo)$x, lty = 2)

    ## ======
    ## opano

    zPue2 <- subset(
      zPue,
      !opano %in% c('4T', '4TI', '4S') & annee >= 2003 & pue > 1
    )
    fit2 <- lm(
      logPue ~ factor(annee) +
        factor(debMois) +
        factor(catBat) +
        factor(opano),
      data = zPue2
    )
    summary(fit2)
    ##
    dev.new()
    par(mfrow = c(2, 2))
    plot(fit2)
    ## zPue2[c('6844','15661','4181'),]
    ##
    par(mfrow = c(2, 3))
    termplot(fit2, zPue2, 'annee', se = TRUE)
    ##
    ## pred <- predict(fit2, type='response')
    ## plot(fit2$model[,1], pred, cex=0.8, xlab='observé', ylab='predit')
    ## abline(a=0,b=1,col=2)
    ##
    ##
    yr.ref <- 2020
    mod.variab <- c("annee", "catBat", "debMois", "opano")
    pred.grid <- zPue2[zPue2$annee == yr.ref, mod.variab]
    tmp <- rep(
      unique(zPue2$annee),
      rep(dim(pred.grid)[1], length(unique(zPue2$annee)))
    )
    for (i in 1:(length(unique(zPue2$annee)) - 1)) {
      pred.grid <- rbind(pred.grid, zPue2[zPue2$annee == yr.ref, mod.variab])
    }
    ##
    pred.grid$annee <- tmp
    pred <- predict(fit2, pred.grid, type = "response", se = TRUE)
    ##
    dev.new()
    par(mfrow = c(1, 1))
    obs.yr <- aggregate(fit2$model[, 1], list(zPue2$annee), mean)
    pred.yr <- aggregate(pred$fit, list(pred.grid$annee), mean)
    pred.yr.se.up <- aggregate(
      pred$fit + 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    pred.yr.se.lo <- aggregate(
      pred$fit - 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    plot(
      obs.yr[, 1],
      exp(obs.yr$x),
      type = "p", #ylim=c(min(pred.yr.se.lo$x),exp(max(pred.yr.se.up$x))),
      ylim = c(0, exp(max(c(pred.yr.se.up$x, obs.yr$x)))),
      xlab = "Année",
      ylab = "PUE moyenne",
      main = 'opano 2003 ssZéro'
    )
    abline(h = 0, col = 'grey70')
    lines(pred.yr[, 1], exp(pred.yr$x), lty = 1)
    lines(pred.yr.se.up[, 1], exp(pred.yr.se.up$x), lty = 2)
    lines(pred.yr.se.lo[, 1], exp(pred.yr.se.lo)$x, lty = 2)

    ## ======
    ## opano (2003+)
    ## ssZéro, 3ansOuPlus
    ##
    ## ajoute pas grand chose

    test <- table(zPue$codeNBPC, zPue$annee, useNA = 'ifany')
    nbpc.3anOuPlus <- names(which(
      apply(test, 1, function(x) {
        sum(x > 0)
      }) >
        2
    ))
    ## sum(subset(zPue, codeNBPC%in%nbpc.3anOuPlus)$pds_vif) / sum(zPue$pds_vif)
    ## test <- subset(zPue, !codeNBPC%in%nbpc.3anOuPlus)
    zPue2 <- subset(
      zPue,
      annee >= 2003 & pue > 1 & codeNBPC %in% nbpc.3anOuPlus
    )
    fit2 <- lm(
      logPue ~ factor(annee) + factor(debMois) + factor(catBat) + factor(opano),
      data = zPue2
    )
    ##
    dev.new()
    par(mfrow = c(2, 2))
    plot(fit2)
    ## zPue2[c('1080','5185','14073'),]
    ##
    par(mfrow = c(2, 2))
    termplot(fit2, zPue2, 'annee', se = TRUE)
    ##
    pred <- predict(fit2, type = 'response')
    ## plot(fit2$model[,1], pred, cex=0.8, xlab='observé', ylab='predit')
    ## abline(a=0,b=1, col=2)
    ##
    ##
    yr.ref <- 2020
    mod.variab <- c("annee", "catBat", "debMois", "opano")
    pred.grid <- zPue2[zPue2$annee == yr.ref, mod.variab]
    tmp <- rep(
      unique(zPue2$annee),
      rep(dim(pred.grid)[1], length(unique(zPue2$annee)))
    )
    for (i in 1:(length(unique(zPue2$annee)) - 1)) {
      pred.grid <- rbind(pred.grid, zPue2[zPue2$annee == yr.ref, mod.variab])
    }
    ##
    pred.grid$annee <- tmp
    pred <- predict(fit2, pred.grid, type = "response", se = TRUE)
    ##
    dev.new()
    par(mfrow = c(1, 1))
    obs.yr <- aggregate(fit2$model[, 1], list(zPue2$annee), mean)
    pred.yr <- aggregate(pred$fit, list(pred.grid$annee), mean)
    pred.yr.se.up <- aggregate(
      pred$fit + 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    pred.yr.se.lo <- aggregate(
      pred$fit - 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    plot(
      obs.yr[, 1],
      exp(obs.yr$x),
      type = "p", #ylim=c(min(pred.yr.se.lo$x),exp(max(pred.yr.se.up$x))),
      ylim = c(0, exp(max(pred.yr.se.up$x))),
      xlab = "Année",
      ylab = "PUE moyenne",
      main = 'opano 2003 ssZéro 3ansOuPlus'
    )
    abline(h = 0, col = 'grey70')
    lines(pred.yr[, 1], exp(pred.yr$x), lty = 1)
    lines(pred.yr.se.up[, 1], exp(pred.yr.se.up$x), lty = 2)
    lines(pred.yr.se.lo[, 1], exp(pred.yr.se.lo)$x, lty = 2)

    ## ======
    ## opano (2003+)
    ## 3ansOuPlus
    ##

    test <- table(zPue$codeNBPC, zPue$annee, useNA = 'ifany')
    nbpc.3anOuPlus <- names(which(
      apply(test, 1, function(x) {
        sum(x > 0)
      }) >
        2
    ))
    ## sum(subset(zPue, codeNBPC%in%nbpc.3anOuPlus)$pds_vif) / sum(zPue$pds_vif)
    ## test <- subset(zPue, !codeNBPC%in%nbpc.3anOuPlus)
    zPue2 <- subset(zPue, annee >= 2003 & codeNBPC %in% nbpc.3anOuPlus)
    fit2 <- lm(
      logPue ~ factor(annee) + factor(debMois) + factor(catBat) + factor(opano),
      data = zPue2
    )
    ##
    dev.new()
    par(mfrow = c(2, 2))
    plot(fit2)
    ## zPue2[c('1080','5185','14073'),]
    ##
    par(mfrow = c(2, 2))
    termplot(fit2, zPue2, 'annee', se = TRUE)
    ##
    pred <- predict(fit2, type = 'response')
    ## plot(fit2$model[,1], pred, cex=0.8, xlab='observé', ylab='predit')
    ## abline(a=0,b=1, col=2)
    ##
    ##
    yr.ref <- 2025
    mod.variab <- c("annee", "catBat", "debMois", "opano")
    pred.grid <- zPue2[zPue2$annee == yr.ref, mod.variab]
    tmp <- rep(
      unique(zPue2$annee),
      rep(dim(pred.grid)[1], length(unique(zPue2$annee)))
    )
    for (i in 1:(length(unique(zPue2$annee)) - 1)) {
      pred.grid <- rbind(pred.grid, zPue2[zPue2$annee == yr.ref, mod.variab])
    }
    ##
    pred.grid$annee <- tmp
    pred <- predict(fit2, pred.grid, type = "response", se = TRUE)
    ##
    dev.new()
    par(mfrow = c(1, 1))
    obs.yr <- aggregate(fit2$model[, 1], list(zPue2$annee), mean)
    pred.yr <- aggregate(pred$fit, list(pred.grid$annee), mean)
    pred.yr.se.up <- aggregate(
      pred$fit + 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    pred.yr.se.lo <- aggregate(
      pred$fit - 1.96 * pred$se.fit,
      list(pred.grid$annee),
      mean
    )
    plot(
      obs.yr[, 1],
      exp(obs.yr$x),
      type = "p", #ylim=c(min(pred.yr.se.lo$x),exp(max(pred.yr.se.up$x))),
      ylim = c(0, exp(max(pred.yr.se.up$x))),
      xlab = "Année",
      ylab = "PUE moyenne",
      main = 'opano 2003 ssZéro 3ansOuPlus'
    )
    abline(h = 0, col = 'grey70')
    lines(pred.yr[, 1], exp(pred.yr$x), lty = 1)
    lines(pred.yr.se.up[, 1], exp(pred.yr.se.up$x), lty = 2)
    lines(pred.yr.se.lo[, 1], exp(pred.yr.se.lo)$x, lty = 2)
  }
  save(zPue, file = file.path(dirOutput, 'zPue.RData'))
  return(zPue)
}
