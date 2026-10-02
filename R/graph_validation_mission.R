#' Évalue les données de la dernière mission scientifique du NGSL pour trouver des erreurs potentielles et
#' corriger lorsque possible. Produit plusieurs graphiques, il est utile d'ouvrir un .pdf avant d'appeler
#' la fonction.
#'
#' Deux éléments à considérer pour le flétan:
#'     1- que les poids individuels sont cohérents avec les longueurs individuelles
#'     2- que les poids d'échantillon sont cohérents avec les poids/longueurs observés
#'     3- que les nombre et poids d'échantillon sont cohérents avec les poids catchurés
#'
#' La fonction utilisée pour la relation entre la longueur et le poids est exp(-12.212072)*(longueur)^3.180181
#'
#' @param prefixe est le nom du relevé à valider dans la base de données
#' @param dirInput chemin local où se trouvent la lecture des relevés
#' @param dirOutput chemin local où enregistrer les objets (graphiques, tableur) créés
#'
#' @return une liste des indicateurs utilisés
#'
graph_validation_mission <- function(donnee) {
  donnee$carbio$longueur_fourche_frais <- donnee$carbio$longueur_fourche_frais /
    10
  donnee$carbio$poids_total_frais <- donnee$carbio$poids_total_frais / 1000
  ## loader les données relevé MPO NGSL
  # print(paste(
  #   'MPO NGSL:',
  #   file.info(file.path(dirInput, 'lectureReleveNgsl_NGSL-893.RData'))$mtime
  # ))
  # load(file.path(dirInput, 'lectureReleveNgsl_NGSL-893.RData'))
  # data.validation$mpo$ngsl <- obj #liste: trait.rn, catch.rn, catch.trait.rn, carbio.trait.rn, stratum.rn
  ##
  # donnee <- obj
  ##
  ## validation 1, est-ce que l2m(carbio$longueur_fourche_frais) = carbio$poids_total_frais ------ semble ok, meme si certains, mettons, surprenants!
  donnee$carbio$poidsTotEstime <- exp(-12.212072) *
    (donnee$carbio$longueur_fourche_frais)^3.180181
  donnee$carbio$longEstime <- (donnee$carbio$poids_total_frais /
    exp(-12.212072))^(1 / 3.180181)
  # (11.94 /
  #     exp(-12.212072))^(1 / 3.180181)
  # par(mfrow = c(1, 2))
  ##
  ## comparaison du poids estimé par rapport au poids attendu,
  ##
  ## plot(donnee$carbio$poids_total_frais, donnee$carbio$poidsTotEstime, main=paste('Flétan', prefixe), xlab='Poids observé', ylab='Poids estimé'); abline(a=0,b=1)
  ## abline(a=0,b=0.75, col=2, lty=2); abline(a=0,b=1.25, col=2, lty=2)
  ## temp <- donnee$carbio$poids_total_frais/(donnee$carbio$poidsTotEstime)
  ## lesquels <- which(temp<0.75 | temp>1.250)
  ## points(donnee$carbio[lesquels,'poids_total_frais'], donnee$carbio[lesquels,'poidsTotEstime'], pch=21,
  ##        bg=c('red',rep(NA,3),'green')[donnee$carbio[lesquels,'sexe']])
  ## legend('bottomright', inset=0.03,
  ##        legend=c(paste('Données',prefixe),
  ##                 '-25% et +25%'),
  ##        pch=c(1,NA), lty=c(NA,2), col=c('black','red'))
  ##
  plot(
    donnee$carbio$longueur_fourche_frais,
    donnee$carbio$poids_total_frais /
      donnee$carbio$poidsTotEstime,
    main = paste(
      'Flétan',
      ', n =',
      nrow(donnee$carbio)
    ),
    xlab = 'longueur_fourche_frais',
    ylab = 'Poids observé/estimé'
  )
  abline(h = 1)
  abline(h = 1.25, col = 2, lty = 2)
  abline(h = 0.75, col = 2, lty = 2)
  temp <- donnee$carbio$poids_total_frais /
    (donnee$carbio$poidsTotEstime)
  lesquels <- which(temp < 0.75 | temp > 1.250)
  points(
    donnee$carbio[lesquels, 'longueur_fourche_frais'],
    donnee$carbio[lesquels, 'poids_total_frais'] /
      donnee$carbio[lesquels, 'poidsTotEstime'],
    pch = 21,
    bg = c('red', rep(NA, 3), 'green')[donnee$carbio[
      lesquels,
      'cod_sexe'
    ]]
  )
  text(
    donnee$carbio[lesquels, 'longueur_fourche_frais'],
    donnee$carbio[lesquels, 'poids_total_frais'] /
      donnee$carbio[lesquels, 'poidsTotEstime'],
    donnee$carbio[lesquels, 'no_station'],
    pos = 3,
    cex = 0.5
  )
  legend(
    'topright',
    inset = 0.03,
    legend = paste('Données', '-25% et +25%'),
    pch = c(1, NA),
    lty = c(NA, 2),
    col = c('black', 'red')
  )
  ##
  donnee$carbio[
    lesquels,
    c(
      'poids_total_frais',
      'poidsTotEstime',
      'longueur_fourche_frais',
      'longEstime',
      'no_poisson',
      'no_station'
    )
  ]
  subset(donnee$catch, no_station %in% donnee$carbio[lesquels, 'no_station'])
  subset(donnee$carbio, no_station %in% donnee$carbio[lesquels, 'no_station'])
  ## longueur_fourche_frais-poids

  ## comparaison du poids estimé par rapport au poids attendu, plus petit que 70cm
  plot(
    donnee$carbio$poids_total_frais,
    donnee$carbio$poidsTotEstime,
    main = paste0('Flétan <70cm'),
    xlab = 'Poids observé',
    ylab = 'Poids estimé',
    xlim = c(0, exp(-12.212072) * (70)^3.180181),
    ylim = c(0, exp(-12.212072) * (70)^3.180181)
  )
  abline(a = 0, b = 1)
  abline(a = 0, b = 0.75, col = 2, lty = 2)
  abline(a = 0, b = 1.25, col = 2, lty = 2)
  temp <- donnee$carbio$poidsTot /
    (donnee$carbio$poidsTotEstime)
  lesquels <- which(temp < 0.75 | temp > 1.250)
  points(
    donnee$carbio[lesquels, 'poidsTot'],
    donnee$carbio[lesquels, 'poidsTotEstime'],
    pch = 21,
    bg = c('red', rep(NA, 3), 'green')[donnee$carbio[
      lesquels,
      'sexe'
    ]]
  )
  # text(
  #   donnee$carbio[lesquels, 'poidsTot'],
  #   donnee$carbio[lesquels, 'poidsTotEstime'],
  #   donnee$carbio[lesquels, 'no_station'],
  #   pos = 3,
  #   cex = 0.5
  # )
  legend(
    'bottomright',
    inset = 0.03,
    legend = c('Données', '-25% et +25%'),
    pch = c(1, NA),
    lty = c(NA, 2),
    col = c('black', 'red')
  )
  ## donnee$carbio[lesquels,c('poidsTot','poidsTotEstime','longueur','longEstime','noSpecimen','no_station','noMission','source')]
  ## longueur-poids
  ##
  ## longueur-poids en log-log

  ##
  ## 2) que sum(carbio$poidsTot) = catch$pds_echant_categ ------------- reste un peu incertain, mais les données dans "catch" semblent ok
  poidsParStation <- aggregate(
    donnee$carbio$poids_total_frais,
    donnee$carbio[c('no_station')],
    FUN = sum,
    na.rm = TRUE
  )
  poidsParStationEstime <- aggregate(
    donnee$carbio$poidsTotEstime,
    donnee$carbio[c('no_station')],
    FUN = sum,
    na.rm = TRUE
  )
  temp <- merge(
    poidsParStation,
    donnee$catch[, c(
      'no_station',
      'pds_echant_categ'
    )],
    all = TRUE
  )
  temp$poidscarbioEch <- temp$x
  temp <- merge(
    poidsParStationEstime,
    temp[, c('no_station', 'pds_echant_categ', 'poidscarbioEch')],
    all = TRUE
  )
  temp$poidscarbioEchEstime <- temp$x
  plot(
    temp[, c('pds_echant_categ', 'poidscarbioEch')],
    col = 4,
    xlab = 'Poids échantillon "catch"',
    ylab = 'Poids échantillon "carbio"'
  )
  abline(a = 0, b = 1)
  abline(a = 0, b = 0.75, col = 2, lty = 2)
  abline(a = 0, b = 1.25, col = 2, lty = 2)
  temp2 <- temp$poidscarbioEch / temp$pds_echant_categ
  lesquels <- which(temp2 < 0.75 | temp2 > 1.25)
  points(temp[, c('pds_echant_categ', 'poidscarbioEchEstime')], col = 3)
  points(temp[, c('pds_echant_categ', 'poidscarbioEch')], pch = 21, bg = 4)
  points(
    temp[lesquels, c('pds_echant_categ', 'poidscarbioEch')],
    pch = 21,
    bg = 4
  )
  points(
    temp[lesquels, c('pds_echant_categ', 'poidscarbioEchEstime')],
    pch = 21,
    bg = 3
  )
  for (i in 1:nrow(temp)) {
    lines(
      rep(temp[i, 'pds_echant_categ'], 2),
      temp[i, c('poidscarbioEch', 'poidscarbioEchEstime')],
      col = 4
    )
  }
  legend(
    'topleft',
    inset = 0.03,
    legend = c('somme carbio$poidsTot', 'somme carbio$poidsTotEstime'),
    col = 4:3,
    pch = 1
  )
  subset(donnee$catch, no_station %in% donnee$carbio[lesquels, 'no_station'])
  subset(donnee$carbio, no_station %in% donnee$carbio[lesquels, 'no_station'])

  ## temp[lesquels,]
  ##
  ##
  ##
  ## 3) que length(carbio$longueur_fourche_frais) = catch$nb_echant_categ
  nbParStation <- aggregate(
    donnee$carbio$longueur_fourche_frais,
    donnee$carbio[c('no_station')],
    FUN = length
  )
  ##
  ##
  ## 4) que catch$nbcatch >= catch$nb_echant_categ
  # plot(
  #   donnee$catch[, c('nb_ind', 'nb_echant_categ')],
  #   main = 'catch',
  #   xlab = 'Nombre catchuré',
  #   ylab = 'Nombre échantillonné'
  # )
  # abline(a = 0, b = 1)
  # lesquels <- which(
  #   apply(donnee$catch[, c('nbcatch', 'nb_echant_categ')], 1, diff) != 0 |
  #     is.na(apply(
  #       donnee$catch[, c('nbcatch', 'nb_echant_categ')],
  #       1,
  #       diff
  #     ))
  # )
  ## donnee$catch[lesquels,]
  ##
  x <- hist(
    donnee$carbio$longueur_fourche_frais,
    breaks = seq(2.5, 200, by = 5),
    main = 'Structure de taille',
    xlab = 'Longueur'
  )
  text(x$mids, x$count, labels = x$count, pos = 3, cex = 0.8)
  abline(v = 85, col = 4)
  legend(
    'topright',
    legend = paste(
      c('<85', '>85'),
      ', n = ',
      c(
        sum(donnee$carbio$longueur_fourche_frais < 84.99),
        sum(donnee$carbio$longueur_fourche_frais >= 84.99)
      )
    )
  )
  ##
  plot(
    sort(donnee$carbio$longueur_fourche_frais),
    seq(0, 1, length.out = length(donnee$carbio$longueur_fourche_frais)),
    type = 's',
    xlab = 'Longueur',
    ylab = 'Proportion'
  )
  ##
  # plot(
  #   donnee$carbio.trait$longueur_fourche_frais,
  #   -donnee$carbio.trait$profMoy,
  #   ylim = c(-650, 0),
  #   xlab = 'Longueur',
  #   ylab = 'Profondeur'
  # )
  # abline(v = 85)
  ##
  # donnee$carbio.trait$longueur_fourche_frais_cl <- cut(
  #   donnee$carbio.trait$longueur_fourche_frais,
  #   breaks = seq(-5.01, 200, by = 10)
  # )
  # x <- boxplot(
  #   -profMoy ~ longueur_cl,
  #   data = donnee$carbio.trait,
  #   ylim = c(-650, 0)
  # )
  # axis(3, at = 1:length(x$n), labels = x$n, cex.axis = 0.7)
  ##
  # write.csv(
  #   donnee$carbio.trait,
  #   file = file.path(dirOutput, 'carbioValidation.csv')
  # )
  # write.csv(
  #   donnee$catch.trait,
  #   file = file.path(dirOutput, 'catchValidation.csv')
  # )
  return(donnee)
}
