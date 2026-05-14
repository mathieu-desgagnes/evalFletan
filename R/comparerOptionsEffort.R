## donc vérifier:
## la distribution des trois efforts
## l,impact sur la comparaison poidsvif/effort
## relation avec frn_voy

comparerOptionsEffort <- function(ziff) {
  ziff.init <- subset(ziff, cod_esp == 130)
  nrow(ziff)
  nrow(ziff.init) #espèce capturée

  ziff.init$cat_bat[ziff.init$lht < 35] <- '<35'
  ziff.init$cat_bat[ziff.init$lht >= 35 & ziff.init$lht < 45] <- '35a45'
  ziff.init$cat_bat[ziff.init$lht >= 45 & ziff.init$lht < 65] <- '45a65'
  ziff.init$cat_bat[ziff.init$lht >= 65] <- '65+'

  sort(table(ziff.init$prespvis), decreasing = TRUE)
  # aggregate(
  #   x = ziff.init$pds_vif,
  #   by = ziff.init[, c('annee', 'prespvis')],
  #   FUN = sum,
  #   na.omit = TRUE
  # )

  zp.init <- subset(ziff.init, prespvis == 130)
  nrow(zp.init) #espèce visée

  zp.init <- subset(zp.init, engin == 51)
  nrow(zp.init) #engin = palangre

  ## nombre d'engins
  summary(zp.init$nb_engin)
  hist(zp.init$nb_engin, breaks = seq(0, 656600, by = 50))
  hist(zp.init$nb_engin, breaks = seq(0, 656600, by = 50), xlim = c(0, 10000))
  hist(zp.init$nb_engin, breaks = seq(-0.5, 656601, by = 1), xlim = c(0, 100))
  hist(
    zp.init[zp.init$annee >= 2003, 'nb_engin'],
    breaks = seq(0, 656600, by = 50),
    xlim = c(0, 10000)
  )

  nrow(zp.init)
  zp.init <- subset(zp.init, is.finite(nb_engin) & nb_engin >= 50)
  nrow(zp.init)

  ## donc vérifier:
  ## la distribution des trois efforts
  ## l,impact sur la comparaison poidsvif/effort
  ## relation avec frn_voy

  pdf(file = file.path('dev', 'troisEfforts.pdf'), width = 14, height = 8.5)
  par(mfrow = c(2, 4))
  for (i.an in sort(unique(zp.init$annee))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      hist(
        temp2$nb_engin,
        breaks = seq(-0.5, 656651, by = 50),
        xlim = c(0, 10000),
        main = paste(i.an, i.reg, sep = ' : ')
      )
    }
  }
  dev.off()

  pdf(file = file.path('dev', 'troisEfforts_mer.pdf'), width = 14, height = 8.5)
  par(mfrow = c(2, 4))
  for (i.an in sort(unique(zp.init$annee))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      hist(
        temp2$jr_mer,
        breaks = seq(-0.05, 1200, by = 0.2),
        xlim = c(0, 10),
        main = paste(i.an, i.reg, sep = ' : ')
      )
    }
  }
  dev.off()

  pdf(
    file = file.path('dev', 'troisEfforts_peche.pdf'),
    width = 14,
    height = 8.5
  )
  par(mfrow = c(2, 4))
  for (i.an in sort(unique(zp.init$annee))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      hist(
        temp2$jr_peche,
        breaks = seq(-0.05, 22, by = 0.2),
        xlim = c(0, 10),
        main = paste(i.an, i.reg, sep = ' : ')
      )
    }
  }
  dev.off()

  pdf(
    file = file.path('dev', 'troisEfforts_mouil.pdf'),
    width = 14,
    height = 8.5
  )
  par(mfrow = c(2, 4))
  for (i.an in sort(unique(zp.init$annee))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      hist(
        temp2$jr_mouil,
        breaks = seq(-0.05, 400, by = 0.2),
        xlim = c(0, 10),
        main = paste(i.an, i.reg, sep = ' : ')
      )
    }
  }
  dev.off()

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
