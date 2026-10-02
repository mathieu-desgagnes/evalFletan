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
  for (i.an in rev(sort(unique(zp.init$annee)))) {
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
  for (i.an in rev(sort(unique(zp.init$annee)))) {
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
  for (i.an in rev(sort(unique(zp.init$annee)))) {
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
  for (i.an in rev(sort(unique(zp.init$annee)))) {
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

  pdf(
    file = file.path('dev', 'comparer_mer_mouil.pdf'),
    width = 14,
    height = 8.5
  )
  par(mfrow = c(2, 4))
  for (i.an in rev(sort(unique(zp.init$annee)))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      plot(
        temp2$jr_mer,
        temp2$jr_mouil,
        xlim = c(0, 10),
        ylim = c(0, 10),
        main = paste(i.an, i.reg, sep = ' : ')
      )
      abline(a = 0, b = 1)
    }
  }
  dev.off()

  pdf(
    file = file.path('dev', 'comparer_peche_mouil.pdf'),
    width = 17,
    height = 8.5
  )
  par(mfrow = c(2, 4))
  for (i.an in rev(sort(unique(zp.init$annee)))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      plot(
        temp2$jr_peche,
        temp2$jr_mouil,
        xlim = c(0, 10),
        ylim = c(0, 10),
        main = paste(i.an, i.reg, sep = ' : ')
      )
      abline(a = 0, b = 1)
    }
  }
  dev.off()

  pdf(
    file = file.path('dev', 'comparer_heure_peche.pdf'),
    width = 16,
    height = 8.5
  )
  par(mfrow = c(2, 4))
  for (i.an in rev(sort(unique(zp.init$annee)))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      plot(
        temp2$eff_hre,
        temp2$jr_peche,
        xlim = c(0, 200),
        ylim = c(0, 10),
        main = paste(i.an, i.reg, sep = ' : ')
      )
      abline(a = 0, b = 1 / 24)
    }
  }
  dev.off()

  pdf(
    file = file.path('dev', 'comparer_heure_mouil.pdf'),
    width = 16,
    height = 8.5
  )
  par(mfrow = c(2, 4))
  for (i.an in rev(sort(unique(zp.init$annee)))) {
    temp <- zp.init[zp.init$annee == i.an, ]
    for (i.reg in sort(unique(zp.init$region))) {
      temp2 <- temp[temp$region == i.reg, ]
      plot(
        temp2$eff_hre,
        temp2$jr_mouil,
        xlim = c(0, 200),
        ylim = c(0, 10),
        main = paste(i.an, i.reg, sep = ' : ')
      )
      abline(a = 0, b = 1 / 24)
    }
  }
  dev.off()

  ##
  ##étude sur les fractions de voyage
  ##
  par(mfrow = c(2, 2))
  for (i.reg in sort(unique(zp.init$region))) {
    temp <- zp.init[zp.init$region == i.reg, ]
    nb_voy <- aggregate(x = temp$no_voy, by = temp['annee'], FUN = length)
    plot(
      nb_voy,
      main = i.reg,
      xlim = c(1985, 2025),
      ylim = c(0, max(nb_voy[, 2]))
    )
    temp2 <- aggregate(
      x = temp$pd_deb,
      by = temp['annee'],
      FUN = sum,
      na.rm = TRUE
    )
    lines(temp2[, 1], temp2[, 2] / max(temp2[, 2]) * max(nb_voy[, 2]))
  }

  tail(
    aggregate(
      x = zp.init$frn_voy,
      by = zp.init[c('no_voy', 'region', 'annee')],
      FUN = sum,
      na.rm = TRUE
    ),
    100
  )
  tail(
    aggregate(
      x = zp.init$frn_voy,
      by = zp.init[c('nbpc', 'no_voy', 'region', 'annee')],
      FUN = sum,
      na.rm = TRUE
    ),
    100
  )
  subset(zp.init, nbpc == 108151 & no_voy == 44 & region == 'S' & annee == 2025)
  ##observation1: frn_voy est le rapport par operation de pêche des débarquements issus de ce voyage. Operation de peche non clairement identifié.
  ## dans l'exemple, lié au "cod_ach" et au "form_esp", où les poissons ronds sont '0' et poissons éviscérés sont '10114'
  subset(zp.init, nbpc == 176558 & no_voy == 69 & region == 'Q' & annee == 2025)
  ##observation2: frn_voy ne tient pas compte de 'tail_esp', qui sont finalement des sous-element d'un même operation
  subset(zp.init, nbpc == 106623 & no_voy == 11 & region == 'S' & annee == 2025)
  ##obs: ici, tail_esp et form_esp sont identiques, seul cod_ach est différent. Les jours en mer sont aussi séparés selon frn_voy, avec arrondissement
  subset(zp.init, nbpc == 176269 & no_voy == 50 & region == 'Q' & annee == 2025)

  temp1 <- rbind(
    subset(
      zp.init,
      nbpc == 8377 & no_voy == 28 & region == 'Q' & annee == 2010
    ),
    subset(
      zp.init,
      nbpc == 176558 & no_voy == 69 & region == 'Q' & annee == 2025
    ),
    subset(
      zp.init,
      nbpc == 106623 & no_voy == 11 & region == 'S' & annee == 2025
    )
  )
  write.csv(temp1, file = file.path('dev', 'exempleZiffIncertain.csv'))

  par(mfrow = c(2, 2))
  for (i.reg in sort(unique(zp.init$region))) {
    temp <- zp.init[zp.init$region == i.reg, ]
    duree_voy <- aggregate(
      x = temp$jr_mer,
      by = temp['annee'],
      FUN = median,
      na.rm = TRUE
    )
    plot(
      duree_voy,
      main = i.reg,
      xlim = c(1985, 2025),
      ylim = c(0, max(duree_voy[, 2]))
    )
    temp2 <- aggregate(
      x = temp$pd_deb,
      by = temp['annee'],
      FUN = sum,
      na.rm = TRUE
    )
    lines(temp2[, 1], temp2[, 2] / max(temp2[, 2]) * max(duree_voy[, 2]))
  }

  par(mfrow = c(2, 2))
  for (i.reg in sort(unique(zp.init$region))) {
    temp <- zp.init[zp.init$region == i.reg, ]
    duree_peche <- aggregate(
      x = temp$jr_peche,
      by = temp['annee'],
      FUN = median,
      na.rm = TRUE
    )
    plot(
      duree_peche,
      main = i.reg,
      xlim = c(1985, 2025),
      ylim = c(0, max(duree_peche[, 2]))
    )
    temp2 <- aggregate(
      x = temp$pd_deb,
      by = temp['annee'],
      FUN = sum,
      na.rm = TRUE
    )
    lines(temp2[, 1], temp2[, 2] / max(temp2[, 2]) * max(duree_peche[, 2]))
  }

  par(mfrow = c(2, 2))
  for (i.reg in sort(unique(zp.init$region))) {
    temp <- zp.init[zp.init$region == i.reg, ]
    duree_mouil <- aggregate(
      x = temp$jr_mouil,
      by = temp['annee'],
      FUN = median,
      na.rm = TRUE
    )
    plot(
      duree_mouil,
      main = i.reg,
      xlim = c(1985, 2025),
      ylim = c(0, max(duree_mouil[, 2]))
    )
    temp2 <- aggregate(
      x = temp$pd_deb,
      by = temp['annee'],
      FUN = sum,
      na.rm = TRUE
    )
    lines(temp2[, 1], temp2[, 2] / max(temp2[, 2]) * max(duree_mouil[, 2]))
  }

  par(mfrow = c(2, 2))
  for (i.reg in sort(unique(zp.init$region))) {
    temp <- zp.init[zp.init$region == i.reg, ]
    duree_eff <- aggregate(
      x = temp$eff_hre,
      by = temp['annee'],
      FUN = median,
      na.rm = TRUE
    )
    plot(
      duree_eff,
      main = i.reg,
      xlim = c(1985, 2025),
      ylim = c(0, max(duree_eff[, 2], na.rm = TRUE))
    )
    temp2 <- aggregate(
      x = temp$pd_deb,
      by = temp['annee'],
      FUN = sum,
      na.rm = TRUE
    )
    lines(
      temp2[, 1],
      temp2[, 2] / max(temp2[, 2]) * max(duree_eff[, 2], na.rm = TRUE)
    )
  }

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
