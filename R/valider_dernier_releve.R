#' Validation du dernier relevé.
#'
#' Cette fonction est utilisée pour retracer les porblèmes avec les données entrées en mer
#' sur les relevés au chalut du MPO. Cette étape est généralement complétée avant le transfert
#' des données sur les bases oracle.
#'
#' Cette version de la fonction utilise les fonctionnalité du package `pse_to_dataframe` pour
#' accéder aux données
#'
#' @import PSEtoDataframe
#'
#' @returns
#' @export
#'
#' @examples
valider_dernier_releve <- function() {
  conn <- PSEtoDataframe::connect_to_db(
    "PSE",
    uid = Sys.getenv("NOM_USAGER_BD"),
    pwd = Sys.getenv("MOT_DE_PASSE_BD")
  )
  PSEtoDataframe::source_info_list()
  no_source <- 38
  no_releve_dernier <- max(PSEtoDataframe::no_releve_list(no_source)$NO_RELEVE)
  data <- PSEtoDataframe::pse_to_dataframe(
    source_info = no_source,
    no_releve = no_releve_dernier,
    species = 893
  )

  pdf(
    file = file.path(paste0(
      'validation-',
      no_source,
      '-',
      no_releve_dernier,
      '.pdf'
    )),
    width = 14,
    height = 8.5
  )
  temp <- graph_validation_mission(
    donnee = data
  )
  dev.off()
}
