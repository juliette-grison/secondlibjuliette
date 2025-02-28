#' @title Valider un schéma
#'
#' @description Cette fonction vérifie si le dataframe (df) donné a les noms de colonnes attendus.
#'
#' @param df : Un data frame devant être validé.
#' @return Cette fonction ne retourne pas de valeur mais indique un message d'erreur si le schéma ne correspond pas.
#'
#' @seealso \code{\link{compter_nombre_d_adjoints}}, \code{\link{compter_nombre_d_elus}}, \code{\link{calcul_distribution_age}}, \code{\link{plot_code_professions}}, \code{\link{trouver_l_elu_le_plus_age}}
#'
#' @examples
#' df <- data.frame(Code.du.département = integer(),
#'                  Libellé.du.département = character(),
#'                  Code.de.la.commune = integer(),
#'                  Nom.de.l.élu = character())

validate_schema <- function(df) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier",
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune",
              "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu",
              "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle",
              "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat",
              "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité"
  )
  stopifnot(identical(colnames(df), schema))
}

#' @title Compter le nombre d'élus
#'
#' @description Cette fonction compte le nombre d'élus dans une liste.
#'
#' @param df : Un data frame contenant une colonne "Libellé.de.la.fonction" indiquant le rôle de chaque élu.
#' @return Un  nombre unique d’élus basés sur l’unicité du triplet nom/prénom/date de naissance.
#'
#' @seealso \code{\link{compter_nombre_d_adjoints}}, \code{\link{calcul_distribution_age}}, \code{\link{plot_code_professions}}, \code{\link{trouver_l_elu_le_plus_age}}
#'
#' @examples
#' df <- data.frame(Nom.de.l.élu = c("Dupont", "Martin", "Dupont"),
#'                  Prénom.de.l.élu = c("Jean", "Sophie", "Jean"),
#'                  Date.de.naissance = c("01/01/1980", "15/07/1975", "01/01/1980"))
#' @importFrom dplyr select distinct

compter_nombre_d_elus <- function(df) {
  validate_schema(df)
  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}

#' @title Compter le nombre d'adjoints
#'
#' @description Cette fonction compte le nombre d'adjoints dans une liste d'élus.
#'
#' @param df : Un data frame contenant une colonne "Libellé.de.la.fonction" indiquant la fonction de chaque élu.
#' @return Un entier représentant le nombre d'adjoints.
#'
#' @seealso \code{\link{compter_nombre_d_elus}}, \code{\link{calcul_distribution_age}}, \code{\link{plot_code_professions}}, \code{\link{trouver_l_elu_le_plus_age}}
#' @examples
#' df_exemple <- data.frame(
#'   Nom.de.l.élu = c("Dupont", "Martin", "Durand"),
#'   Prénom.de.l.élu = c("Jean", "Sophie", "Paul"),
#'   Libellé.de.la.fonction = c("Maire", "Adjoint au maire", "Adjoint aux sports")
#' )
#' @importFrom dplyr filter

compter_nombre_d_adjoints <- function(df) {
  validate_schema(df)

  df |>
    filter(grepl("adjoint", Libellé.de.la.fonction, ignore.case = TRUE)) |>
    nrow()
}

#' @title Trouver l'élu le plus âgé
#'
#' @description Cette fonction trouve l'élu le plus âgé dans une liste d'élus.
#'
#' @param df : Un data frame contenant le nom, le prénom, et la date de naissance des élus.
#' @return Un objet contenant le nom, le prénom, et l’âge de l’élu.
#'
#' @seealso \code{\link{compter_nombre_d_adjoints}}, \code{\link{compter_nombre_d_elus}}, \code{\link{calcul_distribution_age}}, \code{\link{plot_code_professions}}
#'
#' @examples
#' df <- data.frame(Nom.de.l.élu = c("Dupont", "Martin"),
#'                  Prénom.de.l.élu = c("Jean", "Sophie"),
#'                  Date.de.naissance = c("01-01-1950", "15-07-1975"))
#' @importFrom dplyr mutate slice select
#' @importFrom lubridate dmy interval years today

trouver_l_elu_le_plus_age <- function(df) {
  validate_schema(df)
  df |>
    mutate(
      Date.de.naissance = dmy(Date.de.naissance),
      Âge = as.integer(interval(Date.de.naissance, today()) / years(1))
    ) |>
    slice(which.max(Âge)) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Âge)
}

#' @title Calculer la distribution de l'âge
#'
#' @description Cette fonction  calcule les quantiles de l’age des élus.
#'
#' @param df : Un data frame contenant la date de naissance des élus.
#' @return Un objet contenant les quantiles 0, 25, 50, 75, 100 de l’age des élus.
#' @seealso \code{\link{compter_nombre_d_adjoints}}, \code{\link{compter_nombre_d_elus}}, \code{\link{plot_code_professions}}, \code{\link{trouver_l_elu_le_plus_age}}
#' @examples
#' df <- data.frame(Date.de.naissance = c("01-01-1950", "15-07-1975", "10-03-1990"))
#' @importFrom dplyr mutate summarise select
#' @importFrom lubridate dmy

calcul_distribution_age <- function(df) {
  validate_schema(df)

  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance),
           age = as.integer(difftime(Sys.Date(), Date.de.naissance, units = "days") / 365.25)) |>
    summarise(
      quantile_0 = min(age, na.rm = TRUE),
      quantile_25 = quantile(age, 0.25, na.rm = TRUE),
      quantile_50 = quantile(age, 0.50, na.rm = TRUE),
      quantile_75 = quantile(age, 0.75, na.rm = TRUE),
      quantile_100 = max(age, na.rm = TRUE)
    )
}

#' @title Compter le nombre d'élus ayant le même code professionnel.
#'
#' @description Cette fonction compte le nombre d'élus pour chaque code professionnel et le représentera avec un bar chart.
#'
#' @param df : Un data frame contenant les codes des catégories socioprofessionnelles des élus.
#' @return Un bar chart contenant le nombre d'élus pour chaque code professionnel.
#' @seealso \code{\link{compter_nombre_d_adjoints}}, \code{\link{compter_nombre_d_elus}}, \code{\link{calcul_distribution_age}}, \code{\link{trouver_l_elu_le_plus_age}}
#'
#' @examples
#' df <- data.frame(Code.de.la.catégorie.socio.professionnelle = c("A", "B", "A", "C", "B", "A"))
#' @importFrom dplyr group_by summarise arrange
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal

plot_code_professions <- function(df) {
  validate_schema(df)

  count_data <- df |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(nombre = n()) |>
    arrange(nombre)

  ggplot(count_data, aes(
    x = nombre,
    y = reorder(
      Code.de.la.catégorie.socio.professionnelle, nombre
    )
  )
  ) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Nombre d'élus par code de catégorie socio-professionelle",
         x = "Nombre d'élus",
         y = "Code de la catégorie socio-professionnelle") +
    theme_minimal()
}
