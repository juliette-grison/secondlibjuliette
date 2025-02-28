#' @title Résumé des élu.e.s d'une commune
#'
#' @description Cette fonction fournit un résumé des élu.e.s d'une commune donnée sous forme de `data.frame`.
#' Elle vérifie que l'objet est bien de classe `commune`, qu'il contient une seule commune,
#' et affiche des statistiques clés comme le nombre d'élu.e.s, la distribution des âges
#' et l'élu.e le/la plus âgé.e.
#'
#' @param x Un objet de classe `commune`, sous forme de `data.frame`, contenant au moins une colonne `Libellé.de.la.commune`.
#' @param ... Arguments supplémentaires (actuellement non utilisés).
#'
#' @details La fonction effectue plusieurs vérifications :
#' * Vérifie que `x` est bien un objet de classe `"commune"`.
#' * Vérifie que `x` est un `data.frame`.
#' * Vérifie que la colonne `Libellé.de.la.commune` est présente.
#' * Vérifie que le `data.frame` ne contient qu'une seule commune.
#'
#' Ensuite, elle affiche :
#' * Le nom de la commune.
#' * Le nombre total d'élu.e.s (`compter_nombre_d_elus()`).
#' * La distribution des âges des élu.e.s (`calcul_distribution_age()`).
#' * L'élu.e le/la plus âgé.e (`trouver_l_elu_le_plus_age()`).
#'
#' @return La fonction ne retourne pas de valeur mais affiche des informations sur la commune et ses élu.e.s.
#'
#' @importFrom dplyr arrange distinct filter mutate select slice summarise
#' @importFrom lubridate dmy interval years today
#'
#' @seealso \code{\link{summary.departement}}, \code{\link{plot.commune}}, \code{\link{plot.departement}}
#'
#' @examples
#' df <- data.frame(
#'   "Libellé.de.la.commune" = rep("Paris", 5),
#'   "Nom.de.l.élu" = c("Dupont", "Martin", "Bernard", "Durand", "Petit"),
#'   "Prénom.de.l.élu" = c("Jean", "Marie", "Paul", "Sophie", "Claire"),
#'   "Date.de.naissance" = as.Date(c("12-05-1960", "24-07-1980", "30-03-1975", "10-12-1990", "15-06-1985"))
#' )
#' class(df) <- c("commune", "data.frame")
#' @export
summary.commune <- function(x, ...) {
  if (!inherits(x, "commune")) {
    stop("L'objet fourni n'est pas une commune")
  }

  # Vérifier que x est bien un data.frame
  if (!is.data.frame(x)) {
    stop("L'objet fourni n'est pas un data.frame.")
  }

  # Vérifier que la colonne 'Commune' existe
  if (!"Libellé.de.la.commune" %in% names(x)) {
    stop("La colonne 'Libellé.de.la.commune' est absente du dataframe.")
  }

  # Vérifier qu'il s'agit d'une seule commune
  commune_unique <- unique(x$Libellé.de.la.commune)
  if (length(commune_unique) != 1) {
    stop("Le dataframe contient plusieurs communes. Fournissez un dataframe ne contenant qu'une seule commune.")
  }

  # Extraire le nom de la commune
  nom_commune <- commune_unique

  # Utiliser les fonctions existantes
  nombre_elus <- compter_nombre_d_elus(x)
  distribution_ages <- calcul_distribution_age(x)
  elu_plus_age <- trouver_l_elu_le_plus_age(x)

  # Affichage des résultats
  cat("\nNom de la commune :", nom_commune, "\n")
  cat("Nombre d'élu.e.s :", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s :\n")
  print(distribution_ages)
  cat("\nL'élu.e le/la plus âgé.e :\n")
  print(elu_plus_age)
}

#' @title Résumé des élu.e.s d'un département
#'
#' @description Cette fonction fournit un résumé des élu.e.s d'un département donné sous forme de `data.frame`.
#' Elle vérifie que l'objet est bien de classe `departement`, qu'il contient une seule département,
#' et affiche des statistiques clés comme le nombre de communes, le nombre total d'élu.e.s,
#' la distribution des âges et les élu.e.s les plus âgé.e.s et plus jeunes.
#'
#' @param x Un objet de classe `departement`, sous forme de `data.frame`, contenant au moins une colonne `Libellé.du.département`.
#' @param ... Arguments supplémentaires (actuellement non utilisés).
#'
#' @details La fonction effectue plusieurs vérifications :
#' * Vérifie que `x` est bien un objet de classe `"departement"`.
#' * Vérifie que `x` est un `data.frame`.
#' * Vérifie que la colonne `Libellé.du.département` est présente.
#' * Vérifie que le `data.frame` ne contient qu'un seul département.
#'
#' Ensuite, elle affiche :
#' * Le nom du département.
#' * Le nombre total de communes dans ce département.
#' * Le nombre total d'élu.e.s (`compter_nombre_d_elus()`).
#' * La distribution des âges des élu.e.s (`calcul_distribution_age()`).
#' * L'élu.e le/la plus âgé.e et sa commune.
#' * L'élu.e le/la plus jeune et sa commune.
#' * La commune avec la moyenne d'âge la plus faible et la distribution des âges de ses élu.e.s.
#' * La commune avec la moyenne d'âge la plus élevée et la distribution des âges de ses élu.e.s.
#'
#' @return La fonction ne retourne pas de valeur mais affiche des informations sur le département et ses élu.e.s.
#'
#' @importFrom dplyr group_by select slice mutate summarise slice pull filter distinct ungroup
#' @importFrom lubridate dmy
#'
#' @seealso \code{\link{summary.commune}}, \code{\link{plot.commune}}, \code{\link{plot.departement}}
#'
#' @examples
#' df <- data.frame(
#'   "Libellé.du.département" = rep("Loire-Atlantique", 10),
#'   "Libellé.de.la.commune" = rep(c("Nantes", "Saint-Nazaire", "Châteaubriant", "Ancenis", "Clisson"), each = 2),
#'   "Nom.de.l.élu" = c("Dupont", "Martin", "Bernard", "Durand", "Petit", "Rousseau", "Lemoine", "Moreau", "Fabre", "Girard"),
#'   "Prénom.de.l.élu" = c("Jean", "Marie", "Paul", "Sophie", "Claire", "Louis", "Julie", "Antoine", "Camille", "Luc"),
#'   "Date.de.naissance" = as.Date(c("12-05-1960", "24-07-1980", "30-03-1975", "10-12-1990",
#'                                    "15-06-1985", "18-04-1950", "27-09-2000", "05-03-1972",
#'                                    "22-11-1995", "30-08-1988"))
#' )
#' class(df) <- c("departement", "data.frame")
#' @export
summary.departement <- function(x, ...) {
  if (!inherits(x, "departement")) {
    stop("L'objet fourni n'est pas un département.")
  }

  # Vérifier que x est bien un data.frame
  if (!is.data.frame(x)) {
    stop("L'objet fourni n'est pas un data.frame.")
  }

  # Vérifier que la colonne 'Département' existe
  if (!"Libellé.du.département" %in% names(x)) {
    stop("La colonne 'Libellé.du.département' est absente du dataframe.")
  }

  # Nombre de communes dans le département
  nombre_communes <- length(unique(x$Libellé.de.la.commune))

  # Nombre total d'élus dans le département
  nombre_elus <- compter_nombre_d_elus(x)

  # Distribution des âges des élus du département
  distribution_ages <- calcul_distribution_age(x)

  # Trouver l'élu.e le/la plus âgé.e et sa commune
  elu_plus_age <- x |>
    group_by(Libellé.de.la.commune) |>
    slice(which.min(dmy(Date.de.naissance))) |>
    ungroup() |>
    slice(which.min(dmy(Date.de.naissance))) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Libellé.de.la.commune, Date.de.naissance) |>
    mutate(Âge = as.integer(difftime(Sys.Date(), dmy(Date.de.naissance), units = "days") / 365.25)) |>
    select(-Date.de.naissance)

  # Trouver l'élu.e le/la plus jeune et sa commune
  elu_plus_jeune <- x |>
    group_by(Libellé.de.la.commune) |>
    slice(which.max(dmy(Date.de.naissance))) |>
    ungroup() |>
    slice(which.max(dmy(Date.de.naissance))) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Libellé.de.la.commune, Date.de.naissance) |>
    mutate(Âge = as.integer(difftime(Sys.Date(), dmy(Date.de.naissance), units = "days") / 365.25)) |>
    select(-Date.de.naissance)

  # Calculer la moyenne d'âge par commune
  moyenne_age_par_commune <- x |>
    group_by(Libellé.de.la.commune) |>
    summarise(Age_Moyen = mean(as.integer(difftime(Sys.Date(), dmy(Date.de.naissance), units = "days") / 365.25))) |>
    ungroup()

  # Trouver la commune avec la moyenne d'âge la plus faible
  commune_age_min <- moyenne_age_par_commune |> slice(which.min(Age_Moyen)) |> pull(Libellé.de.la.commune)
  distribution_age_min <- calcul_distribution_age(x |> filter(Libellé.de.la.commune == commune_age_min))

  # Trouver la commune avec la moyenne d'âge la plus élevée
  commune_age_max <- moyenne_age_par_commune |> slice(which.max(Age_Moyen)) |> pull(Libellé.de.la.commune)
  distribution_age_max <- calcul_distribution_age(x |> filter(Libellé.de.la.commune == commune_age_max))

  # Affichage des résultats
  cat("\nNom du département :", unique(x$Libellé.du.département), "\n")
  cat("Nombre de communes :", nombre_communes, "\n")
  cat("Nombre d'élu.e.s :", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s du département :\n")
  print(distribution_ages)

  cat("\nL'élu.e le/la plus âgé.e :\n")
  print(elu_plus_age)

  cat("\nL'élu.e le/la plus jeune :\n")
  print(elu_plus_jeune)

  cat("\nCommune à la moyenne d'âge la plus faible :", commune_age_min, "\n")
  cat("Distribution des âges des élu.e.s dans cette commune :\n")
  print(distribution_age_min)

  cat("\nCommune à la moyenne d'âge la plus élevée :", commune_age_max, "\n")
  cat("Distribution des âges des élu.e.s dans cette commune :\n")
  print(distribution_age_max)
}

#' @title Visualisation des professions des élus d'une commune
#'
#' @description Cette fonction génère un graphique en barres représentant la répartition des élus d'une commune selon leur catégorie socio-professionnelle.
#'
#' @param x Un objet de type `commune`, qui est un `data.frame` contenant les informations d'une seule commune.
#' @param ... Arguments supplémentaires (actuellement non utilisés).
#'
#' @details
#' * La fonction vérifie la présence des colonnes `Libellé.de.la.commune` et `Libellé.du.département`.
#' * Le graphique affiche le nombre d'élus par code professionnel.
#' * L'axe des abscisses indique le nombre total d'élus dans la commune.
#' * Le titre du graphique correspond au nom de la commune suivi de celui du département.
#'
#' @return Un graphique `ggplot` affichant la distribution des élus selon leur catégorie socio-professionnelle.
#'
#' @seealso \code{\link{summary.commune}}, \code{\link{summary.departement}}, \code{\link{plot.departement}}
#'
#' @examples
#' \dontrun{
#' plot.commune(df_ma_commune)
#' }
#'
#' @importFrom dplyr group_by summarise arrange n
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#' @export
plot.commune <- function(x, ...) {
  if (!inherits(x, "commune")) {
    stop("L'objet fourni n'est pas une commune")
  }

  # Vérifier que la commune et le département sont bien définis
  if (!"Libellé.de.la.commune" %in% names(x) | !"Libellé.du.département" %in% names(x)) {
    stop("Les colonnes 'Libellé.de.la.commune' et/ou 'Libellé.du.département' sont absentes du dataframe.")
  }

  # Extraire le nom de la commune et du département
  nom_commune <- unique(x$Libellé.de.la.commune)
  nom_departement <- unique(x$Libellé.du.département)

  if (length(nom_commune) != 1 | length(nom_departement) != 1) {
    stop("Le dataframe contient plusieurs communes ou plusieurs départements. Fournissez un dataframe avec une seule commune.")
  }

  # Compter le nombre d'élus par code professionnel
  count_data <- x |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(nombre = n()) |>
    arrange(nombre)

  # Définir le titre du graphique
  titre_graphique <- paste(nom_commune, "-", nom_departement)

  # Nombre total d'élus dans la commune
  nombre_elus <- sum(count_data$nombre)

  # Définir le label de l'axe des abscisses
  label_x <- paste("Libellés des codes professionnels pour les élus (", nombre_elus, " élus)", sep = "")

  # Créer le graphique avec ggplot
  ggplot(count_data, aes(
    x = nombre,
    y = reorder(Code.de.la.catégorie.socio.professionnelle, nombre)
  )) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = titre_graphique,
         x = label_x,
         y = "Code de la catégorie socio-professionnelle") +
    theme_minimal()
}


#' @title Visualisation des professions des élus d'un département
#'
#' @description Cette fonction génère un graphique en barres représentant la répartition des élus d'un département selon leur catégorie socio-professionnelle.
#'
#' @param x Un objet de type `departement`, qui est un `data.frame` contenant les informations des communes d'un département.
#' @param ... Arguments supplémentaires (actuellement non utilisés).
#'
#' @details
#' * Le dataframe fourni doit contenir un département avec plusieurs communes.
#' * Le graphique affiche les 10 codes professionnels les plus représentés en nombre d'élus.
#' * Le titre du graphique correspond au nom du département suivi du nombre de communes.
#' * L'axe des abscisses précise le département concerné.
#'
#' @return Un graphique `ggplot` affichant la distribution des 10 catégories socio-professionnelles les plus représentées.
#'
#' @seealso \code{\link{summary.commune}}, \code{\link{summary.departement}}, \code{\link{plot.commune}}
#'
#' @examples
#' \dontrun{
#' plot.departement(df_mon_departement)
#' }
#'
#' @importFrom dplyr group_by summarise arrange slice slice_head n
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#' @export
plot.departement <- function(x, ...) {
  if (!inherits(x, "departement")) {
    stop("L'objet fourni n'est pas un département.")
  }

  # Récupération du nom du département et du nombre de communes
  nom_departement <- unique(x$Libellé.du.département)
  nombre_communes <- length(unique(x$Libellé.de.la.commune))

  # Regrouper et compter les élus par code professionnel
  count_data <- x |>
    group_by(Code.de.la.catégorie.socio.professionnelle) |>
    summarise(nombre = n()) |>
    arrange(desc(nombre)) |>  # Trier par ordre décroissant
    slice_head(n = 10)  # Garder les 10 plus fréquents

  # Création du graphique avec ggplot
  ggplot(count_data, aes(
    x = nombre,
    y = reorder(Code.de.la.catégorie.socio.professionnelle, nombre)
  )) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste("Répartition des élus par code professionnel -", nom_departement, "-", nombre_communes, "communes"),
      x = paste("Libellés des 10 codes professionnels les plus représentés pour le département", nom_departement),
      y = "Code de la catégorie socio-professionnelle"
    ) +
    theme_minimal()
}
