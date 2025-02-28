#' @title Création d'un objet de classe commune
#'
#' @description Cette fonction transforme un `data.frame` en objet de classe `commune`, en s'assurant que les colonnes nécessaires sont présentes.
#'
#' @param df : Un `data.frame` contenant les informations d'une commune.
#'
#' @details
#' * Le `data.frame` doit contenir au moins les colonnes `Libellé.de.la.commune` et `Libellé.du.département`.
#' * La classe de l'objet en sortie est définie comme `commune`, ce qui permet l'utilisation de méthodes spécifiques comme `plot.commune`.
#'
#' @return Un objet de classe `commune`.
#'
#' @seealso \code{\link{creer_departement}}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Libellé.de.la.commune = "Paris",
#'   Libellé.du.département = "Paris",
#'   Code.de.la.catégorie.socio.professionnelle = c("A", "B", "C"),
#'   stringsAsFactors = FALSE
#' )
#' commune_obj <- creer_commune(df)
#' class(commune_obj)
#' }
#'
#' @export
creer_commune <- function(df) {
  # Vérification que le dataframe contient bien les colonnes nécessaires
  required_cols <- c("Libellé.de.la.commune", "Libellé.du.département")
  if (!all(required_cols %in% names(df))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé.de.la.commune' et 'Libellé.du.département'.")
  }

  # Définition de la classe de l'objet
  class(df) <- c("commune", class(df))
  return(df)
}


#' @title Création d'un objet de classe département
#'
#' @description Cette fonction transforme un `data.frame` en objet de classe `departement`, en s'assurant que les colonnes nécessaires sont présentes.
#'
#' @param df : Un `data.frame` contenant les informations des communes d'un département.
#'
#' @details
#' * Le `data.frame` doit contenir au moins les colonnes `Libellé.du.département` et `Libellé.de.la.commune`.
#' * La classe de l'objet en sortie est définie comme `departement`, ce qui permet l'utilisation de méthodes spécifiques comme `plot.departement`.
#'
#' @return Un objet de classe `departement`.
#'
#' @seealso \code{\link{creer_commune}}
#'
#' @examples
#' \dontrun{
#' df_departement <- data.frame(
#'   Libellé.de.la.commune = c("Nantes", "Saint-Herblain"),
#'   Libellé.du.département = "Loire-Atlantique",
#'   Code.de.la.catégorie.socio.professionnelle = c("A", "B", "C"),
#'   stringsAsFactors = FALSE
#' )
#' departement_obj <- creer_departement(df_departement)
#' class(departement_obj)
#' }
#'
#' @export
creer_departement <- function(df) {
  # Vérification que le dataframe contient bien les colonnes nécessaires
  required_cols <- c("Libellé.du.département", "Libellé.de.la.commune")
  if (!all(required_cols %in% names(df))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé.du.département' et 'Libellé.de.la.commune'.")
  }

  # Définition de la classe de l'objet
  class(df) <- c("departement", class(df))
  return(df)
}
