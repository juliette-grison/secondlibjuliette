#' Jeu de données
#'
#' Ce jeu de données contient des informations sur les élus d'une zone géographique spécifique en France, incluant leur catégorie socio-professionnelle, leur fonction et des détails administratifs.
#'
#' @format Un (data.frame) de 489972 lignes et 16 colonnes avec les variables suivantes :
#' \describe{
#'   \item{Code.du.département}{Code du département de la commune}
#'   \item{Libellé.du.département}{Nom du département de la commune}
#'   \item{Code.de.la.collectivité.à.statut.particulier}{Code de la collectivité à statut particulier (si applicable)}
#'   \item{Libellé.de.la.collectivité.à.statut.particulier}{Nom de la collectivité à statut particulier (si applicable)}
#'   \item{Code.de.la.commune}{Code de la commune}
#'   \item{Libellé.de.la.commune}{Nom de la commune}
#'   \item{Nom.de.l.élu}{Nom de l'élu}
#'   \item{Prénom.de.l.élu}{Prénom de l'élu}
#'   \item{Code.sexe}{Code indiquant le sexe de l’élu (H = Homme, F = Femme)}
#'   \item{Date.de.naissance}{Date de naissance de l'élu (au format JJ-MM-AAAA)}
#'   \item{Code.de.la.catégorie.socio.professionnelle}{Code de la catégorie socio-professionnelle de l'élu}
#'   \item{Libellé.de.la.catégorie.socio.professionnelle}{Libellé de la catégorie socio-professionnelle}
#'   \item{Date.de.début.du.mandat}{Date de début du mandat de l'élu (au format JJ-MM-AAAA)}
#'   \item{Libellé.de.la.fonction}{Titre de la fonction occupée par l'élu (si disponible)}
#'   \item{Date.de.début.de.la.fonction}{Date de début de la fonction occupée (au format JJ-MM-AAAA) (si disponible)}
#'   \item{Code.nationalité}{Code de la nationalité de l'élu}
#' }
#' @source Données collectées auprès de sources publiques officielles : https://www.data.gouv.fr/fr/datasets/repertoire-national-des-elus-1/
#' @examples
#' # Charger les données
#' data(df)
#'
#' # Afficher les premières lignes
#' head(df)
#'
#' # Résumé statistique des variables
#' summary(df)
#'
#' # Nombre d'élus par département
#' table(df$Libellé.du.département)
#'
#' # Distribution des catégories socio-professionnelles
#' table(df$Libellé.de.la.catégorie.socio.professionnelle)
"df"


