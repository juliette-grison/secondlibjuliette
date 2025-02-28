#' @title Génèrer un rapport à partir d'un fichier .qmd
#'
#' @description Cette fonction permet de générer un rapport en utilisant un fichier .qmd
#' paramétré, en fonction des paramètres de commune, de département, et du
#' chemin de sortie spécifié. Les paramètres sont passés à la fonction
#' via un fichier YAML temporaire, et le rapport est compilé à l'aide de
#' Quarto.
#'
#' @param commune Une chaîne de caractères spécifiant le code de la commune pour
#'   laquelle générer le rapport (par exemple, "Bordeaux").
#' @param departement Une chaîne de caractères spécifiant le code du département
#'   pour lequel générer le rapport (par exemple, "Gironde").
#' @param output Le chemin du fichier où le rapport généré sera sauvegardé.
#'
#' @return Un fichier rapport généré.
#'
#' @import quarto
#' @import yaml
#'
#' @export
generer_rapport <- function(commune, departement, output) {

    rapport_path <- system.file("rapport", "rapport.qmd", package = "firstlibjuliette")

  if (rapport_path == "") {
    stop("Le fichier rapport.qmd n'a pas été trouvé dans le package.")
  }

  # Créer un fichier YAML temporaire avec les paramètres
  params <- list(code_commune = commune, code_departement = departement)
  params_path <- tempfile(fileext = ".yml")
  yaml::write_yaml(params, params_path)

  # Utiliser quarto::quarto_render pour générer le rapport
  quarto::quarto_render(input = rapport_path,
                        output_file = output,   # Spécifier le fichier de sortie
                        execute_params = params_path)  # Passer le fichier YAML temporaire

  # Supprimer le fichier temporaire après l'exécution
  unlink(params_path)
}
