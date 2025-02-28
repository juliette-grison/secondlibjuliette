library(dplyr)
library(lubridate)
library(ggplot2)

# Création d'un jeu de données fictif
test_df <- data.frame(
  Code.du.département = c("75", "33"),
  Libellé.du.département = c("Paris", "Gironde"),
  Code.de.la.collectivité.à.statut.particulier = c(NA, NA),
  Libellé.de.la.collectivité.à.statut.particulier = c(NA, NA),
  Code.de.la.commune = c("75101", "33063"),
  Libellé.de.la.commune = c("Paris 1er", "Bordeaux"),
  Nom.de.l.élu = c("Dupont", "Martin"),
  Prénom.de.l.élu = c("Jean", "Sophie"),
  Code.sexe = c("M", "F"),
  Date.de.naissance = c("01/01/1960", "15/07/1980"),
  Code.de.la.catégorie.socio.professionnelle = c("1", "2"),
  Libellé.de.la.catégorie.socio.professionnelle = c("Cadre", "Employé"),
  Date.de.début.du.mandat = c("01/01/2020", "15/07/2021"),
  Libellé.de.la.fonction = c("Maire", "Adjoint au maire"),
  Date.de.début.de.la.fonction = c("01/01/2020", "15/07/2021"),
  Code.nationalité = c("FR", "FR"),
  stringsAsFactors = FALSE
)

# Tests pour validate_schema
test_that("validate_schema accepte un dataframe valide", {
  expect_silent(validate_schema(test_df))
})

test_that("validate_schema échoue avec un dataframe invalide", {
  df_invalid <- test_df
  names(df_invalid)[1] <- "Code_departement"
  expect_error(validate_schema(df_invalid))
})

# Tests pour compter_nombre_d_elus
test_that("compter_nombre_d_elus compte correctement les élus uniques", {
  expect_equal(compter_nombre_d_elus(test_df), 2)
})

test_that("compter_nombre_d_elus échoue sur un dataframe invalide", {
  df_invalid <- test_df[, -c(7, 8)] # Supprime nom et prénom
  expect_error(compter_nombre_d_elus(df_invalid))
})

# Tests pour compter_nombre_d_adjoints
test_that("compter_nombre_d_adjoints détecte correctement les adjoints", {
  expect_equal(compter_nombre_d_adjoints(test_df), 1)
})

test_that("compter_nombre_d_adjoints retourne zéro si aucun adjoint", {
  df_no_adjoints <- test_df %>% mutate(Libellé.de.la.fonction = "Maire")
  expect_equal(compter_nombre_d_adjoints(df_no_adjoints), 0)
})

# Tests pour trouver_l_elu_le_plus_age
test_that("trouver_l_elu_le_plus_age retourne l'élu le plus âgé", {
  result <- trouver_l_elu_le_plus_age(test_df)
  expect_equal(result$Nom.de.l.élu, "Dupont")
})

test_that("trouver_l_elu_le_plus_age échoue si Date.de.naissance est absente", {
  df_invalid <- test_df %>% select(-Date.de.naissance)
  expect_error(trouver_l_elu_le_plus_age(df_invalid))
})

# Tests pour calcul_distribution_age
test_that("calcul_distribution_age retourne les bons quantiles", {
  result <- calcul_distribution_age(test_df)
  expect_equal(ncol(result), 5) # Vérifie 5 quantiles
})

test_that("calcul_distribution_age échoue si Date.de.naissance est absente", {
  df_invalid <- test_df %>% select(-Date.de.naissance)
  expect_error(calcul_distribution_age(df_invalid))
})

# Tests pour plot_code_professions
test_that("plot_code_professions génère un objet ggplot", {
  plot <- plot_code_professions(test_df)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_code_professions échoue si Code.de.la.catégorie.socio.professionnelle est absent", {
  df_invalid <- test_df %>% select(-Code.de.la.catégorie.socio.professionnelle)
  expect_error(plot_code_professions(df_invalid))
})
