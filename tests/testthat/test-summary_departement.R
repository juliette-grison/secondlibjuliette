# Création d'un jeu de données pour les tests de summary.departement()
df_departement <- data.frame(
  "Libellé.du.département" = rep("Loire-Atlantique", 10),
  "Libellé.de.la.commune" = rep(c("Nantes", "Saint-Nazaire", "Châteaubriant", "Ancenis", "Clisson"), each = 2),
  "Nom.de.l.élu" = c("Dupont", "Martin", "Bernard", "Durand", "Petit", "Rousseau", "Lemoine", "Moreau", "Fabre", "Girard"),
  "Prénom.de.l.élu" = c("Jean", "Marie", "Paul", "Sophie", "Claire", "Louis", "Julie", "Antoine", "Camille", "Luc"),
  "Date.de.naissance" = as.Date(c("1960-05-12", "1980-07-24", "1975-03-30", "1990-12-10", "1985-06-15", "1950-04-18", "2000-09-27", "1972-03-05", "1995-11-22", "1988-08-30"))
)
class(df_departement) <- c("departement", "data.frame")

test_that("summary.departement() renvoie une erreur si l'objet n'est pas de classe departement", {
  df_invalide <- df_departement
  class(df_invalide) <- "data.frame" # Retirer la classe "departement"
  expect_error(summary.departement(df_invalide), "L'objet fourni n'est pas un département")
})

test_that("summary.departement() renvoie une erreur si plusieurs départements sont présents", {
  df_multiple_departement <- df_departement
  df_multiple_departement$Libellé.du.département <- c("Loire-Atlantique", "Gironde", "Nord", "Rhône", "Bouches-du-Rhône", "Var", "Hérault", "Isère", "Finistère", "Haute-Garonne")
  expect_error(summary.departement(df_multiple_departement), "Le dataframe contient plusieurs départements")
})
