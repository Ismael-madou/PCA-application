# Installation des packages utilitaires git2r et remotes
to_install <- setdiff(c("git2r", "remotes"), installed.packages()[,"Package"])
if (length(to_install) != 0) {install.packages(to_install)}
# Création du jeton d'accès au dépot urcadown
# Remplacer [JETON] par le jeton communiqué par l'enseignant.
Sys.setenv(`URCADOWN_PAT` = "x_Ejn6-StCE_P2fcsv_G")
pat <- git2r::cred_token(token = "URCADOWN_PAT")
# Installation du package urcadown
remotes::install_git(url = "https://gitlab-mi.univ-reims.fr/pregnault/urcadown.git", 
                     credentials = pat,
                     build_vignettes = TRUE)
