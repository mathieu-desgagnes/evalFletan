




> Ce projet utilise des accents/français dans la documentation et les données.
> Pour faciliter le développement local et éviter certains warning de vérification, 
> les variables `_R_CHECK_ASCII_CODE_`, `_R_CHECK_ASCII_DATA_` et `_R_CHECK_SYSTEM_CLOCK_` 
> sont définies dans le fichier .Renviron à la racine du projet.
> Cela garantit que les commandes `devtools::check()` et `R CMD check` passent sans erreurs inutiles.
