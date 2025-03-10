test.csv et train.csv : données de test et d'entrainement 

submission.csv : fichier contenant la soumission finale 

init.R : permet d'initialiser les données

experts.R : définit les fonctions semi-online et les modèles de correction (LightGBM, XGBoost, etc...)

load.R , solar.R , wind.R : implémentent respectivement la modélisation de load, solar_power et wind_power

compound.R : contient la prédiction finale et le code de création du fichier de soumission

