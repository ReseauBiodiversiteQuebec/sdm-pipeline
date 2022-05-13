# SDM PIPELINE

 Package R intégrant les fonctions et données pour produire des Species Distribution Model (SDM) dans le contexte de la plateforme Biodiversité Québec. 

 ## Installation

 Pour assurer le fonctionnement du package et éviter des conflits de dépendances avec installation de R de l'utilisateur, nous recommandons d'installer le package `sdmpipeline` dans un environnement de projet.

 ### Dans R Studio

 Créer un nouveau projet et installer le package à partir de ce projet :

Installer le package dans le nouvel environnement.

 ```r
devtools::install_github("ReseauBiodiversiteQuebec/sdm-pipeline")
 ```

### Avec R env

Créer un nouvel environnement R à partir du dossier de travail : 

  ```r
install.package("renv")
renv::init()
quit()
  ```
Redémarrez la console R et installer le package.

 ```r
devtools::install_github("ReseauBiodiversiteQuebec/sdm-pipeline")
 ```