# Projet ENERGIE1 BICHON-GONTIER

Ce projet est composé de plusieurs dossiers et de fichier

## Structure des Dossiers

   - **src**: Contient les fichiers source `.f90`.
   - **mod**: Contient les fichiers module `.mod` créés par la compilation.
   - **obj**: Contient les fichiers objet `.o`.
   - **script_gnuplot**: Contient les scripts Gnuplot.
   - **script_bash**: Contient les scripts Bash utilisés dans le Makefile.
   - **dat**: Contient les fichiers `.dat`.
   - **images**: Contient les images générées par la compilation.



## Fichiers

- **makefile**: Contient les instructions de compilation.
- **exe**: Exécutable qui exécute le programme.
- **exe**: Fichier de donnée, sert au choix de la méthode, de la fonction u, ainsi que des paramètres

## Instructions de Compilation

Pour compiler le projet, utilisez le fichier `makefile` en exécutant la commande suivante dans le terminal :

```bash
make
```
Ensuite vous pouvez executer le programme par la commande :
```bash
./exe
```
Pour créer les images, nous passons par des scripts gnuplot, exemple cette commande
```bash
gnuplot script_gnuplot/tracerf.gnu
```
tracera la fonction f approché( en fonction des paramètres du fichier init) et f exacte sur l'image 'dat/f.png'.

## Instructions de modifications

Pour changer les paramètres, vous pouvez vous rendre dans le fichier 'init', et changer les paramètre que vous souhaitez.

Les fonctions tests possibles sont décrite dans le rapport

## Contenue des fichier .f90

Dans le dossier source, vous retrouverez les fichiers .f90 qui corresponde à notre programme, un bref résumé ici :

- **main.f90** : notre corps principale du programme, c'est lui qui fait appel aux différents module pour résoudre le problème

- **const_var.f90** : fichier de variable globale et de paramètre, utile pour la clarté du programme

- **fonctions.f90** : fichier contenant toutes les fonctions( au sens mathématiques) utiles au programme

- **lecture_init.f90** : fichier contenant la subroutine qui lit le fichier "init"

- **mesh.f90** : fichier qui crée notre maillage en espace et en temps  de (2M)*(2M+1) point intérieur à notre espace, ce maillage permet qu'il y ait autant d'équations que d'inconnues **creation_mat.f90** : fichier qui contient les subroutines pour créer nos matrices et vecteurs du problème

- **Operations_matrices.f90** : fichier qui contient les subroutines agissant sur les matrices ( gradient conjugué, regularisation, preconditionnement, ...)

- **approche.f90** : fichier contenant les fonctions qui approche u et f en utilisant les $u_{i,l}$ trouvé

- **erreurL2.f90** : fichier contenant la subroutine calculant l'erreur $L^2$ grâce à un Gauss-Legendre en dimension 2

- **calcul_mu_optimal.f90** : fichier contenant les subroutines calculant le mu_optimal et ecrivant les itérations dans un fichier dans le but de le tracé( pas eu le temps de le faire), à noter que la fonction mu_optimal_pre ne fonctionne pas, nous avons découvert ce bug qu'à la toute fin et n'avons pu le corriger à temps, nous avons donc utilisé $\mu = 0.0001$ dans le cas du preconditionnement

- **ecriture.f90** : fichier contenant les subroutines ecrivant dans les fichier de donnée du dossier "dat/", qui ensuite nous permettent de tracer nos résultats.

