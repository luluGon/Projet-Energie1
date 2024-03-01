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
