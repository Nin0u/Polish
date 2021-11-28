# Rapport

Ceci est le rapport de projet à inclure dans la racine comme mentionné dans les modalités de rendu.

## Identifiants

<!-- Format : Nom, Prenom, Identifiant Gilab, N° étudiant -->

| Nom    | Prénom  | ID Gitlab | N° étudiant |
| ------ | ------- | --------- | ----------- |
| YE     | Nicolas | @yen      | 71814766    |
| KELLER | Dylan   | @keller   | 21961014    |

## Fonctionnalités

<!--
    Donnez une description précise des fonctionnalités implémentées
    par votre rendu - sujet minimal, extensions éventuelles,
    éventuellement parties non réalisées ou non encore fonctionnelles.
-->

## Compilation et exécution

<!--
    Documentez ensuite de façon précise la manière dont votre
    projet doit être compilé (normalement via dune) et exécuté (en donnant
    les options acceptées par votre programme). Précisez si vous vous êtes
    servi de bibliothèques externes, et donnez dans ce cas un pointeur
    vers leur documentation.
-->

## Découpage modulaire

<!--
    Donnez une description des traitements pris en charge par chaque
    module (.ml) de votre projet. Précisez le rôle et la nécessité
    de chaque module ajouté au dépôt initial.
-->

Nous avons décidé de découper notre projet de la manière suivante :

- Un module pour lire et interpréter le fichier polish : `ReadPolish`
- Un module pour afficher le code polish interpreté : `PrintPolish`
- Un module pour evaluer le code interpreté : `EvalPolish`

Ce découpage permet d'attribuer à chaque module une tâche précise qui nécéssite plusieurs fonctions auxiliaires.
Nous allégeons alors par la même occasion le fichier `polish.ml`.

## Organisation du travail

<!--
    Cette partie est plus libre dans sa forme. Indiquez la manière
    dont les tâches ont été réparties entre les membres du groupe
    au cours du temps. Donnez une brève chronologie de votre travail
    sur ce projet au cours de ce semestre, avant et après le
    confinement.
-->

11/11/21 : Fork du projet et ajout du Rapport à la racine
13/11/21 : Réunion sur le découpage en module du projet : Un module pour chaque fonction demandée.
28/11/21 : read_polish terminé. Correction éventuelles grâce à print_polish

## Misc

<!--
    Cette partie est entièrement libre : remarques, suggestions,
    questions...
-->
