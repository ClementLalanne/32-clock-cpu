(******************************************************************************)
                     Projet de Systèmes Digitaux
                           Hsieh Yu-Guan
                          Lalanne Clément
                            Sanchez Téo
                          Stransky François
(******************************************************************************)


Pour compiler notre programme :
ocamlbuild -use-menhir simulator.byte

Pour l'exécuter :
./simulator.byte vitesse
où vitesse est le multiplicateur du temps réel.
Par exemple pour exécuter sur le temps réel il faut exécuter
./simulator.byte 1.
La montre tourne sans problème en temps réel. Pour tester la videsse de
décrochage il suffit d'augmenter le paramètre vitesse jusqu'à ce que la montre
ne soit plus fluide.

Explication des différents fichiers :
clock.ass est notre code assembleur
clock.byte est ce même code assembleur compilé en byte cond
compil_byte.py est notre compilateur assembleur -> byte code
emptyram est notre ram de démarage
micro_processeur.mj est notre processeur en mini jazz
micro_processeur.net est la netlist produite par le compilateur
Rapport.pdf est un rapport de notre projet
Reg_et_ram est une aide mémoire au cas où nous perdrions les codes de démarage
simulator.ml est notre simulateur qui sert aussi de programme principal

Modification des paramètres de démarage :
Pour modifier les secondes minutes heures jours jours_de_la_semaine mois années
il faut rentrer les codes binaires correspondants dans les lignes 4 à 10 de
emptyram.
