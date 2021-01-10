# Kinetics-Games
## Description
This learning module provides an analogy to chemical kinetics and is designed to easily understand its mechanisms. The program handles a basic kinetic reactions A → B and the progress of the reactinons including monomolecular, bimolecular, catalytic, autocatalytic, consecutive, and equilibrium reactions. 

## Rules
![alt text](https://github.com/MasayukiNagai/Kinetics-Games/blob/master/images/kingames.png)
* Instead of using actual molecules, we assume a container which contains equal-sized balls, A and B, and stores information as colors
* We put our hand into the container and draw ball(s) blindly and successively. Every selected ball is identified visuallyas an A-ball or a B-ball
* When a reaction happens, a drawn ball is replaced by ball of the opposite color taken from stock

## How to run
You can launch the learning module from R using the Shiny package by typing

Run the following command in the R console:

```R
shiny::runGitHub("Kinetics-Games","MasayukiNagai")
```

## Acknowledgement
Developed in summmer 2019 under the supervision of Professor David Harvey at DePauw University.
