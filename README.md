# CamelTrouble
CamelTrouble is a multiplayer game based on TankTrouble, written in OCaml.

![Image of CamelTrouble](https://github.com/luOlivia/CamelTrouble/blob/js/resources/gameplay.gif)

[Give it a try!](https://as2626.github.io/cameltrouble/)

## How to play
Player 1 controls their camel via controls WASD and shoot with E. Player 2 uses arrow keys to move and "." to shoot. The objective of the game is to shoot and kill the other player while navigating around the maze.

## Key Features
- Custom player names
- Shooting & death sound effects
- Wall collisions for balls & camels
- Score keeping
- Automatic replay
- Randomly generated mazes

## Description

CamelTrouble is an Ocaml implementation of [TankTrouble](https://tanktrouble.com/). It involves random maze generation, random player spawn locations/orientation, and the ability for both players to move via keyboard inputs. The players can shoot a max of 5 balls, and all of the balls and the camels can demonstrate collisions with walls. Balls move dynamically and ricochet around the maze, before expiring after a period of time. If any ball collides with a camel player, then the game ends and players have the option to replay. After we encompassed the functionality of the original game, we added more features to improve user experiences. This led us to implement start/end screens, player personalization, and sound effects.

## Building the project
1. Ensure Ocaml and several packages are installed
```
opam install Graphics
opam install js_of_ocaml
opam install js_of_ocaml-lwt
```
2. With all dependencies installed, run
```
make play
make build
```
3. Open `index.html` to play!

## Authors

CamelTrouble was made as the final project for Fall 2019 CS 3110 at Cornell University.

Made with 🐫 by Abhay Singh (as2626), Lillian Hong (clillianhong), & Olivia Lu (luolivia)
