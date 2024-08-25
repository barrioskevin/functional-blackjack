# Blackjack in Scala
## Overview

This project is a Scala implementation of a standard double-deck Blackjack game. It simulates a casino-style Blackjack game and is designed to demonstrate the use of immutable data structures in Scala to handle game logic.
## Features

- **Immutable Data Structures**: Utilizes Scala's immutable data structures to manage game state.
- **Standard Rules**: Implements the rules of Blackjack, including card values, player and dealer actions, and game outcomes.
- **Double Deck**: Uses a standard double-deck for card dealing.
- **Game Logic**: Handles various game states and transitions using pattern matching.
- **Scalability**: Easily extendable to add more features or modify existing ones.

## Getting Started

### Prerequisites

- [Java JDK](https://www.oracle.com/java/technologies/javase-jdk11-downloads.html) (version 11 or later)
- [sbt](https://www.scala-sbt.org/download.html) (Scala Build Tool)

### Installation

1. **Clone the Repository:**
   - Open your terminal and run:
     ```bash
     git clone https://github.com/imawful/functional-blackjack.git
     ```

2. **Navigate to the Project Directory:**
   - Change to the project directory:
     ```bash
     cd functional-blackjack
     ```

3. **Install sbt (Scala Build Tool):**
   - If you don't have sbt installed, follow the instructions on the [sbt website](https://www.scala-sbt.org/download.html) to install it.

4. **Compile the Project:**
   - Use sbt to compile the project:
     ```bash
     sbt compile
     ```

5. **Run the Game:**
   - After compilation, run the game with:
     ```bash
     sbt run
     ```
## Usage

0. **SBT Commands:**
   - This project uses sbt (Scala Build Tool). Common commands include:
     - **Compile the Code:** Run `sbt compile` to compile the project code.
     - **Run the Game:** Use `sbt run` to start the game.

1. **Starting the Game:**
   - Open your terminal, navigate to the project directory, and run:
     ```bash
     sbt run
     ```
   - This will start the Blackjack game.

2. **Playing the Game:**
   - Follow the on-screen instructions to play Blackjack.
   - You'll be prompted to make decisions such as hitting or standing.

3. **Game Flow:**
   - The game simulates card dealing, score calculations, and determines the winner based on standard Blackjack rules.

4. **Restarting the Game:**
   - To start a new game, restart the game by running `sbt run` again.
   - The game ends when there are no players left. Players can be removed either by opting to leave or if they have insufficient bankroll to place the minimum bet.


## Acknowledgements

- [Scala Documentation](https://docs.scala-lang.org/)
- [sbt Documentation](https://www.scala-sbt.org/1.x/docs/)
