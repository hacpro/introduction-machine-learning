#   Modell-free reinforcement learning
# download and install latest version from GitHub
devtools::install_github("nproellochs/ReinforcementLearning")

library(ReinforcementLearning)

# Beispiel Daten laden (100k zufaellig ausgewaelte Tic-Tac-Toe Spiele)
data(tictactoe)

head(tictactoe, 50)

# Parameter fuer Reinforcement Learning definieren
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Reinforcement learning ausfuehren
model <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", control = control)

# Modell ausgeben
print(model)

# Policy ausgeben (bester Zug fuer entsprechendes State)
policy(model)

# Beispiel fuer gelerntes Handel

# State
#     B . .
#     B X .
#     . . X

# Algorithmus nach Zug fragen
policy(model)["B..BX...X"]

##  Vorgeschlagener Zug "c7" macht Sinn


# Alternativ koennte das Lernen auch ueber eine Funktion 
# gemacht werden, welche die Uebergaenge der Zustaende modelliert 
# (see environment function)
