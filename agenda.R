

#install.packages("data.tree", dependencies = T)
install.packages("networkD3", dependencies = T)

library(data.tree)
library(networkD3)

agenda <- Node$new("Einführung in Machinelles Lernen")
ziele <- agenda$AddChild("Definition Kursziele")
warumR <- ziele$AddChild("Was ist R und warum RStudio")
wasIstML <- warumR$AddChild("Was ist maschinelles Lernen")
workshop <- wasIstML$AddChild("Workshop/Tutorial")
arten <- workshop$AddChild("Arten von maschinellem Lernen")
advanced <- arten$AddChild("Weiterführende Konzepte")


SetGraphStyle(agenda, rankdir = "TB")
SetEdgeStyle(agenda, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(agenda, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
             fontname = "Arial", tooltip = GetDefaultTooltip)
plot(agenda)


agendaNetwork <- ToDataFrameNetwork(agenda, "name")
simpleNetwork(agendaNetwork, fontSize = 16, linkDistance = 50,
              charge = -200,
              fontFamily = "Arial")

