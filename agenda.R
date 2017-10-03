

#install.packages("data.tree", dependencies = T)
install.packages("networkD3", dependencies = T)

library(data.tree)
library(networkD3)

agenda <- Node$new("Agenda")
warumR <- agenda$AddChild("Warum R?")
wasIstML <- agenda$AddChild("Was ist maschinelles Lernen?")
wasIstML$AddChild("Buzzword cleanup")
wasIstML$AddChild("Welche Fragen kann ML beantworten?")
workshop <- agenda$AddChild("Workshop/Tutorial")
arten <- agenda$AddChild("Arten von maschinellem Lernen")
arten$AddChild("Supervised Learning")
arten$AddChild("Unsupervised Learning")
arten$AddChild("Reinforcement Learning")
vs2017 <- agenda$AddChild("ML mit Visual Studio 2017")
advanced <- agenda$AddChild("Weiterführende Konzepte")


SetGraphStyle(agenda, rankdir = "TB")
SetEdgeStyle(agenda, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(agenda, style = "filled,rounded", shape = "box", 
             fillcolor = "GreenYellow", 
             fontname = "Arial", tooltip = GetDefaultTooltip)
SetNodeStyle(wasIstML, 
             fillcolor = "LightBlue", penwidth = 3,
             fontsize = 20 )
# ev. Children dann gross und sonst klein
plot(agenda)


agendaNetwork <- ToDataFrameNetwork(agenda)
simpleNetwork(agendaNetwork, fontSize = 16,
              linkDistance = 50, charge = -200, 
              opacity = .3,
              zoom = T,
              fontFamily = "Arial")

