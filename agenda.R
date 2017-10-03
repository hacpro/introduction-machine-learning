

#install.packages("data.tree", dependencies = T)
#install.packages("networkD3", dependencies = T)

library(data.tree)
library(networkD3)


# Agenda
agenda <- Node$new("Agenda")
warumR <- agenda$AddChild("Was ist R?")
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

plotGraph(agenda)


# Was ist R
r <- Node$new("The R Language")
paradigmen <- r$AddChild("Paradigmen")
paradigmen$AddChild("funktional")
paradigmen$AddChild("dynamisch")
paradigmen$AddChild("objektorientert")

typisierung <- r$AddChild("Typisierung")
typisierung$AddChild("dynamisch")
typisierung$AddChild("implizit")
typisierung$AddChild("schwach")

r$AddChild("RStudio IDE")
r$AddChild("Open Source")
interpretiert <- r$AddChild("Intepretiert")
optimierungen <- interpretiert$AddChild("Optimierungen")
optimierungen$AddChild("Vorkompilierte Funktionen")
optimierungen$AddChild("JIT Kompiler")

plotGraph(r)



plotGraph <- function(node)
{
  SetGraphStyle(node, rankdir = "TB")
  SetEdgeStyle(node, arrowhead = "vee", color = "grey35", penwidth = 2)
  SetNodeStyle(node, style = "filled,rounded", shape = "box", 
               fillcolor = "GreenYellow", 
               fontname = "Arial", tooltip = GetDefaultTooltip)
  # SetNodeStyle(wasIstML, 
  #              fillcolor = "LightBlue", penwidth = 3,
  #              fontsize = 20 )
  plot(node)
}


agendaNetwork <- ToDataFrameNetwork(agenda)
simpleNetwork(agendaNetwork, fontSize = 16,
              linkDistance = 50, charge = -200, 
              opacity = .3,
              zoom = T,
              fontFamily = "Arial")

