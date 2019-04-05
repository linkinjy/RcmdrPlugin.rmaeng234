Menu.rmaeng2 <- function(){
  win2 <- tktoplevel()
win2$env$menu <- tk2menu(win2)
tkconfigure(win2, menu = win2$env$menu)
win2$env$menuFile <- tk2menu(win2$env$menu, tearoff = FALSE)
# Our cascaded menu
win2$env$menuOpenRecent <- tk2menu(win2$env$menuFile, tearoff = FALSE)
tkadd(win2$env$menuOpenRecent, "command", label = "Recent File 1",
      command = function() tkmessageBox(
        message = "I don't know how to open Recent File 1", icon = "error"))
tkadd(win2$env$menuOpenRecent, "compmand", label = "Recent File 2",          #####
      command = function() tkmessageBox(
        message = "I don't know how to open Recent File 2", icon = "error")) #####
tkadd(win2$env$menuFile, "cascade", label = "ANOVA",
      menu = win2$env$menuOpenRecent)
tkadd(win2$env$menuFile, "cascade", label = "Split Design",
      menu = win2$env$menuOpenRecent)                             #################
tkadd(win2$env$menuFile, "cascade", label = "Latin Square Design",
      menu = win2$env$menuOpenRecent)
tkadd(win2$env$menuFile, "cascade", label = "Factorial Design",
      menu = win2$env$menuOpenRecent)
tkadd(win2$env$menuFile, "cascade", label = "Confounding Design",
      menu = win2$env$menuOpenRecent)
tkadd(win2$env$menuFile, "cascade", label = "Fractional Replication",
      menu = win2$env$menuOpenRecent)
tkadd(win2$env$menuFile, "cascade", label = "Table of Orthogonal Arrays",
      menu = win2$env$menuOpenRecent)
tkadd(win2$env$menuFile, "cascade", label = "Response Surface Analyze",
      menu = win2$env$menuOpenRecent)
tkadd(win2$env$menuFile, "command", label = "Quit",
      command = function() tkdestroy(win2))
tkadd(win2$env$menu, "cascade", label = "DOE", menu = win2$env$menuFile)
}
