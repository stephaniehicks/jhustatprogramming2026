#######################################
#### To deploy a quarto dashboard on shinyapps.io
######################################

library(quarto)
quarto_publish_app(server = "shinyapps.io")
