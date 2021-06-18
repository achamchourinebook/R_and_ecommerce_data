# Packages

# >> install with a check
ip <- installed.packages()
my.packages <- c("ggplot2", "dplyr", "lubridate", "RODBC")

# if you don't understand what the next statement does - no worries
# we will discuss how it works later
to.install <- my.packages[!(my.packages %in% ip[,"Package"])]

if(length(to.install) > 0) 
  install.packages(to.install, dependencies = TRUE)
# << install with check

# load some of them
library(ggplot2)
library(dplyr)
library(lubridate)

# unload 
detach("package:ggplot2", unload = TRUE, character.only = TRUE)
detach("package:dplyr", unload = TRUE, character.only = TRUE)
detach("package:lubridate", unload = TRUE, character.only = TRUE)