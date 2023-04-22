library(rsconnect)
library(renv)

renv::restore()

setAccountInfo(name = Sys.getenv('NAME'),
               token = Sys.getenv('TOKEN'),
               secret = Sys.getenv('SECRET'))

deployApp(forceUpdate = T, appDir = 'app', appName = 'millburn_savant')
