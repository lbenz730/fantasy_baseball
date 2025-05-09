devtools::install_version('rsconnect', 
                          version = '1.0.1', 
                          repos='http://cran.rstudio.com/', 
                          upgrade = F)
library(rsconnect)

setAccountInfo(name = Sys.getenv('NAME'),
               token = Sys.getenv('TOKEN'),
               secret = Sys.getenv('SECRET'))

deployApp(forceUpdate = T, appDir = 'app', appName = 'millburn_savant')
