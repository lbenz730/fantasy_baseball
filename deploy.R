library(rsconnect)
cat(Sys.getenv('NAME'))
cat('\n')
cat(Sys.getenv('TOKEN'))
cat('\n')
cat(Sys.getenv('SECRET'))
cat('\n')

setAccountInfo(name = Sys.getenv('NAME'),
               token = Sys.getenv('TOKEN'),
               secret = Sys.getenv('SECRET'))

deployApp(forceUpdate = T, appDir = 'app', appName = 'millburn_savant')
