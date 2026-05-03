devtools::install_version('rsconnect', 
                          version = '1.0.1', 
                          repos='http://cran.rstudio.com/', 
                          upgrade = F)
library(rsconnect)

setAccountInfo(name = Sys.getenv('NAME'),
               token = Sys.getenv('TOKEN'),
               secret = Sys.getenv('SECRET'))

deployApp(forceUpdate = T,        # root of the repo becomes the base
          appPrimaryDoc = 'chat_bot/app.R',
          appFiles = c(
            'chat_bot/app.R',
            'chat_bot/docs/bylaws.txt',
            'chat_bot/docs/clade_prompt.md',
            list.files('data/', recursive = TRUE, full.names = TRUE)
          ),
          appName = 'dorothy_dAI',
          envVars = c("ANTHROPIC_API_KEY"))
