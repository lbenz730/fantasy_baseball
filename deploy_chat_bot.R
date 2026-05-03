library(rsconnect)

setAccountInfo(name = Sys.getenv('NAME'),
               token = Sys.getenv('TOKEN'),
               secret = Sys.getenv('SECRET'))

deployApp(forceUpdate = T,        # root of the repo becomes the base
          appPrimaryDoc = 'chat_bot/app.R',
          appFiles = c(
            '.Renviron',
            'chat_bot/app.R',
            'chat_bot/docs/bylaws.txt',
            'chat_bot/docs/clade_prompt.md',
            list.files('data/', recursive = TRUE, full.names = TRUE)
          ), 
          appId = 17282689,
          appName = 'dorothy_dAI')
