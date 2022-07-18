### Knit File
rmarkdown::render(input = 'fantasy.rmd',
                  output_file ='fantasy.html')
                  

### Upload file
rsconnect::rpubsUpload(title = 'Millburn Fantasy', 
                       contentFile = 'fantasy.html',
                       originalDoc = 'fantasy.Rmd',
                       id = 'https://api.rpubs.com/api/v1/document/905951/1f88063ceeae401cb4bf80f1450a6961') 
