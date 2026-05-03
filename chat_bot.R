library(shiny)
library(bslib)
library(ellmer)
library(shinychat)
library(dplyr)
library(readr)
library(purrr)
library(glue)
library(furrr)
library(stringr)

plan(multisession(workers = 12))

# ──────────────────────────────────────────────────────────────────────
# 1. LOAD YOUR DATA
#    Replace this section with however you currently load data in your
#    Shiny app. These are just example placeholders matching your repo
#    structure. The key thing is that these objects exist in the global
#    environment when the app starts.
# ──────────────────────────────────────────────────────────────────────


seasons <- 2020:2026



# Example: load all season files from your local clone of the repo
# data_dir <- "path/to/fantasy_baseball/data/stats"
# years <- 2015:2026
#
df_start <- read_csv('data/df_start.csv')
df_schedule <- 
  map_dfr(seasons, ~{
    read_csv(glue('data/stats/{.x}/schedule_{.x}.csv')) %>% 
      mutate('season' = .x)
  })

df_managers <- read_csv('data/stats/manager_history.csv')
df_teams <- 
  map_dfr(seasons, ~{
    read_csv(glue('data/stats/{.x}/teams_{.x}.csv')) %>% 
      mutate('season' = .x)
  })

df_daily <- map_dfr(seasons, ~read_csv(paste0('data/stats/', .x, '/daily_stats_', .x, '.csv')))
df_historical_playoff_odds <- 
  map_dfr(2021:max(seasons), ~{
    read_csv(paste0('data/playoff_odds/historical_playoff_odds_', .x, '.csv')) %>% 
      mutate('season' = .x)
  })


df_wp <- 
  future_map_dfr(dir('data/win_prob/', full.names = T, recursive = T), 
                 ~{
                   read_csv(.x, show_col_types = F) %>% 
                     mutate('season' = as.numeric(str_extract(.x, '20\\d+\\d+')),
                            'matchup_id' = as.numeric(gsub('.*week_', '', gsub('\\.csv', '', .x))))
                 })

df_transactions <- 
  map_dfr(2022:max(seasons), ~{
    read_csv(glue('data/stats/{.x}/transaction_log_{.x}.csv')) %>% 
      mutate('season' = .x)
  })

df_trades <- 
  map_dfr(2022:max(seasons), ~{
    read_csv(glue('data/stats/{.x}/traded_players_{.x}.csv')) %>% 
      mutate('season' = .x)
  })

old_schedule <- read_csv('data/stats/schedule_2015_2019.csv')
bylaws <- read_file('docs/bylaws.txt')
prompt <- read_file('docs/clade_prompt.md')


# ──────────────────────────────────────────────────────────────────────
# 2. AUTO-GENERATE DATA MANIFEST
#    This builds a description of every data frame so the LLM knows
#    what's available without seeing all the rows.
# ──────────────────────────────────────────────────────────────────────

# List your data frames here (add all of yours)
data_registry <- 
  list(
    'start_cap' = df_start,
    'schedule' = df_schedule,
    'teams' = df_teams,
    'managers' = df_managers,
    'daily_stats' = df_daily,
    'historical_playoff_odds' = df_historical_playoff_odds,
    'win_probability' = df_wp,
    'transactions' = df_transactions,
    'trades' = df_trades,
    'old_schedule' = old_schedule,
    'bylaws' = bylaws
  )

build_manifest <- function(registry) {
  descriptions <- lapply(names(registry), function(name) {
    df <- registry[[name]]
    col_info <- paste(
      sapply(names(df), function(col) {
        paste0("    ", col, " (", class(df[[col]])[1], ")")
      }),
      collapse = "\n"
    )
    sample_rows <- paste(
      utils::capture.output(print(head(df, 5))),
      collapse = "\n"
    )
    paste0(
      "## ", name, "\n",
      "Rows: ", nrow(df), " | Columns: ", ncol(df), "\n",
      "Columns:\n", col_info, "\n\n",
      "Sample rows:\n```\n", sample_rows, "\n```"
    )
  })
  paste(descriptions, collapse = "\n\n")
}

data_manifest <- build_manifest(data_registry)

# ──────────────────────────────────────────────────────────────────────
# 3. SYSTEM PROMPT
# ──────────────────────────────────────────────────────────────────────

system_prompt <- paste0(prompt, data_manifest)

# ──────────────────────────────────────────────────────────────────────
# 4. DEFINE TOOLS
# ──────────────────────────────────────────────────────────────────────

# Tool: run R code against the loaded data
query_data_tool <- tool(
  function(r_code) {
    # Create a safe-ish environment with only the data + dplyr
    safe_env <- new.env(parent = baseenv())
    
    # Attach data frames
    for (name in names(data_registry)) {
      assign(name, data_registry[[name]], envir = safe_env)
    }
    
    # Attach useful packages into the environment
    for (fn_name in getNamespaceExports("dplyr")) {
      try(
        assign(fn_name, getExportedValue("dplyr", fn_name), envir = safe_env),
        silent = TRUE
      )
    }
    for (fn_name in c("head", "tail", "nrow", "ncol", "sum", "mean", "max",
                      "min", "which.max", "which.min", "paste", "paste0",
                      "round", "abs", "sort", "unique", "table", "grepl",
                      "gsub", "as.numeric", "as.character", "is.na",
                      "complete.cases", "data.frame", "c", "seq",
                      "seq_len", "length", "names", "print")) {
      try(
        assign(fn_name, get(fn_name, envir = baseenv()), envir = safe_env),
        silent = TRUE
      )
    }
    # Also pull from stats namespace for things like median, sd, quantile
    for (fn_name in c("median", "sd", "quantile", "cor")) {
      try(
        assign(fn_name, getExportedValue("stats", fn_name), envir = safe_env),
        silent = TRUE
      )
    }
    
    result <- tryCatch(
      {
        val <- eval(parse(text = r_code), envir = safe_env)
        if (is.data.frame(val)) {
          # Truncate large results
          if (nrow(val) > 50) {
            val <- head(val, 50)
            note <- "\n[Showing first 50 rows]"
          } else {
            note <- ""
          }
          paste0(
            paste(utils::capture.output(print(val, n = 50)), collapse = "\n"),
            note
          )
        } else {
          paste(utils::capture.output(print(val)), collapse = "\n")
        }
      },
      error = function(e) {
        paste("Error running code:", e$message)
      }
    )
    result
  },
  name = "query_data",
  description = "Execute R code against the league's data frames. Use dplyr
    for data manipulation. Available data frames and their columns are
    described in the system prompt. Return the R code as a string.",
  arguments = list(
    r_code = type_string("R code to execute. Must be a valid R expression
      using available data frames and dplyr verbs.")
  )
)

# Tool: list available tables
list_tables_tool <- tool(
  function() {
    info <- sapply(names(data_registry), function(name) {
      df <- data_registry[[name]]
      paste0(name, ": ", nrow(df), " rows x ", ncol(df), " cols (",
             paste(names(df), collapse = ", "), ")")
    })
    paste(info, collapse = "\n")
  },
  name = "list_tables",
  description = "List all available data frames with their dimensions and column names.",
  arguments = list()
)

get_bylaws_tool <- tool(
  function() {
    bylaws
  },
  name = "get_bylaws",
  description = "Retrieve the full league bylaws text. Use this when users
    ask about rules, punishments, entry fees, voting procedures, scoring
    rules, the Ohtani rule, start caps, RP caps, playoff procedures,
    transaction rules, or any league governance questions.",
  arguments = list()
)

# ──────────────────────────────────────────────────────────────────────
# 5. SHINY APP
# ──────────────────────────────────────────────────────────────────────


ui <- page_sidebar(
  title = "🛳️ dorothyd.AI",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#F76B1F",
    secondary = "#1B3A5C",
    bg = "#F5F2EC",
    fg = "#1B3A5C",
    base_font = sass::font_google("Oswald"),
    heading_font = sass::font_google("Oswald")
  ),
  tags$head(tags$style(HTML("
  .navbar-brand, .navbar { background-color: #1B3A5C !important; color: #F5F2EC !important; }
  .navbar-brand { color: #F76B1F !important; letter-spacing: 1px; font-size: 1.3em !important; }
  .sidebar { background-color: #1B3A5C !important; }
  .sidebar p, .sidebar li, .sidebar em { color: #B8CEDF !important; font-size: 21px !important; }
  .sidebar .sidebar-title { color: #F5F2EC !important; font-size: 1.5em !important; }
  .sidebar hr { border-color: #2E5070 !important; }
  body, p, li { font-size: 16px !important; }
  .btn-outline-secondary {
    color: #F76B1F !important;
    border-color: #F76B1F !important;
    background: transparent !important;
    font-size: 18px !important;
  }
  .btn-outline-secondary:hover {
    background-color: #F76B1F !important;
    color: white !important;
  }
  #token_usage {
    color: #4A7A9B !important;
    background: transparent !important;
    border: none !important;
    font-size: 14px !important;
    padding: 0;
  }
"))),
  sidebar = sidebar(
    title = "Hi, I'm Dorothy!",
    width = 400,
    p("Your guide to Millburnish league history — all aboard."),
    p(em("Try asking:"), style = "margin-bottom: 4px;"),
    tags$ul(
      style = "font-size: 0.85em; padding-left: 1.2em;",
      tags$li("What's the highest score ever in a single week?"),
      tags$li("Explain the start cap rule"),
      tags$li("What's the longest winning streak?"),
      tags$li("Show me head-to-head record between Alex and Dan"),
      tags$li("Longest odds who ultimately did make the playoffs")
    ),
    hr(),
    p("Be as specific as possible for best results."),
    p('If our conversation gets too long, start a new conversation with the button below.'),
    actionButton("reset_chat", "New Conversation",
                 class = "btn-outline-secondary btn-sm",
                 style = "width: 100%;"),
    verbatimTextOutput("token_usage"),
    p(
      style = "font-size: 0.75em; color: #4A7A9B; text-align: center; margin-top: 8px;",
      "Powered by Claude Sonnet 4.6"
    )
  ),
  chat_ui(
    id = "chat",
    messages = "**Hey! I'm your Dorothy, the official stats bot of the Millburnish Fantasy Baseball League.** Ask me anything about
        league history, records, trades, player stats, or rules. I'll look it up in the data."
  )
)


server <- function(input, output, session) {
  # Create a NEW chat client per session (important for multi-user)
  chat_client <- reactiveVal()
  
  make_chat <- function() {
    
    client <- chat_anthropic(
      model = "claude-sonnet-4-6",
      system_prompt = system_prompt
    )
    # Register tools
    client$register_tool(query_data_tool)
    client$register_tool(list_tables_tool)
    client$register_tool(get_bylaws_tool)
    client
  }
  
  chat_client(make_chat())
  
  
  
  # Handle user messages
  observeEvent(input$chat_user_input, {
    stream <- chat_client()$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
  
  # Reset conversation (clears history, saves tokens on subsequent messages)
  observeEvent(input$reset_chat, {
    chat_client(make_chat())
    chat_clear("chat")
    chat_append("chat", "**Conversation reset!** Ask me anything.")
  })
  
  output$token_usage <- renderText({
    invalidateLater(3000, session)
    input$reset_chat
    
    tryCatch({
      client <- chat_client()
      req(client)
      tokens <- client$get_tokens()
      
      if (nrow(tokens) == 0) return('API Pings: 0 | Estimated Cost: $0.00')
      
      total_input  <- sum(tokens$input,  na.rm = TRUE)
      total_output <- sum(tokens$output, na.rm = TRUE)
      cost <- (total_input / 1e6 * 3) + (total_output / 1e6 * 15)
      
      paste0('API Pings: ', nrow(tokens), ' | Estimated Cost: $', round(cost, 2))
    }, error = function(e) {
      paste0('Error: ', e$message)
    })
  })
  
}

shinyApp(ui, server)


