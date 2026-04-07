# figures/weekly_summary.R
# Weekly Summary Graphics for Millburnish Fantasy Baseball
#
# Produces three PNG files matching the app's gt table style:
#   week_{N}_scoreboard.png  – matchup results
#   week_{N}_standings.png   – standings + odds movement
#   week_{N}_performers.png  – top individual performers
#
# Usage (run from project root):
#   source(here('figures/weekly_summary.R'))
#   make_weekly_summary(season = 2026, week = 1)

library(dplyr)
library(gt)
library(gtExtras)
library(glue)
library(here)
library(ggsci)


# ── Logo lookup builder ───────────────────────────────────────────────────────
# Uses change_logo() from app/helpers.R. Must temporarily setwd to app/ so
# change_logo()'s dir('www') check resolves correctly, then converts the
# resulting www/... paths to absolute app/www/... paths.
build_logo_lookup <- function(teams_df) {
  old_wd <- setwd(here('app'))
  on.exit(setwd(old_wd))
  df <- change_logo(teams_df)
  setNames(
    normalizePath(file.path(here('app'), df$logo), winslash = '/', mustWork = FALSE),
    df$team
  )
}

# ── Shared gt styling ─────────────────────────────────────────────────────────
# Matches tab_options() used throughout server.R
apply_gt_style <- function(gt_tbl) {
  gt_tbl %>%
    tab_options(
      column_labels.font.size   = 20,
      heading.title.font.size   = 40,
      heading.subtitle.font.size = 40,
      heading.title.font.weight = 'bold',
      heading.subtitle.font.weight = 'bold',
      column_labels.font.weight = 'bold'
    ) %>%
    tab_style(
      style     = cell_borders(sides = 'bottom', color = 'black', weight = px(3)),
      locations = cells_column_labels(columns = everything())
    ) %>%
    cols_align(align = 'center', columns = everything())
}

amber_pal <- function(n = 100) ggsci::rgb_material('amber', n = n)

# ── Win probability inline SVG sparkline ─────────────────────────────────────
# Builds a tiny SVG with a fixed 50% dashed reference line and the win prob
# trajectory as a steelblue line. gt renders it via text_transform + gt::html().
wp_sparkline_svg <- function(vals, width = 160, height = 40) {
  if (is.null(vals) || length(vals) < 2) return('---')
  n      <- length(vals)
  xs     <- (seq_len(n) - 1) / (n - 1) * width          # x: spread across width
  ys     <- (1 - vals) * height                          # y: flip (SVG y down)
  path_d <- paste0(
    'M', round(xs[1], 1), ',', round(ys[1], 1), ' ',
    paste0('L', round(xs[-1], 1), ',', round(ys[-1], 1), collapse = ' ')
  )
  mid_y  <- round(0.5 * height, 1)                       # y coord for 50%
  paste0(
    '<svg width="', width, '" height="', height, '" style="display:block">',
    '<line x1="0" y1="', mid_y, '" x2="', width, '" y2="', mid_y,
    '" stroke="#aaa" stroke-width="1" stroke-dasharray="4,3"/>',
    '<path d="', path_d,
    '" fill="none" stroke="steelblue" stroke-width="2"',
    ' stroke-linejoin="round" stroke-linecap="round"/>',
    '</svg>'
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1 – Matchup Scoreboard
# ══════════════════════════════════════════════════════════════════════════════
make_scoreboard_table <- function(df_week, season, week, logo_lookup, close_thresh_pct = 0.03) {

  matchups <- df_week %>%
    group_by(game_id) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      # slice(1) gives home team as `team`; preserve for wp join
      home_team  = team,
      away_team  = team_opp,
      home_wins  = total_points >= total_points_opp,
      win_team   = if_else(home_wins, team,     team_opp),
      los_team   = if_else(home_wins, team_opp, team),
      win_score  = if_else(home_wins, total_points, total_points_opp),
      los_score  = if_else(home_wins, total_points_opp, total_points),
      margin     = win_score - los_score,
      close_game = margin / ((win_score + los_score) / 2) <= close_thresh_pct,
      logo_win   = logo_lookup[win_team],
      logo_los   = logo_lookup[los_team]
    ) %>%
    arrange(margin)
  

  # ── Win probability sparklines ──────────────────────────────────────────────
  # win_prob in the csv = home team's probability.
  # We flip it for matchups where the away team won, so the sparkline always
  # shows the WINNER's probability (should trend toward 1.0).
  wp_path <- here(glue('data/win_prob/{season}/week_{week}.csv'))
  has_wp  <- file.exists(wp_path)

  if (has_wp) {
    wp_raw <- read_csv(wp_path, show_col_types = FALSE)

    wp_sparks <- wp_raw %>%
      arrange(day_of_matchup) %>%
      group_by(team_home, team_away) %>%
      summarise(spark = list(win_prob), .groups = 'drop')

    matchups <- matchups %>%
      left_join(wp_sparks, by = c('home_team' = 'team_home',
                                  'away_team'  = 'team_away')) %>%
      # Flip probability for games where away team won
      mutate(win_prob_spark = map2_chr(spark, home_wins, function(s, hw) {
        wp_sparkline_svg(if (isTRUE(hw)) s else 1 - s)
      })) %>%
      select(-spark)
  }

  score_range <- range(c(matchups$win_score, matchups$los_score), na.rm = TRUE)

  df_gt <- matchups %>%
    select(logo_win, win_team, win_score,
           los_score, los_team, logo_los,
           margin,
           { if (has_wp) 'win_prob_spark' else NULL })

  tbl <- gt(df_gt) %>%

    # Logos
    text_transform(
      locations = cells_body(columns = c(logo_win, logo_los)),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%

    # Format scores and margin
    fmt_number(columns = c(win_score, los_score, margin), decimals = 1, sep_mark = '') %>%

    # Amber heatmap on scores
    data_color(
      columns = c(win_score, los_score),
      fn = scales::col_numeric(palette = amber_pal(), domain = score_range),
      autocolor_text = FALSE
    ) %>%

    # Winner side = lightgreen
    tab_style(
      style     = cell_fill(color = 'lightgreen'),
      locations = cells_body(columns = c(logo_win, win_team, win_score))
    ) %>%
    # Loser side = pink
    tab_style(
      style     = cell_fill(color = 'pink'),
      locations = cells_body(columns = c(los_score, los_team, logo_los))
    ) %>%
    # Bold winning score
    # tab_style(
    #   style     = cell_text(weight = 'bold', size = px(18)),
    #   locations = cells_body(columns = win_score)
    # ) %>%

    # Close game: highlight margin cell
    tab_style(
      style     = cell_fill(color = '#fff3cd'),
      locations = cells_body(columns = margin, rows = matchups$close_game)
    ) %>%
    tab_style(
      style     = cell_text(weight = 'bold', color = 'darkorange'),
      locations = cells_body(columns = margin, rows = matchups$close_game)
    ) %>%

    # Dividers
    tab_style(
      style     = cell_borders(sides = 'right', color = 'black', weight = px(3)),
      locations = cells_body(columns = c(logo_los, win_score, margin))
    ) %>%

    cols_label(
      logo_win  = '',
      win_team  = '\U1f3c6 Winner',
      win_score = '',
      margin    = 'Margin',
      los_score = '',
      los_team  = 'Loser',
      logo_los  = ''
    ) %>%

    tab_header(
      title    = md('**Week Results**'),
      subtitle = md(glue('**Week {week}  \u2022  {season} Season**'))
    ) %>%
    tab_footnote(
      footnote  = 'Sorted by margin (closest games first). Highlighted margin = within 3% of avg score.',
      placement = 'left'
    ) %>%

    apply_gt_style()

  # Render SVG sparklines if win prob data exists
  if (has_wp) {
    tbl <- tbl %>%
      fmt(
        columns = win_prob_spark,
        fns     = function(x) lapply(x, gt::html)
      ) %>%
      cols_label(win_prob_spark = 'Win Prob.')
  }

  tbl
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2 – Standings + Odds Movement
# ══════════════════════════════════════════════════════════════════════════════
make_standings_table <- function(exp_standings, playoff_history, week, season, logo_lookup) {

  curr <- playoff_history %>% filter(matchup_id == week)
  prev <- playoff_history %>% filter(matchup_id == week - 1)
  if (nrow(prev) == 0) {
    prev <- curr %>% mutate(playoffs = 1/3, last_place = 1/12, champ = 1/12)
  }

  df <- exp_standings %>%
    filter(!is.na(win)) %>%       # drop league average row if present
    mutate(rank_pos = row_number()) %>%
    left_join(curr %>% select(team, playoff_prob = playoffs,
                              ferry_prob = last_place, champ_prob = champ),
              by = 'team') %>%
    left_join(prev %>% select(team, prev_playoff = playoffs, prev_ferry = last_place),
              by = 'team') %>%
    arrange(desc(playoff_prob)) %>%
    mutate(
      playoff_chg = playoff_prob - prev_playoff,
      ferry_chg   = ferry_prob   - prev_ferry,
      logo        = logo_lookup[team],
      record      = paste0(win, '-', loss)
    ) %>%
    select(rank_pos, logo, team, record,
           total_points, batting_points, sp_points, rp_points,
           playoff_prob, playoff_chg, ferry_prob, ferry_chg, champ_prob)

  pts_range <- range(df$total_points, na.rm = TRUE)

  # Helper: format change column as "+X.X%" with color
  fmt_chg <- function(x) {
    ifelse(is.na(x), '---',
           paste0(ifelse(x >= 0, '+', ''), formatC(100 * x, digits = 1, format = 'f'), '%'))
  }

  df <- df %>%
    mutate(
      playoff_chg_label = fmt_chg(playoff_chg),
      ferry_chg_label   = fmt_chg(ferry_chg)
    )

  gt(select(df, rank_pos, logo, team, record,
            total_points, batting_points, sp_points, rp_points,
            playoff_prob, playoff_chg_label, ferry_prob, ferry_chg_label, champ_prob)) %>%

    # Logos
    text_transform(
      locations = cells_body(columns = logo),
      fn = function(x) local_image(filename = x, height = 40)
    ) %>%

    fmt_number(columns = c(total_points, batting_points, sp_points, rp_points),
               decimals = 1, sep_mark = '') %>%
    fmt_percent(columns = c(playoff_prob, ferry_prob, champ_prob),
                decimals = 1, sep_mark = '') %>%
    sub_missing(columns = everything(), missing_text = '---') %>%

    # Amber heatmap on points
    data_color(
      columns = c(total_points, batting_points, sp_points, rp_points),
      fn = scales::col_numeric(palette = amber_pal(), domain = NULL),
      autocolor_text = FALSE
    ) %>%
    data_color(
      columns = c(playoff_prob, ferry_prob, champ_prob),
      fn = scales::col_numeric(palette = amber_pal(), domain = c(0, 1)),
      autocolor_text = FALSE
    ) %>%

    # Colour the change labels: green = better playoff odds, red = worse
    tab_style(
      style     = cell_text(color = 'seagreen', weight = 'bold'),
      locations = cells_body(columns = playoff_chg_label,
                             rows = df$playoff_chg > 0)
    ) %>%
    tab_style(
      style     = cell_text(color = 'red', weight = 'bold'),
      locations = cells_body(columns = playoff_chg_label,
                             rows = df$playoff_chg < 0)
    ) %>%
    # Ferry: going DOWN is good
    tab_style(
      style     = cell_text(color = 'seagreen', weight = 'bold'),
      locations = cells_body(columns = ferry_chg_label,
                             rows = df$ferry_chg < 0)
    ) %>%
    tab_style(
      style     = cell_text(color = 'red', weight = 'bold'),
      locations = cells_body(columns = ferry_chg_label,
                             rows = df$ferry_chg > 0)
    ) %>%

    # Section dividers
    tab_style(
      style     = cell_borders(sides = 'right', color = 'black', weight = px(3)),
      locations = cells_body(columns = c(logo, record, rp_points, champ_prob))
    ) %>%
    
    tab_style(
      style     = cell_borders(sides = 'bottom', color = 'black', weight = px(3)),
      locations = cells_body(rows = 4, columns = everything())
    ) %>%

    # Spanners
    tab_spanner(label = 'Total Points',
                columns = c(total_points, batting_points, sp_points, rp_points)) %>%
    tab_spanner(label = 'Season Simulations',
                columns = c(playoff_prob, playoff_chg_label, ferry_prob, ferry_chg_label, champ_prob)) %>%

    cols_label(
      rank_pos         = '#',
      logo             = '',
      team             = 'Team',
      record           = 'W-L',
      total_points    = 'Total',
      batting_points  = 'Batting',
      sp_points       = 'SP',
      rp_points       = 'RP',
      playoff_prob     = 'Playoffs',
      playoff_chg_label = '\u0394',
      ferry_prob       = 'Ferry',
      ferry_chg_label  = '\u0394',
      champ_prob       = 'Champion'
    ) %>%

    tab_header(
      title    = md('**Standings & Odds**'),
      subtitle = md(glue('**Week {week}  \u2022  {season} Season**'))
    ) %>%
    tab_footnote(
      footnote  = '\u0394 = change in odds vs. prior week',
      placement = 'left'
    ) %>%

    apply_gt_style()
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 3 – Top Individual Performers
# ══════════════════════════════════════════════════════════════════════════════
make_performers_table <- function(daily_stats, teams, week, season, logo_lookup, top_n = 10) {

  # Reuse the exact same logic as server.R's gt_top
  df_bat <- daily_stats %>%
    filter(in_lineup, batter, matchup_id == week) %>%
    group_by(player_id, team_id, player) %>%
    summarise(n_games   = sum(played),
              n_points  = sum(points), .groups = 'drop') %>%
    filter(n_games > 0) %>%
    mutate(player_url = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
           ppg        = n_points / n_games) %>%
    arrange(-n_points) %>%
    slice(1:top_n) %>%
    left_join(teams %>% select(team_id, team, logo), by = 'team_id') %>%
    mutate(logo = logo_lookup[team]) %>%
    select(player, player_url, team, logo, n_points)

  df_sp <- daily_stats %>%
    filter(in_lineup, start, matchup_id == week) %>%
    group_by(player_id, team_id, player) %>%
    summarise(n_games   = sum(start),
              n_points  = sum(points), .groups = 'drop') %>%
    filter(n_games > 0) %>%
    mutate(player_url = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
           ppg        = n_points / n_games) %>%
    arrange(-ppg, -n_games) %>%
    slice(1:top_n) %>%
    left_join(teams %>% select(team_id, team, logo), by = 'team_id') %>%
    mutate(logo = logo_lookup[team]) %>%
    select(player, player_url, team, logo, n_games, ppg)

  df_rp <- daily_stats %>%
    filter(in_lineup, relief, !relief_start, matchup_id == week) %>%
    group_by(player_id, team_id, player) %>%
    summarise(n_games   = sum(relief),
              n_points  = sum(points), .groups = 'drop') %>%
    filter(n_games > 0) %>%
    mutate(player_url = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>%
    arrange(-n_points) %>%
    slice(1:top_n) %>%
    left_join(teams %>% select(team_id, team, logo), by = 'team_id') %>%
    mutate(logo = logo_lookup[team]) %>%
    select(player, player_url, team, logo, n_games, n_points)

  # Pad to top_n rows
  pad <- function(df, n) {
    if (nrow(df) < n) df[seq(nrow(df) + 1, n), ] <- NA
    df
  }
  df_bat <- pad(df_bat, top_n); names(df_bat) <- paste0(names(df_bat), '_bat')
  df_sp  <- pad(df_sp,  top_n); names(df_sp)  <- paste0(names(df_sp),  '_sp')
  df_rp  <- pad(df_rp,  top_n); names(df_rp)  <- paste0(names(df_rp),  '_rp')

  df_top <- bind_cols(df_bat, df_sp, df_rp)

  gt(df_top) %>%
    cols_align('center') %>%

    tab_spanner(label = 'Batting',          columns = contains('_bat')) %>%
    tab_spanner(label = 'Starting Pitching', columns = contains('_sp')) %>%
    tab_spanner(label = 'Relief Pitching',  columns = contains('_rp')) %>%

    sub_missing(columns = everything(), missing_text = '---') %>%
    fmt_number(columns = c(n_points_bat, ppg_sp, n_points_rp), decimals = 1, sep_mark = '') %>%
    fmt_number(columns = n_games_sp, decimals = 0, sep_mark = '') %>%

    # Player headshots (ESPN CDN)
    text_transform(
      locations = cells_body(columns = contains('player_url')),
      fn = function(x) web_image(url = x, height = 50)
    ) %>%
    # Team logos
    text_transform(
      locations = cells_body(columns = logo_bat, rows = !is.na(logo_bat)),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = logo_sp, rows = !is.na(logo_sp)),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = logo_rp, rows = !is.na(logo_rp)),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%

    # Amber heatmap on score columns
    data_color(
      columns = n_points_bat,
      fn = scales::col_numeric(palette = amber_pal(), domain = NULL),
      autocolor_text = FALSE
    ) %>%
    data_color(
      columns = ppg_sp,
      fn = scales::col_numeric(palette = amber_pal(), domain = NULL),
      autocolor_text = FALSE
    ) %>%
    data_color(
      columns = n_points_rp,
      fn = scales::col_numeric(palette = amber_pal(), domain = NULL),
      autocolor_text = FALSE
    ) %>%

    # Section dividers
    tab_style(
      style     = cell_borders(sides = 'bottom', color = 'black', weight = px(3)),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style     = cell_borders(sides = 'right', color = 'black', weight = px(3)),
      locations = cells_body(columns = c(n_points_bat, n_games_sp, n_points_rp))
    ) %>%

    cols_label(
      player_bat    = 'Player', player_url_bat = '', team_bat  = 'Team', logo_bat = '',
      n_points_bat  = 'Points',
      player_sp     = 'Player', player_url_sp  = '', team_sp   = 'Team', logo_sp  = '',
      n_games_sp    = '# Starts', ppg_sp        = 'Points/Start',
      player_rp     = 'Player', player_url_rp  = '', team_rp   = 'Team', logo_rp  = '',
      n_games_rp    = '# App.',  n_points_rp    = 'Points'
    ) %>%

    tab_header(
      title    = md('**Top Performances**'),
      subtitle = md(glue('**Week {week}  \u2022  {season} Season**'))
    ) %>%

    apply_gt_style()
}

# ══════════════════════════════════════════════════════════════════════════════
# MAIN – Build & Save All Three Tables
# ══════════════════════════════════════════════════════════════════════════════
make_weekly_summary <- function(season = 2026, week) {
  message(glue('Building Week {week} summary for {season}...'))

  # Load data
  teams         <- read_csv(here(glue('data/stats/{season}/teams_{season}.csv')),
                             show_col_types = FALSE)
  team_points   <- read_csv(here(glue('data/stats/{season}/team_points.csv')),
                             show_col_types = FALSE)
  playoff_hist  <- read_csv(here(glue('data/playoff_odds/historical_playoff_odds_{season}.csv')),
                             show_col_types = FALSE)
  exp_standings <- read_csv(here(glue('data/stats/{season}/exp_standings.csv')),
                             show_col_types = FALSE)
  daily_stats   <- read_csv(here(glue('data/stats/{season}/daily_stats_{season}.csv')),
                             show_col_types = FALSE)

  df_week <- team_points %>%
    filter(matchup_id == week, !is.na(total_points))

  if (nrow(df_week) == 0)
    stop(glue('No completed data found for week {week} in season {season}.'))

  logo_lookup <- build_logo_lookup(teams)

  out_dir <- here(glue('figures/weekly_summary/{season}'))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # 1 – Scoreboard
  message('  Building scoreboard...')
  gt_scores <- make_scoreboard_table(df_week, season, week, logo_lookup)
  gtExtras::gtsave_extra(gt_scores,
                         file.path(out_dir, glue('week_{week}_scoreboard.png')),
                         vwidth = 1200, selector = 'table')

  # 2 – Standings + Odds
  message('  Building standings...')
  gt_stand <- make_standings_table(exp_standings, playoff_hist, week, season, logo_lookup)
  gtExtras::gtsave_extra(gt_stand,
                         file.path(out_dir, glue('week_{week}_standings.png')),
                         vwidth = 1600, selector = 'table')

  # 3 – Top Performers
  message('  Building top performers...')
  gt_perf <- make_performers_table(daily_stats, teams, week, season, logo_lookup)
  gtExtras::gtsave_extra(gt_perf,
                         file.path(out_dir, glue('week_{week}_performers.png')),
                         vwidth = 2400, selector = 'table')

  # Stack scoreboard + standings into one combined PNG
  message('  Combining scoreboard + standings...')
  path_scoreboard <- file.path(out_dir, glue('week_{week}_scoreboard.png'))
  path_standings  <- file.path(out_dir, glue('week_{week}_standings.png'))
  path_combined   <- file.path(out_dir, glue('week_{week}_combined.png'))

  img_scores <- magick::image_read(path_scoreboard)
  img_stand  <- magick::image_read(path_standings)

  # Match widths before stacking (scale narrower image up to the wider one)
  w_scores <- magick::image_info(img_scores)$width
  w_stand  <- magick::image_info(img_stand)$width
  target_w <- max(w_scores, w_stand)

  if (w_scores != target_w)
    img_scores <- magick::image_scale(img_scores, glue('{target_w}x'))
  if (w_stand != target_w)
    img_stand  <- magick::image_scale(img_stand,  glue('{target_w}x'))

  magick::image_append(c(img_scores, img_stand), stack = TRUE) %>%
    magick::image_write(path_combined)

  message(glue('  Done! Files saved to {out_dir}'))
  invisible(list(scoreboard = gt_scores, standings = gt_stand, performers = gt_perf))
}
