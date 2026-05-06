scrape_oh_franklin <- function(path = NULL, timestamp, ...) {
  raw_dir <- fs::path(the$path_dropbox, the$election_folder, "OH", "raw", "Franklin")
  fs::dir_create(raw_dir)
  local_file <- fs::path(raw_dir, glue("OH_franklin_{timestamp}.ASC"))

  .oh_sftp_exec(c(
    paste0('send "get ', path, ' \\"', local_file, '\\"\\r"'),
    'expect "sftp>"'
  ))

  readr::read_fwf(
    local_file,
    col_positions = readr::fwf_positions(
      start = c(1, 5, 8, 12, 18, 24, 30, 36, 102, 112, 168, 206, 261),
      end = c(4, 7, 11, 17, 23, 29, 35, 41, 104, 167, 205, 235, 262),
      col_names = c(
        "contest_num", "candidate_num", "precinct_code",
        "votes_total", "votes_mail", "votes_early", "votes_eday", "votes_prov",
        "party", "contest_title", "candidate_name", "precinct_name", "vote_for"
      )
    ),
    col_types = readr::cols(
      contest_num   = readr::col_integer(),
      candidate_num = readr::col_integer(),
      precinct_code = readr::col_integer(),
      votes_total   = readr::col_integer(),
      votes_mail    = readr::col_integer(),
      votes_early   = readr::col_integer(),
      votes_eday    = readr::col_integer(),
      votes_prov    = readr::col_integer(),
      vote_for      = readr::col_integer(),
      .default      = readr::col_character()
    ),
    show_col_types = FALSE
  ) |>
    mutate(across(where(is.character), stringr::str_trim)) |>
    filter(
      str_detect(contest_title, "US Senator|United States Senator|U\\.S\\. Senator") |
        (str_detect(contest_title, "Governor") & !str_detect(contest_title, "^Lieutenant")) |
        str_detect(contest_title, "Justice of the Supreme Court") |
        str_detect(contest_title, "Representative.*(\\b01\\b|\\b1st\\b|\\b13\\b|\\b13th\\b)"),
      nzchar(party)
    ) |>
    pivot_longer(
      c(votes_mail, votes_early, votes_eday, votes_prov),
      names_to = "method",
      values_to = "votes"
    ) |>
    mutate(
      method = str_remove(method, "^votes_"),
      state = "OH",
      time = .env$timestamp,
      geo = paste("oh", "025", tolower(precinct_name), sep = "_"),
      election = ymd(the$election_date),
      candidate = tolower(candidate_name) |> find_oh_candidate(),
      contest = find_oh_contest(contest_title, candidate),
      party = str_extract(contest, "dem$|rep$|lib$"),
      district = find_oh_district(candidate),
      jurisdiction = find_oh_jurisdiction(contest)
    ) |>
    mutate(
      stage = case_when(
        all(is.na(votes)) ~ 0L,
        max(votes, na.rm = TRUE) > 0 ~ 6L,
        .default = 0L
      ),
      .by = geo
    ) |>
    select(any_of(the$columns)) |>
    filter(!is.na(party))
}