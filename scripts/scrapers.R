## New York City (labeled as New York but get over it)
scrape_ny <- function(state, county, path, timestamp) {
  # start with the "Total" page
  base = read_html(path)

  links = tibble(
    link = base |> html_elements("a") |> html_attr("href"),
    ad = base |> html_elements("a") |> html_attr("title")
  ) |>
    filter(str_detect(ad, "^AD \\d+$")) |>
    mutate(
      link = glue("https://enr.boenyc.gov/{link}")
    )

  get_ad <- function(link) {
    t = read_html(link) |>
      html_element(".underline") |>
      html_table(head = TRUE) |>
      janitor::clean_names(case = "title")

    t[-1, ]
  }

  county_to_ad <- tribble(
    ~jurisdiction, ~ad,
    "New York", c(37, 61, 65:76),
    "Bronx", c(77:87),
    "Kings", c(41:61, 64),
    "Queens", c(23:40),
    "Richmond", c(61:64)
  ) |>
    unnest_longer(ad) |> 
    mutate(
      ad = as.character(ad)
    )

  links |>
    mutate(
      results = map(link, get_ad)
    ) |>
    select(ad, results) |>
    unnest_longer(results) |>
    unnest_wider(results) |>
    rename(
      ed = X,
      pct_reported = X_2
    ) |>
    mutate(
      pct_reported = str_remove(pct_reported, "%") |> as.numeric() / 100,
      ed = str_squish(ed),
    ) |>
    select(-matches("^X_\\d+$")) |>
    pivot_longer(
      cols = -c(ad, ed, pct_reported),
      names_to = "candidate_name",
      values_to = "precinct_total"
    ) |>
    mutate(
      candidate_name = case_when(
        .default = candidate_name,
        str_detect(candidate_name, regex("Mamdani", TRUE)) ~ "Zohran Mamdani",
        str_detect(candidate_name, regex("Cuomo", TRUE)) ~ "Andrew Cuomo",
        str_detect(candidate_name, regex("Adams", TRUE)) ~ "Eric Adams",
        str_detect(candidate_name, regex("Walden", TRUE)) ~ "Jim Walden",
        str_detect(candidate_name, regex("Sliwa", TRUE)) ~ "Curtis Sliwa",
        str_detect(candidate_name, regex("Walden", TRUE)) ~ "Jim Walden",
        str_detect(candidate_name, regex("Estrada", TRUE)) ~ "Irene Estrada",
        str_detect(candidate_name, regex("Hernandez", TRUE)) ~ "Joseph Hernandez"
      ),
      candidate_party = case_match(
        candidate_name,
        "Zohran Mamdani" ~ "Democrat",
        "Andrew Cuomo" ~ "Fight and Deliver",
        "Eric Adams" ~ "Safe&Affordable/EndAntiSemitism",
        "Jim Walden" ~ "Integrity",
        "Joseph Hernandez" ~ "Quality of Life",
        "Irene Estrada" ~ "Conservative",
        "Curtis Sliwa" ~ "Republican"
      ),
      state = "NY",
      race_name = "Mayor",
      vote_mode = "Total",
      ad = str_remove(ad, "AD "),
      ed = str_remove(ed, "ED "),
      precinct_total = as.numeric(precinct_total),
      precinct_total = ifelse(is.na(precinct_total), 0, precinct_total),
      precinct_id = paste(ad, str_pad(ed, side = "left", width = 3, pad = "0"), sep = "_")
    ) |>
    left_join(
      county_to_ad,
      join_by(ad)
    ) |> 
    # amend fusion voting issues
    summarize(
      precinct_total = sum(precinct_total, na.rm=TRUE),
      .by = c(state, jurisdiction, precinct_id, ad, ed, race_name, candidate_name, candidate_party, vote_mode, pct_reported)
    ) |> 
    mutate(
      virtual_precinct = FALSE,
      timestamp = ymd_hms(.env$timestamp),
      race_id = NA,
      magnitude = 1,
      pct_reported
    )
}

## Virginia
scrape_va <- function(state, county, path, timestamp) {
  # return(read_csv(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/input_data/VA/test_files/VA_2025_11_03_22_38_00.csv")))

  # Download the raw json
  raw_file_path = glue('{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/{state}_{timestamp}.json')
  download.file(path, destfile = raw_file_path)

  base = tibble(data = read_json(raw_file_path) |> pluck('localResults')) |>
    hoist(
      data,
      jurisdiction = "name",
      items = "ballotItems"
    ) |>
    select(-data) |>
    unnest_longer(items)

  base |>
    hoist(
      items,
      race_id = "id",
      race_name = "name",
      options = "ballotOptions",
      magnitude = "voteFor"
    ) |>
    select(-items) |> 
    unnest_longer(options) |>
    hoist(
      options,
      candidate_name = "name",
      candidate_party = "politicalParty",
      precinct_results = "precinctResults"
    ) |>
    select(-options) |>
    unnest_longer(precinct_results) |>
    hoist(
      precinct_results,
      precinct_id = "name",
      groupResults = "groupResults",
      virtual_precinct_pct = "isVirtual",
      precinct_total_pct = "voteCount",
      # progress, from the 2024 Georgia file, can take on the following values:
      # Not Reported
      # Fully Reported
      # Election Night Complete
      # Partially Reported
      progress = "reportingStatus"
    ) |>
    mutate(
      groupResults = case_when(
        is.na(groupResults) ~ list(list(list(groupName = character(), voteCount = integer(), isFromVirtualPrecinct = logical()))),
        .default = groupResults
      )
    ) |>
    select(-precinct_results) |>
    unnest_longer(groupResults) |>
    hoist(
      groupResults,
      vote_mode = "groupName",
      precinct_total = "voteCount",
      virtual_precinct = "isFromVirtualPrecinct"
    ) |>
    mutate(
      timestamp = .env$timestamp |> ymd_hms(),
      state = .env$state,
      precinct_id = str_remove(precinct_id, "\\(.*?\\)$") |> str_replace(" - ", "-") |> str_squish(),
      jurisdiction = ifelse(jurisdiction == "KING & QUEEN COUNTY", "KING AND QUEEN COUNTY", jurisdiction),
      # jurisdiction = jurisdiction |> str_remove(regex("County$", ignore_case = TRUE)) |> str_squish() |> str_to_title(),
      race_name = case_when(
        str_detect(race_name, regex("Lieutenant Governor", ignore_case = TRUE)) ~ "Lt Governor",
        str_detect(race_name, regex("House of Delegates", ignore_case = TRUE)) ~ paste("State House", str_extract(race_name, "\\d+")),
        .default = race_name
      ),
      candidate_party = case_when(
        str_detect(candidate_party, regex("Democrat|Dem", ignore_case = TRUE)) ~ "Democrat",
        str_detect(candidate_party, regex("Repub|Rep", ignore_case = TRUE)) ~ "Republican",
        str_detect(candidate_party, regex("Liber|Lib", ignore_case = TRUE)) ~ "Libertarian",
        str_detect(candidate_party, regex("Green|Grn", ignore_case = TRUE)) ~ "Green",
        str_detect(candidate_party, regex("Ind|Independent", ignore_case = TRUE)) ~ "Independent",
        .default = "Other"
      ),
      # Recode candidate names
      candidate_name = case_when(
        race_name == "Governor" & str_detect(candidate_name, regex("Spanberger", TRUE)) ~ "Abigal Spanberger",
        race_name == "Governor" & str_detect(candidate_name, regex("Winsome", TRUE)) ~ "Winsome Earle-Sears",
        race_name == "Attorney General" & str_detect(candidate_name, regex("Jones", TRUE)) ~ "Jay Jones",
        race_name == "Attorney General" & str_detect(candidate_name, regex("Miyares", TRUE)) ~ "Jason Miyares",
        candidate_name == "Write-In" ~ "Write-ins",
        .default = candidate_name
      ),
      vote_mode = case_when(
        str_detect(vote_mode, regex("Election Day", ignore_case = TRUE)) ~ "Election Day",
        str_detect(vote_mode, regex("Early Voting|Advanced Voting", ignore_case = TRUE)) ~ "Early Voting",
        str_detect(vote_mode, regex("Mail|Absentee|Post-Election", ignore_case = TRUE)) ~ "Absentee/Mail",
        str_detect(vote_mode, regex("Provisional", ignore_case = TRUE)) ~ "Provisional",
        .default = str_to_title(vote_mode)
      )
    ) |>
    # because of the combined vote modes between Mail and Post Voting
    summarize(
      precinct_total = sum(precinct_total, na.rm = TRUE),
      .by = -precinct_total
    ) |> 
    select(
      state,
      race_id,
      race_name,
      candidate_name,
      candidate_party,
      magnitude,
      jurisdiction,
      precinct_id,
      progress,
      virtual_precinct,
      timestamp,
      vote_mode,
      precinct_total
    ) |>
    arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
}
