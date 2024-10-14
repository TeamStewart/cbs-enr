# Convert a state abbreviation to its full name
lookup_state_name <- function(abbreviation) {
  state.name[match(abbreviation, state.abb)]
}

get_gsheet <- function(sheet) {
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/10MtdPvXsqA4ddjhrXgHA2TEYH5AtW1-dmvS1Bv7TgEk/edit?gid=0#gid=0",
    sheet = sheet
  )
}