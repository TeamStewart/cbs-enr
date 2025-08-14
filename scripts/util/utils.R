# Convert a state abbreviation to its full name
lookup_state_name <- function(abbreviation) {
  state.name[match(abbreviation, state.abb)]
}

get_gsheet <- function(spreadsheet, sheet, ...) {
  googlesheets4::read_sheet(
    ss = spreadsheet,
    sheet = sheet,
    ...
  )
}