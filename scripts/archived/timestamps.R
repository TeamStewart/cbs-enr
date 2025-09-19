## ARCHIVED from 2024 general election
az_timestamp <- function() {
  if (county == "Maricopa") {
    read_html("https://elections.maricopa.gov/results-and-data/election-results.html#ElectionResultsSearch") |>
      html_element(":nth-child(8) small") |>
      html_text2() |>
      str_extract("\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} [AP]M") |>
      mdy_hms(tz = "America/Phoenix") |>
      with_tz(tzone = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  } else if (county == "Pima") {
    now() |> format("%Y_%m_%d_%H_%M_%S", tz = "UTC")
  }
}

## ARCHIVED from 2024 general election
ga_timestamp <- function(){
  read_html("https://app.enhancedvoting.com/results/public/api/elections/Georgia/2024NovGen") |>
    html_text() |>
    fromJSON() |>
    pluck("lastUpdated") |>
    ymd_hms() |>
    floor_date(unit = "second") |>
    with_tz(tzone = "America/New_York") |>
    str_replace_all("-|:| ", "_")
}

## ARCHIVED from 2024 general election
mi_timestamp <- function(){
  if(county %in% c("Eaton", "Oakland", "Macomb")){
    clarity_timestamp()
  } else if(county == "Ingham"){
    read_html("https://app.enhancedvoting.com/results/public/api/elections/ingham-county-mi/general11052024") |>
      html_text() |>
      fromJSON() |>
      pluck("lastUpdated") |>
      ymd_hms() |>
      floor_date(unit = "second") |>
      with_tz(tzone = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }
}

## ARCHIVED from 2024 general election
nc_timestamp <- function() {
  read_xml("https://s3.amazonaws.com/dl.ncsbe.gov?delimiter=/&prefix=ENRS/2024_11_05/") |>
    xml2::as_list() |>
    pluck("ListBucketResult", function(x) x[names(x) == "Contents"]) |>
    map(
      ~ tibble(
        Key = pluck(.x, "Key", 1),
        LastModified = pluck(.x, "LastModified", 1)
      )
    ) |>
    list_rbind() |>
    filter(Key == "ENRS/2024_11_05/results_pct_20241105.zip") |>
    pull(LastModified) |>
    ymd_hms(tz = "America/New_York") |>
    str_replace_all("-|:| ", "_")
}

## ARCHIVED from 2024 general election
pa_timestamp <- function() {
  if (county %in% c("Allegheny","Delaware")) {
    clarity_timestamp()
  } else if (county == "Philadelphia") {
    read_html("https://philadelphiaresults.azurewebsites.us/ResultsExport.aspx?") |>
      html_element(".turnout-last-updated-time") |>
      html_text() |>
      mdy_hms(tz = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }
}