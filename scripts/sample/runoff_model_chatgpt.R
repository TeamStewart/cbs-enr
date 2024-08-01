# Analysis of Georgia runoff, June 21, 2022

# Set working directory and file paths
primary_data <- "scripts/sample/ga_allraces.csv"
runoff_data <- "scripts/sample/ga_runoff_allraces.csv"

# Load necessary libraries
library(tidyverse)

# Map candidate names to last names
candidates_df <- data.frame(
  candidates = c("ERICK E. ALLEN", "CHARLIE BAILEY", "RAPHAEL BAKER", "TIMOTHY BARR",
                 "WILLIAM ''WILL'' BODDIE, JR.", "TYRONE BROOKS, JR.", "PAUL BROUN",
                 "TONY BROWN", "VIVIAN L. CHILDS", "MIKE COLLINS", "MICHAEL CORBIN",
                 "DAVID CURRY", "DEE DAWKINS-HAIGLER", "THOMAS DEAN", "JOHN EAVES",
                 "JAKE EVANS", "JESSICA ALLISON FORE", "BYRON GATEWOOD", "MARK GONSALVES",
                 "FLOYD GRIFFIN", "JOYCE MARIE GRIGGS", "KWANZA HALL", "MEAGAN HANSON",
                 "BLAKE HARBIN", "PHYLLIS HATCHER", "JASON T. HAYES", "WADE HERRING",
                 "NICOLE HORN", "JEREMY HUNT", "DERRICK L. JACKSON", "LESTER G. JACKSON III",
                 "TABITHA JOHNSON-GREEN", "WAYNE JOHNSON", "VERNON J. JONES", "R. MALIK",
                 "RICH MCCORMICK", "LISA MCCOY", "MARC MCMAIN", "MICHELLE L. MUNROE",
                 "BEE NGUYEN", "YG NYGHTSTORM", "FEMI ODUWOLE", "MICHAEL OWENS",
                 "RICH ROBERTSON", "JANICE LAWS ROBINSON", "RENITTA SHANNON", "ALAN SIMS",
                 "PAULETTE SMITH", "MALLORY STAPLES", "NADIA SURRENCY", "MITCHELL SWAN",
                 "SUZI VOYLES", "PAUL WALTON", "CHRIS WEST", "MARY WEST", "PAUL WHITEHEAD",
                 "MATTHEW WILSON", "EUGENE YU"),
  lastname = c("ALLEN", "BAILEY", "BAKER", "BARR", "BODDIE", "BROOKS", "BROUN", "BROWN",
               "CHILDS", "COLLINS", "CORBIN", "CURRY", "DAWKINS", "DEAN", "EAVES", "EVANS",
               "FORE", "GATEWOOD", "GONSALVES", "GRIFFIN", "GRIGGS", "HALL", "HANSON",
               "HARBIN", "HATCHER", "HAYES", "HERRING", "HORN", "HUNT", "JACKSON_D",
               "JACKSON_J", "JOHNSON_T", "JOHNSON_W", "JONES", "MALIK", "MCCORMICK",
               "MCCOY", "MCMAIN", "MUNROE", "NGUYEN", "NYGHTSTORM", "ODUWOLE", "OWENS",
               "ROBERTSON", "ROBINSON", "SHANNON", "SIMS", "SMITH", "STAPLES", "SURRENCY",
               "SWAN", "VOYLES", "WALTON", "WEST_C", "WEST_M", "WHITEHEAD", "WILSON", "YU")
)

# Save candidates dataframe to file (optional)
write.csv(candidates_df, file = "scripts/sample/candidates_lastname.csv", row.names = FALSE)

# Read in primary data
primary_data_df <- read_csv(primary_data, locale = locale(encoding = "UTF-8"))

# Preprocess primary data
primary_data_df <- primary_data_df %>%
  mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "UTF-8", sub = ""))) %>%
  mutate(across(everything(), ~ toupper(trimws(.)))) %>%
  mutate(race = case_when(
    str_detect(race, "LIEUTENANT|LIUTENANT") & party == "DEM" ~ "LT_GOV",
    str_detect(race, "SECRETARY OF STATE") & party == "DEM" ~ "SOS",
    str_detect(race, "INSURANCE") & party == "DEM" ~ "INSUR",
    str_detect(race, "LABOR") & party == "DEM" ~ "LABOR",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 1 ") & party == "DEM" ~ "CD1",
    (str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 2 ") & party == "REP") |
      (str_detect(race, "US HOUSE") & str_detect(race, "DIST 2") & party == "REP") ~ "CD2",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & (str_detect(race, "DISTRICT 6 ") | str_detect(race, "DISTRICT 6/")) & party == "REP" ~ "CD6",
    str_detect(race, "US HOUSE") & str_detect(race, "DIST 6") & party == "REP" ~ "CD6",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & (str_detect(race, "DISTRICT 7 ") | str_detect(race, "DISTRICT 7/")) & party == "REP" ~ "CD7",
    str_detect(race, "US HOUSE") & str_detect(race, "DIST 7") & party == "REP" ~ "CD7",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 10") & str_detect(race, " - DEM") & party == "DEM" ~ "CD10D",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 10") & str_detect(race, " - REP") & party == "REP" ~ "CD10R",
    TRUE ~ race
  )) %>%
  filter(race %in% c("LT_GOV", "SOS", "INSUR", "LABOR", "CD1", "CD2", "CD6", "CD7", "CD10D", "CD10R")) %>%
  filter(precinct != "-1") %>%
  mutate(candidates = str_replace(candidates, "TABITHA JOHNSON- GREEN", "TABITHA JOHNSON-GREEN"),
         precinct = case_when(
           county == "CHARLTON" & precinct == "WINKOUR" ~ "WINOKUR",
           county == "CHATHAM" & str_detect(precinct, "1-05C ") ~ "1-05C",
           county == "CHATHAM" & str_detect(precinct, "1-16C ") ~ "1-16C",
           county == "CHATHAM" & str_detect(precinct, "7-11C ") ~ "7-11C",
           county == "FULTON" & precinct == "1.10E+02" ~ "11E2",
           county == "FULTON" & precinct == "1.10E+03" ~ "11E3",
           county == "FULTON" & precinct == "1.10E+04" ~ "11E4",
           county == "FULTON" & precinct == "1.20E+02" ~ "12E2",
           TRUE ~ precinct
         ))

# Save processed primary data
primary_data_df <- primary_data_df %>%
  mutate(votes_primary = as.numeric(votes)) %>%
  select(race, candidates, county, precinct, type, votes_primary)
write.csv(primary_data_df, file = "scripts/sample/primary_votes.csv", row.names = FALSE)

# Read in runoff data
runoff_data_df <- read_csv(runoff_data)

# Preprocess runoff data
runoff_data_df <- runoff_data_df %>% 
  mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "UTF-8", sub = ""))) %>%
  mutate(across(everything(), ~ toupper(trimws(.)))) %>%
  mutate(race = case_when(
    str_detect(race, "LIEUTENANT") ~ "LT_GOV",
    str_detect(race, "SECRETARY OF STATE") ~ "SOS",
    str_detect(race, "INSURANCE") ~ "INSUR",
    str_detect(race, "LABOR") ~ "LABOR",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 1 ") ~ "CD1",
    (str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 2 ")) |
      (str_detect(race, " US HOUSE") & str_detect(race, "DIST 2")) ~ "CD2",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & (str_detect(race, "DISTRICT 6 ") | str_detect(race, "DISTRICT 6/")) ~ "CD6",
    str_detect(race, "US HOUSE") & str_detect(race, "DIST 6") ~ "CD6",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & (str_detect(race, "DISTRICT 7 ") | str_detect(race, "DISTRICT 7/")) ~ "CD7",
    str_detect(race, "US HOUSE") & str_detect(race, "DIST 7") & party == "REP" ~ "CD7",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 10") & str_detect(race, " - DEM") ~ "CD10D",
    str_detect(race, "US HOUSE OF REPRESENTATIVES") & str_detect(race, "DISTRICT 10") & str_detect(race, " - REP") ~ "CD10R",
    TRUE ~ race
  )) %>%
  filter(race %in% c("LT_GOV", "SOS", "INSUR", "LABOR", "CD1", "CD2", "CD6", "CD7", "CD10D", "CD10R")) %>%
  filter(precinct != "-1") %>%
  mutate(candidates = str_replace(candidates, "TABITHA JOHNSON- GREEN", "TABITHA JOHNSON-GREEN"),
         candidates = str_replace(candidates, "VERNON JONES", "VERNON J. JONES")) %>%
  mutate(precinct = case_when(
           county == "CHARLTON" & precinct == "WINKOUR" ~ "WINOKUR",
           county == "CHATHAM" & str_detect(precinct, "1-05C ") ~ "1-05C",
           county == "CHATHAM" & str_detect(precinct, "1-16C ") ~ "1-16C",
           county == "CHATHAM" & str_detect(precinct, "7-11C ") ~ "7-11C",
           TRUE ~ precinct
         ))

# Save processed runoff data
runoff_data_df <- runoff_data_df %>%
  mutate(votes_runoff = as.numeric(votes)) %>%
  select(race, candidates, county, precinct, type, votes_runoff)
write.csv(runoff_data_df, file = "scripts/sample/runoff_votes.csv", row.names = FALSE)

# Merge the primary dataset with the last names and produce vote shares
primary_data_df <- primary_data_df %>%
  left_join(candidates_df, by = "candidates") %>%
  group_by(county, precinct, race, type) %>%
  mutate(tvotes = sum(votes_primary)) %>%
  ungroup() %>%
  mutate(pct = votes_primary / tvotes) %>%
  select(race, county, precinct, type, tvotes, pct, lastname) %>%
  rename(ptvotes = tvotes, ppct = pct) %>%
  pivot_wider(names_from = lastname, values_from = ppct, names_prefix = "ppct_")

# Save primary vote shares
write.csv(primary_data_df, file = "scripts/sample/primary_pct.csv", row.names = FALSE)

# Merge the runoff dataset with the last names and produce vote shares
runoff_data_df <- runoff_data_df %>%
  left_join(candidates_df, by = "candidates") %>%
  filter(!is.na(lastname)) %>%
  group_by(county, precinct, race, type) %>%
  mutate(tvotes = sum(votes_runoff)) %>%
  ungroup() %>%
  mutate(pct = votes_runoff / tvotes) %>%
  select(race, county, precinct, type, tvotes, pct, lastname) %>%
  rename(rtvotes = tvotes, rpct = pct) %>%
  pivot_wider(names_from = lastname, values_from = rpct, names_prefix = "rpct_")

# Save runoff vote shares
write.csv(runoff_data_df, file = "scripts/sample/runoff_pct.csv", row.names = FALSE)

# Merge the two previous datasets
final_df <- primary_data_df %>%
  left_join(runoff_data_df, by = c("race", "county", "precinct", "type"))

# Save final merged dataset
write.csv(final_df, file = "scripts/sample/final_merged.csv", row.names = FALSE)

# Regressions
# Assuming 'ADVANCED VOTING' is a type in the dataset and runiform() for testing purpose
set.seed(123)
final_df <- final_df %>%
  mutate(across(starts_with("rpct"), ~ ifelse(type == "ADVANCED VOTING", runif(n()), .)),
         ptvotes = ifelse(type == "ADVANCED VOTING", runif(n(), 0, 1000), ptvotes),
         rtvotes = ifelse(type == "ADVANCED VOTING", runif(n(), 0, 1000), rtvotes))

# Define regression and prediction function
run_regression <- function(data, dependent, independents, weight_var, filter_cond) {
  formula <- as.formula(paste(dependent, "~", paste(independents, collapse = " + ")))
  model <- lm(formula, data = data, weights = data[[weight_var]], subset = eval(parse(text = filter_cond)))
  data$py <- predict(model, newdata = data)
  data
}

# Run regressions and predictions
final_df <- run_regression(final_df, "rpctHUNT", c("ppctCHILDS", "ppctHUNT", "ppctJOHNSON_W", "ppctROBERTSON", "ppctWEST_C"), "rtvotes", "type == 'ADVANCED VOTING' & race == 'CD2'")
final_df <- final_df %>%
  mutate(estHUNT = ifelse(race == "CD2" & type == "ADVANCED VOTING", ifelse(is.na(rpctHUNT), py, rpctHUNT), estHUNT))

final_df <- run_regression(final_df, "rpctMCCORMICK", c("ppctEVANS", "ppctGATEWOOD", "ppctHANSON", "ppctHARBIN", "ppctMCCORMICK", "ppctSMITH", "ppctSTAPLES", "ppctYU"), "rtvotes", "type == 'ADVANCED VOTING' & race == 'CD6'")
final_df <- final_df %>%
  mutate(estMCCORMICK = ifelse(race == "CD6" & type == "ADVANCED VOTING", ifelse(is.na(rpctMCCORMICK), py, rpctMCCORMICK), estMCCORMICK))

final_df <- run_regression(final_df, "rpctCOLLINS", c("ppctBARR", "ppctBROUN", "ppctCOLLINS", "ppctCURRY", "ppctJONES", "ppctMCMAIN", "ppctSIMS", "ppctSWAN"), "rtvotes", "type == 'ADVANCED VOTING' & race == 'CD10R'")
final_df <- final_df %>%
  mutate(estCOLLINS = ifelse(race == "CD10R" & type == "ADVANCED VOTING", ifelse(is.na(rpctCOLLINS), py, rpctCOLLINS), estCOLLINS))

final_df <- run_regression(final_df, "rpctHALL", c("ppctALLEN", "ppctBAILEY", "ppctBROOKS", "ppctBROWN", "ppctHALL", "ppctHAYES", "ppctJACKSON_D", "ppctSHANNON"), "rtvotes", "type == 'ADVANCED VOTING' & race == 'LT_GOV'")
final_df <- final_df %>%
  mutate(estHALL = ifelse(race == "LT_GOV" & type == "ADVANCED VOTING", ifelse(is.na(rpctHALL), py, rpctHALL), estHALL))

# Turnout regressions
turnout_regression <- function(data, race, weight_var) {
  model <- lm(rtvotes ~ ptvotes, data = data, weights = data[[weight_var]], subset = type == "ADVANCED VOTING" & race == race)
  data$py <- predict(model, newdata = data)
  data
}

final_df <- turnout_regression(final_df, "CD2", "rtvotes")
final_df <- final_df %>%
  mutate(est_turnout_CD2 = ifelse(race == "CD2" & type == "ADVANCED VOTING", ifelse(is.na(rtvotes), py, rtvotes), est_turnout_CD2))

final_df <- turnout_regression(final_df, "CD6", "rtvotes")
final_df <- final_df %>%
  mutate(est_turnout_CD6 = ifelse(race == "CD6" & type == "ADVANCED VOTING", ifelse(is.na(rtvotes), py, rtvotes), est_turnout_CD6))

final_df <- turnout_regression(final_df, "CD10R", "rtvotes")
final_df <- final_df %>%
  mutate(est_turnout_CD10R = ifelse(race == "CD10R" & type == "ADVANCED VOTING", ifelse(is.na(rtvotes), py, rtvotes), est_turnout_CD10R))

final_df <- turnout_regression(final_df, "LT_GOV", "rtvotes")
final_df <- final_df %>%
  mutate(est_turnout_LT_GOV = ifelse(race == "LT_GOV" & type == "ADVANCED VOTING", ifelse(is.na(rtvotes), py, rtvotes), est_turnout_LT_GOV))

# Multiply out the estimates
final_df <- final_df %>%
  mutate(est_votesHUNT = estHUNT * est_turnout_CD2,
         est_votesMCCORMICK = estMCCORMICK * est_turnout_CD6,
         est_votesCOLLINS = estCOLLINS * est_turnout_CD10R,
         est_votesHALL = estHALL * est_turnout_LT_GOV)

# Save final dataframe
write.csv(final_df, file.path(dir, "scratch/final_estimates.csv"), row.names = FALSE)
