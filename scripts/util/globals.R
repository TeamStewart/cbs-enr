PATH_GDRIVE = "https://drive.google.com/drive/u/0/folders/1ByjqU2Cq-qnBR8NYpC6r_JOE8CB1hJnL"
PATH_CBS_S3 = "cbsn-elections-external-models"
PATH_LOCAL = ""
ELECTION_DATE = "20260526"
ELECTION_FOLDER = "26_primaries"
PATH_DROPBOX = ""
PATH_ENIGHTDATA = ""

local({
  user = Sys.info()["user"]
  
  if (user == "josephloffredo"){
    PATH_DROPBOX <<- "~/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data"
    PATH_ENIGHTDATA <<- "/Users/josephloffredo/Desktop/GitHub/enightdata"
  } else if (user == "mason") {
    PATH_DROPBOX <<- "~/Dropbox (MIT)/Research/CBS-MIT Election Data"
    PATH_ENIGHTDATA <<- "~/Documents/github/enightdata"
  } else if (user == "stewart") {
    PATH_DROPBOX <<- "/Users/stewart/MIT Dropbox/Mason Reece/Research/CBS-MIT Election Data"
    PATH_ENIGHTDATA <<- "~/Documents/enightdata"
  } else {
    cli::cli_abort("User {user} not recognized, cannot set PATH_DROPBOX or PATH_ENIGHTDATA")
  }
  
})
