PATH_GDRIVE = ""
PATH_CBS_S3 = "cbsn-elections-external-models"
PATH_LOCAL = ""
ELECTION_DATE = "2024_general"
PATH_DROPBOX = ""

local({
  
  user = Sys.info()["user"]
  
  if (user == "josephloffredo"){
    PATH_DROPBOX <<- "~/Dropbox (MIT)/2024 Election Results"
  } else if (user == "mason") {
    PATH_DROPBOX <<- "~/Dropbox (MIT)/Research/2024 Election Results"
  }
  
})
