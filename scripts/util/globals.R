PATH_GDRIVE = "https://drive.google.com/drive/folders/1741SR-N8X01Pao_soiwwqCjuMFwAgo5k"
PATH_CBS_S3 = "cbsn-elections-external-models"
PATH_LOCAL = ""
ELECTION_DATE = "20241105"
PATH_DROPBOX = ""

local({
  
  user = Sys.info()["user"]
  
  if (user == "josephloffredo"){
    PATH_DROPBOX <<- "~/Dropbox (MIT)/2024 Election Results"
  } else if (user == "mason") {
    PATH_DROPBOX <<- "~/Dropbox (MIT)/Research/2024 Election Results"
  }
  
})
