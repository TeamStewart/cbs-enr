PATH_GDRIVE = "https://drive.google.com/drive/folders/1741SR-N8X01Pao_soiwwqCjuMFwAgo5k"
PATH_CBS_S3 = "cbsn-elections-external-models"
PATH_LOCAL = ""
ELECTION_DATE = "20251105"
ELECTION_FOLDER = "25_general"
PATH_DROPBOX = ""

local({
  
  user = Sys.info()["user"]
  
  if (user == "josephloffredo"){
    PATH_DROPBOX <<- "~/Dropbox (MIT)/CBS-MIT Election Data"
  } else if (user == "mason") {
    PATH_DROPBOX <<- "~/Dropbox (MIT)/Research/CBS-MIT Election Data"
  } else if (user == "Mason") {
    PATH_DROPBOX <<- "C:/Users/Mason/Dropbox (MIT)/Research/CBS-MIT Election Data"
  }
  
})
