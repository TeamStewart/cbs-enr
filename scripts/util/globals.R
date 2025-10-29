PATH_GDRIVE = "https://drive.google.com/drive/u/0/folders/1ByjqU2Cq-qnBR8NYpC6r_JOE8CB1hJnL"
PATH_CBS_S3 = "cbsn-elections-external-models"
PATH_LOCAL = ""
ELECTION_DATE = "20251104"
ELECTION_FOLDER = "25_general"
PATH_DROPBOX = ""

local({
  
  user = Sys.info()["user"]
  
  if (user == "josephloffredo"){
    PATH_DROPBOX <<- "~/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data"
  } else if (user == "mason") {
    PATH_DROPBOX <<- "~/Dropbox (MIT)/Research/CBS-MIT Election Data"
  } else if (user == "Mason") {
    PATH_DROPBOX <<- "C:/Users/Mason/Dropbox (MIT)/Research/CBS-MIT Election Data"
  }
  
})
