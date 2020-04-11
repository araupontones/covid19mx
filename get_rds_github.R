


tempdir = tempdir()
rdsfile=file.path(tempdir, "data.rds")

#open connection to write data in download folder
filecon <- file(zipfile, "wb") 
#write data contents to download file!! change unzip folder to temporary file when in shiny
writeBin(RawData$content, filecon) 
#close the connection
close(filecon)

t = readRDS(zipfile)


get_rds <- function(url, rds_file) {
  
  RawData <- GET(url) #Sucess!!
  
  
  tempdir = tempdir()
  rdsfile=file.path(tempdir, rds_file)
  
  #open connection to write data in download folder
  filecon <- file(rdsfile, "wb") 
  #write data contents to download file!!
  writeBin(RawData$content, filecon) 
  #close the connection
  close(filecon)
  
  t = readRDS(rdsfile)
  
  return(t)
  
  
}


#get data
url ="https://github.com/araupontones/covid19mx/blob/master/app/diasData.rds?raw=true"
rds_file = "diasData.rds"

midata = get_rds(url = url, rds_file = "diasData.rds")
