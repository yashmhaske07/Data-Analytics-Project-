library(data.table)
ScanOnFolderMaster <- "D:/My Projects/MelbDatathon2018/Samp_1/ScanOnTransaction"
ScanOffFolderMaster <- "D:/My Projects/MelbDatathon2018/Samp_1/ScanOffTransaction"
mySamp <- 0
ScanOnFolder <- sub("x",mySamp,ScanOnFolderMaster)
ScanOffFolder <- sub("x",mySamp,ScanOffFolderMaster)

#list the files
onFiles <- list.files(ScanOnFolder,recursive = TRUE,full.names = TRUE)
offFiles <- list.files(ScanOffFolder,recursive = TRUE,full.names = TRUE)
#how many
length(onFiles)
allFiles <- union(onFiles,offFiles)
cat("\nthere are", length(allFiles),'files')
head(data.frame(readLines(onFiles[2])))
AllData <- data.frame()
AllData <- head(textData)
coln <-c('Mode','BusinessDate','DateTime','CardID','CardType','VehicleID','ParentRoute','RouteID','StopID')
colnames(AllData)<-coln
for(i in 51:75)
{
j<-data.frame()
j <-data.frame(readLines(onFiles[i]))
textData <-data.frame(do.call('rbind', strsplit(as.character(j$readLines.onFiles.i..),'|',
                                                fixed=TRUE)))
colnames(textData) <-coln
print(i)
AllData<-rbind(AllData,textData)
}
nrow(AllData)
write.csv(AllData,"D://ScanOnFiles3.csv")


AllData1 <- AllData
#tapOFF
for(i in 126:length(offFiles))
{
  j<-data.frame()
  j <-data.frame(readLines(offFiles[i])) 
  textData <-data.frame(do.call('rbind', strsplit(as.character(j$readLines.offFiles.i..),'|',
                                                    fixed=TRUE)))
  colnames(textData) <-coln
  print(cat("\nRows are", nrow(AllData1),'current row',i,''))
  AllData1<-rbind(AllData1,textData)
}
nrow(AllData1)
write.csv(AllData1,"D:/My Projects/MelbDatathon2018/SortedData/Sample0/TouchOff/sample6.csv")
