TidyData <- function()
  
{
  
  #Utils: function add suffix 
  addSuffix<- function(x, suffix) { 
    if (!(x %in% c("Subject","Activity"))) { 
      paste(x,suffix, sep="") 
    } 
    else{ 
      x 
    } 
  } 
  
  setDirectory <- function()
  {
    require(plyr) 
    pathfile<-file.path(getwd(),   "data/UCI HAR Dataset" ) 
    if(!file.exists("./data")){dir.create("./data")}
    if(!file.exists("./data/Dataset.zip"))
      {
      fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(fileUrl,destfile="./data/Dataset.zip")
      }
    
    ###Unzip DataSet to /data directory
    unzip(zipfile="./data/Dataset.zip",exdir="./data")
    
    #Get data 
    
    pathfiletest <-file.path(pathfile, "test") 
    pathfiletrain <-file.path(pathfile, "train") 
    dirPath <- c (pathfiletest, pathfiletrain , pathfile)
    dirPath
    
  }
  
  loadXTestData <- function(dirPathParam)
  {
    xtest<-read.table(file.path(dirPathParam[1],"X_test.txt")) 
    ##print(nrow(xtest))
    ## print(nrow(subjecttest))
    return (xtest)
  }
  
  loadYTestData <- function(dirPathParam)
  {
    
    ytest<-read.table(file.path(dirPathParam[1],"Y_test.txt")) 
    ## print(nrow(ytest))
    return (ytest)
  }  
  
  loadSubjectTestData <- function (dirPathParam)
  {
    
    subjecttest<-read.table(file.path(dirPathParam[1],"subject_test.txt")) 
    ## print(nrow(subjecttest))
    return (subjecttest)
  }
  
  loadXTrainData <- function (dirPathParam)
  {
    
    xtrain<-read.table(file.path(dirPathParam[2],"X_train.txt")) 
    ## print(nrow(xtrain))
    return (xtrain)   
  }
  
  loadYTrainData <- function(dirPathParam)
  {
    ytrain<-read.table(file.path(dirPathParam[2],"Y_train.txt"))
    ## head(xtrain,2)
    ## head(ytrain ,2)
    ## head(subjecttest,2)
    ## print(nrow(ytrain))
    return (ytrain)
  }
  
  loadSubjectTrainData <- function(dirPathParam)
  {
    subjecttrain<-read.table(file.path(dirPathParam[2],"subject_train.txt")) 
    ## print(nrow(subjecttrain))
    ## print(nrow(subjecttrain))
    return (subjecttrain)
  }
  
  loadActivityLabelsData <- function(dirPathParam)
    
  {
    activitylabels<-read.table(file.path(dirPathParam[3], 
                                         "activity_labels.txt"), 
                               col.names = c("Id", "Activity") 
    )
    return (activitylabels)
  }
  
  loadFeatureLabelsData <- function (dirPathParam)
  {
    featurelabels<-read.table(file.path(dirPathParam[3], 
                                        "features.txt"), 
                              colClasses = c("character") 
    )
    
    return (featurelabels)
    
  }
  
  mergeData <- function (XTrainData ,YTrainData , SubjectTrainData , XTestData, YTestData,SubjectTestData, FeatureLables)
  {
    
    ## print(nrow(Test1))
    
    #1.Merges the training and the test sets to create one data set. 
    traindata <-cbind(cbind(XTrainData, SubjectTrainData), YTrainData) 
    testdata<-cbind(cbind(XTestData, SubjectTestData), YTestData)
    
    sensordata<-rbind(traindata, testdata) 
    ## print(FeatureLables)
    sensorlabels<-rbind(rbind(FeatureLables, c(562, "Subject")), c(563, "Id"))[,2] 
    names(sensordata)<-sensorlabels 
    ## print(names(sensordata))
    ##print(head(sensordata,1))
    return (sensordata)
  }
  
  extractData <- function(mergedData , ActivityLabels)
  {
    
    ## head(mergedData,1)
    #2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    sensordatameanstd <- mergedData[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(mergedData))] 
    
    ## print(sensordatameanstd)
    
    #3. Uses descriptive activity names to name the activities in the data set 
    sensordatameanstd <- join(sensordatameanstd, ActivityLabels, by = "Id", match = "first") 
    ## print (head(sensordatameanstd) ,1)
    
    ## sensordatameanstd <- sensordatameanstd[,-1] 
    
    
    #4. Appropriately labels the data set with descriptive names. 
    
    ## head(sensordatameanstd,1)
    names(sensordatameanstd) <- gsub("([()])","",names(sensordatameanstd)) 
    #norm names 
    names(sensordatameanstd) <- make.names(names(sensordatameanstd)) 
    
    
    #5. From the data set in step 4, creates a second, independent tidy data set  
    # with the average of each variable for each activity and each subject  
    finaldata<-ddply(sensordatameanstd, c("Subject","Activity"), numcolwise(mean)) 
    #improve column names 
    finaldataheaders<-names(finaldata) 
    finaldataheaders<-sapply(finaldataheaders, addSuffix, ".mean") 
    names(finaldata)<-finaldataheaders 
    # Names after
    ## head(str(finaldata),6)
    write.table(finaldata, file = "sensordata_avg_by_subject.txt", row.name=FALSE) 
    
  }
  
  directoryPath <- setDirectory()
  XTestData <- loadXTestData (directoryPath)
  YTestData <- loadYTestData (directoryPath)
  SubjectTestData  <- loadSubjectTestData (directoryPath)  
  XTrainData <- loadXTrainData (directoryPath)
  YTrainData <- loadYTrainData (directoryPath)
  SubjectTrainData  <- loadSubjectTrainData (directoryPath) 
  ActivityLabels <- loadActivityLabelsData(directoryPath)
  FeatureLables <- loadFeatureLabelsData(directoryPath)
  
  mergedData <- mergeData(XTrainData ,YTrainData , SubjectTrainData , XTestData, YTestData,SubjectTestData, FeatureLables) 
  extractData(mergedData , ActivityLabels)
  
}

