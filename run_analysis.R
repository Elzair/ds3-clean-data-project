require(dplyr)

filterAndLabel <- function(data, arg) {
    cols <- c()
    nams <- c()
    for (i in 1:nrow(arg)) {
        cols <- c(cols, seq(from = arg[i, "cols"], to = arg[i, "cols"] + arg[i, "lens"] - 1))
        if(arg[i, "lens"] == 6) {
            nams <- c(nams,
                      paste("Average of ", arg[i, "titles"], " X"),
                      paste("Average of ", arg[i, "titles"], " Y"),
                      paste("Average of ", arg[i, "titles"], " Z"),
                      paste("Standard Deviation of ", arg[i, "titles"], " X"),
                      paste("Standard Deviation of ", arg[i, "titles"], " Y"),
                      paste("Standard Deviation of ", arg[i, "titles"], " Z")
                      )
        }
        else if(arg[i, "lens"] == 2) {
            nams <- c(nams,
                      paste("Average of ", arg[i, "titles"]),
                      paste("Standard Deviation of ", arg[i, "titles"]))
        }
        else if(arg[i, "lens"] == 1) {
            nams <- c(nams, arg[i, "titles"])
        }
    }

    filtered_data <- data[, cols]
    colnames(filtered_data) <- nams
    filtered_data
}

replaceActivities <- function(data) {
    new_data <- c()
    for(i in 1:nrow(data)) {
        dat <- switch(as.character(data[i,1]),
                             "1" = "Walking",
                             "2" = "Walking Upstairs",
                             "3" = "Walking Downstairs",
                             "4" = "Sitting",
                             "5" = "Standing",
                             "6" = "Laying"
                      )
        new_data <- c(new_data, dat)
    }
    data.frame(Activity = new_data)
}

average_data <- function(data) {
    data %>%
        group_by(Activity, Subject) %>%
        summarise_each(funs(mean))
}

run_analysis <- function() {
    Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = F)
    Xtest  <- read.table("UCI HAR Dataset/test/X_test.txt", header = F)
    strain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = F, col.names = c("Subject"))
    stest  <- read.table("UCI HAR Dataset/test/subject_test.txt", header = F, col.names = c("Subject"))
    ytrain <- replaceActivities(read.table("UCI HAR Dataset/train/y_train.txt", header = F,
                                           col.names = c("Activity")))
    ytest  <- replaceActivities(read.table("UCI HAR Dataset/test/y_test.txt", header = F,
                                       col.names = c("Activity")))

    Xmerge <- rbind(Xtrain, Xtest)
    smerge <- rbind(strain, stest)
    ymerge <- rbind(ytrain, ytest)

    cols <- c(1,
              41,
              81,
              121,
              161,
              201,
              214,
              227,
              240,
              253,
              266,
              345,
              424,
              503,
              516,
              529,
              542
              )
    titles <- c("tBody Acceleration",
                "tGravity Acceleration",
                "tBody Acceleration Jerk",
                "tBody Gyro",
                "tBody Gyro Jerk",
                "tBody Acceleration Magnitude",
                "tGravity Acceleration Magnitude",
                "tBody Acceleration Jerk Magnitude",
                "tBody Gyro Magnitude",
                "tBodyGyro Jerk Magnitude",
                "fBody Acceleration",
                "fBody Acceleration Jerk",
                "fBody Gyro",
                "fBody Acceleration Magnitude",
                "fBody Acceleration Jerk Magnitude",
                "fBody Gyro Magnitude",
                "fBody Gyro Jerk Magnitude")
    lens   <- c(6,
                6,
                6,
                6,
                6,
                2,
                2,
                2,
                2,
                2,
                6,
                6,
                6,
                2,
                2,
                2,
                2)

    colsArg <- data.frame(cols = cols, titles = titles, lens = lens)
    Xfilt   <- filterAndLabel(Xmerge, colsArg)
    merged  <- cbind(Xfilt, smerge, ymerge)
    average_data(merged)
}
