setwd(".")
options(stringsAsFactors = FALSE)

fileNameData<- "../data/dataset_edited_without_time.csv"
targetName <- "death_event"

# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   fileName <- args[1]
#   targetName <- args[2]
# }

list.of.packages <- c("easypackages", "PRROC", "e1071", "clusterSim","rpart", "caret", "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")


patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

# put the target on the last right position
patients_data <- patients_data%>%select(-targetName,targetName)

target_index <- dim(patients_data)[2]
original_patients_data <- patients_data

# shuffle the rows
patients_data <- original_patients_data[sample(nrow(original_patients_data)),] 

# select formula based on feature
allFeaturesFormula <- as.formula(paste(colnames(patients_data)[target_index], '.', sep=' ~ ' ))
selectedFormula <- allFeaturesFormula

cart_model <- rpart(selectedFormula, method="class", data=patients_data);
ptree <- prune(cart_model, cp=cart_model$cptable[which.min(cart_model$cptable[,"xerror"]),"CP"])
importanceFrame <- (varImp(ptree))
importanceFrame$feature <- rownames(importanceFrame)
importanceFrameSorted <- importanceFrame[order(-importanceFrame$"Overall"), ]

rownames(importanceFrameSorted) <- (removeUnderscore(rownames(importanceFrameSorted)))
importanceFrameSorted$feature <- removeUnderscore(importanceFrameSorted$feature)


print(importanceFrameSorted)
