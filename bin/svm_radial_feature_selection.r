setwd(".")
options(stringsAsFactors = FALSE)

fileName <- "../data/dataset_edited_without_time.csv"
targetName <- "death_event"

# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   fileName <- args[1]
#   targetName <- args[2]
# }

list.of.packages <- c("easypackages", "e1071","PRROC", "caret", "fastAdaboost", "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

# put the target on the last right position
patients_data <- patients_data%>%select(-targetName,targetName)
patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

totalElements <- dim(patients_data)[1]

subsets_size <- totalElements

if (subsets_size != totalElements) {
    cat("!!! ATTENTION: We are running the method on a subset of the original dataset, \n", sep="")
    cat("!!! containing only ", subsets_size, " elements \n", sep="")
    cat("!!! instead of ", totalElements, " elements \n", sep="")
}

patients_data <- patients_data[1:subsets_size, ]

dataset_dim_retriever(patients_data)
target_index <- dim(patients_data)[2]
# imbalance_retriever(patients_data[,target_index])

patients_data_labels <- as.factor(patients_data[, target_index])


num_folds <- 200
num_feature <- c(ncol(patients_data))

cat("SVM with radial kernel\n")

# svm for feature selection
svmProfile <- rfe(patients_data, patients_data_labels,
                  sizes = num_feature,
                  rfeControl = rfeControl(functions = caretFuncs, number = num_folds),
                  method = "svmRadial")
                  
                  
featureImportance <- varImp(svmProfile, scale=FALSE)
featureImportance$feature <- rownames(featureImportance)

rownames(featureImportance) <- (removeUnderscore(rownames(featureImportance)))
featureImportance$feature <- removeUnderscore(featureImportance$feature)

print(featureImportance)

