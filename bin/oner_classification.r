setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

EXP_ARG_NUM <- 2

fileName <- "/home/davide/projects/breast_cancer_Coimbra/data/dataR2_EDITED.csv"
targetName <- "DIAGNOSIS"

list.of.packages <- c("easypackages", "OneR", "class", "gmodels", "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

NUM_METRICS <- 9
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC")


threshold <- 0.5

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

# rename target
names(patients_data)[names(patients_data) == targetName] <- "target"

# let's put the target label last on the right 
patients_data <- patients_data%>%dplyr::select(-target,target)
patients_data_original <- patients_data

execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("[Execlution number ", exe_i, " out of ", execution_number, "]\n", sep="" )
    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows
    target_index <- dim(patients_data)[2]

    training_set_perce = 80
    cat("training_set_perce = ", training_set_perce, "\n", sep="")

    # the training set is the first 60% of the whole dataset
    training_set_first_index <- 1 # NEW
    training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) # NEW

    # the test set is the last 40% of the whole dataset
    test_set_first_index <- training_set_last_index+1 # NEW
    test_set_last_index <- dim(patients_data)[1] # NEW

    cat("[Creating the subsets for the values]\n")
    patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
    patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW

    patients_data_test_labels  <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW


    print("dim(patients_data_train)")
    print(dim(patients_data_train))

    print("dim(patients_data_test)")
    print(dim(patients_data_test))


    # Original application of One Rule with all the dataset
    prc_model_train <- OneR(patients_data_train, verbose = TRUE)

    summary(prc_model_train)
    prediction <- predict(prc_model_train, patients_data_test)
    # eval_model(prediction, patients_data_test)

    prediction_binary <- as.numeric(prediction) -1
    patients_data_test_PRED_binary <- data.frame(prediction)

    thisConfMat <- confusion_matrix_rates(patients_data_test_labels, prediction_binary, "@@@ Test set @@@")
    
    if (exe_i == 1)  confMatDataFrame <-  thisConfMat
    else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)
        
 }
 
cat("\n\n\n=== final results ===\n")
cat("Number of executions = ", execution_number, "\n", sep="")

# statistics on the dataframe of confusion matrices
statDescConfMatr <- stat.desc(confMatDataFrame)
meanAndSdRowResults <- (statDescConfMatr)[c("mean", "std.dev"),]
print(dec_three(statDescConfMatr))
cat("\n\n=== === === ===\n")


printResultsLatex("One rule", meanAndSdRowResults)

computeExecutionTime()


