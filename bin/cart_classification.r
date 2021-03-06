setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

EXP_ARG_NUM <- 2


# fileName <- "/home/davide/projects/arterial_events_and_IBD/data/journal.pone.0201991.s001_EDITED_event_type_binary.csv"
# targetName <- "TARGET_type_0ACS_1stroke"

# fileName <-  "/home/davide/projects/heart-failure-gene-expression-analysis/temp/patients_data_dataset_dim_red_svd5_file_1052379918.csv" 
# targetName <- "diagnosis"


fileName <- "../../../projects/sepsis_severity_ICU/data/sepsis_severity_dataset_edited_2019-02-11.csv"
targetName <- "ADDED.survival"

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "clusterSim", "PRROC", "e1071", "rpart",  "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

threshold <- 0.5

# file reading
patients_data <- read.csv(fileName, header = TRUE, sep =",");
cat("Read data from file ", fileName, "\n", sep="")

NUM_METRICS <- 9
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC")

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)

target_index <- dim(patients_data)[2]
original_patients_data <- patients_data

execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("[Execlution number ", exe_i, " out of ", execution_number, "]\n", sep="" )

    # shuffle the rows
    patients_data <- patients_data[sample(nrow(patients_data)),] 

    # Allocation of the size of the training set
    perce_training_set <- 80
    size_training_set <- round(dim(patients_data)[1]*(perce_training_set/100))

    cat("perce_training_set = ",perce_training_set,"%", sep="")

    # Allocation of the training set and of the test set
    training_set <- (patients_data[1:size_training_set,])
    test_set_index_start <- size_training_set+1
    test_set_index_end <- dim(patients_data)[1]
    test_set  <- patients_data[test_set_index_start:test_set_index_end,]

    test_labels <- patients_data[test_set_index_start:test_set_index_end, target_index]   # NEW


    print("dim(training_set)")
    print(dim(training_set))

    print("dim(test_set)")
    print(dim(test_set))


    # Generation of the CART model
    allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
    cart_model <- rpart(allFeaturesFormula, method="class", data=training_set);

    pred_test_predictions <- as.numeric(predict(cart_model, test_set, typ="class"))-1
    pred_test_set_labels <- as.numeric(test_set$death_event)

    patients_data_test_PRED_binary <- as.numeric(pred_test_predictions)

    patients_data_test_PRED_binary[patients_data_test_PRED_binary>=threshold]=1
    patients_data_test_PRED_binary[patients_data_test_PRED_binary<threshold]=0
    # mcc_outcome <- mcc(pred_test_set_labels, patients_data_test_PRED_binary)
    # confusion_matrix_rates(pred_test_set_labels, patients_data_test_PRED_binary)

    thisConfMat <- confusion_matrix_rates(test_labels, pred_test_predictions, "@@@ Test set @@@")

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


printResultsLatex("Decision tree", meanAndSdRowResults)

computeExecutionTime()


