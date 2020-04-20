setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

# fileName <- "/home/davide/projects/arterial_events_and_IBD/data/journal.pone.0201991.s001_EDITED_binary_event.csv"
# targetName <- "TARGET_arterial_event"

fileName <- "/home/davide/projects/arterial_events_and_IBD/data/journal.pone.0201991.s001_EDITED_event_type_binary.csv"
targetName <- "TARGET_type_0ACS_1stroke"

TRAIN_SET_OVERSAMPLING_SYNTHETIC <- FALSE

# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "clusterSim", "PRROC", "e1071", "rpart",  "dplyr", "pastecs", "xgboost", "ROSE")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")
execution_number <- 100
      



# apply_procedure
apply_procedure <- function(patients_data, feature_name, target_index) {


      cat("Number of executions = ", execution_number, "\n", sep="")
      for(exe_i in 1:execution_number)
      {

	  cat("[Execlution number ", exe_i, " out of ", execution_number, "] we silence the ", feature_name ," feature \n", sep="" )

	  # shuffle the rows
	  patients_data <- patients_data[sample(nrow(patients_data)),] 

	  # Allocation of the size of the training set
	  perce_training_set <- 80
	  size_training_set <- round(dim(patients_data)[1]*(perce_training_set/100))

	  cat("perce_training_set = ",perce_training_set,"%", sep="")

	  # Allocation of the training set and of the test set
	  training_set_with_target <- patients_data[1:size_training_set, (1:(target_index))]
	  
	  # formula
	  allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
	  if(TRAIN_SET_OVERSAMPLING_SYNTHETIC == TRUE)
	      {
		  thisP <- 0.5
	      
		  data.rose <- ROSE(allFeaturesFormula, data = training_set_with_target, p=thisP, seed = 1)$data
		  training_set_with_target <- data.rose
	      }
	  
	  training_set <- training_set_with_target[, (1:(target_index-1))]
	  
	  test_set_index_start <- size_training_set+1
	  test_set_index_end <- dim(patients_data)[1]
	  test_set  <- patients_data[test_set_index_start:test_set_index_end, (1:(target_index-1))]

	  training_labels <- patients_data[1:size_training_set, target_index]   # NEW
	  test_labels <- patients_data[test_set_index_start:test_set_index_end, target_index]   # NEW


	  print("dim(training_set)")
	  print(dim(training_set))

	  print("dim(test_set)")
	  print(dim(test_set))

	  this_max_depth <- 2
	  this_eta <- 1
	  this_nthread <- 2
	  this_nrounds <- 2
	  this_verbose <- 0
	  
	  bst_model <- xgboost(data = as.matrix(training_set), label=training_labels, max.depth = this_max_depth, eta = this_eta, nthread = this_nthread, nrounds = this_nrounds, objective = "binary:logistic", verbose = this_verbose)
	
	  pred <- predict(bst_model, as.matrix(test_set))
	  pred_test_predictions <- as.numeric(pred)
	  pred_test_set_labels <- as.numeric(test_labels)

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

      cat("feature removed: ", feature_name, "\n", sep="")
      # statistics on the dataframe of confusion matrices
      statDescConfMatr <- stat.desc(confMatDataFrame)
      meanSdRowResults <- (statDescConfMatr)[c("mean", "std.dev"),]
      print(dec_three(statDescConfMatr))
      cat("\n\n=== === === ===\n")
      cat("[final] feature removed: ", feature_name, "\n", sep="")
      print(dec_three(meanSdRowResults))
      cat("\n\n=== === === ===\n")
  
      return(statDescConfMatr)



}


threshold <- 0.5

# file reading
original_patients_data <- read.csv(fileName, header = TRUE, sep =",");
cat("Read data from file ", fileName, "\n", sep="")

NUM_METRICS <- 7
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP rate", "TN rate", "PR AUC", "ROC AUC")

# let's put the target label last on the right 
original_patients_data <- original_patients_data%>%select(-targetName,targetName)

# results
list_of_results <- vector()

p <- 1
for(thisFeature in colnames(original_patients_data[,1:(ncol(original_patients_data)-1)])) {

	these_patients_data <- original_patients_data
      
        thisFeatureColumnIndex <- grep(thisFeature, colnames(these_patients_data))
		  
	# let's silence/remove this gene
	these_patients_data[thisFeatureColumnIndex] <- NULL
	this_target_index <- dim(these_patients_data)[2]
	
	cat("dim(these_patients_data): ")
	print(dim(these_patients_data))
	thisResultMatrix <- apply_procedure(these_patients_data, thisFeature, this_target_index)
	list_of_results[p] <- paste0(toString(thisResultMatrix[c("mean"),c("MCC")]), "_", thisFeature)
	
	p <- p + 1
}	

z <- 1
cat("\n ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ \n")
cat("final importance ranking\n")
cat("feature_mcc\n")
print(sort(list_of_results))

cat(" ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ \n ")

computeExecutionTime()
