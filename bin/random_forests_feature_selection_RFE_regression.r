setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

# fileName <- "/home/davide/projects/neuroblastoma_glioblastoma/data/EasternChinaPeerJ/10.7717_peerj.5665_supplementary_table_RAW_5_EDITED.csv"
# targetName <- "outcome"
# 
# fileName <- "/home/davide/projects/neuroblastoma_EHRs/data/10.1371_journal.pone.0063253_Table_S1_EDITED.csv"
# targetName <- "outcome_binary"


fileName <- "../data/pone_0174640_dataset_EDITED.csv"
targetName <- "Duration"

TRAIN_SET_OVERSAMPLING_SYNTHETIC <- FALSE

# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "clusterSim", "PRROC", "e1071", "rpart",  "dplyr", "pastecs", "randomForest", "mice")
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
	  
	  rf_new <- NULL
	  rf_new <- randomForest(allFeaturesFormula, data=training_set_with_target, importance=FALSE, proximity=TRUE)
	  
	  cat("\n[Applying the trained random forest classifier on the test set]\n")
	  patients_data_test_PRED <- predict(rf_new, test_set)

        thisResult <- regression_rates(test_labels, patients_data_test_PRED, "@@@ Test set @@@")

        if (exe_i == 1)  resultDataFrame <-  thisResult
        else  resultDataFrame <- rbind(resultDataFrame, thisResult)
	  
      }
      
      cat("\n\n\n=== final results ===\n")
      cat("Number of executions = ", execution_number, "\n", sep="")

      cat("feature removed: ", feature_name, "\n", sep="")
      # statistics on the dataframe of confusion matrices
      statDescConfMatr <- stat.desc(resultDataFrame)
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

patients_data <- original_patients_data

# let's put the target label last on the right 
patients_data <- patients_data%>%dplyr::select(-targetName,targetName)

target_index <- ncol(patients_data)

NUM_METRICS <- 5
resultDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(resultDataFrame) <- c("RMSE", "MAE", "MSE", "SMAPE", "R^2")

# let's put the target label last on the right 
# original_patients_data <- original_patients_data%>%select(-targetName,targetName)
target_index <- dim(original_patients_data)[2]    
original_patients_data[,target_index] <- as.factor(original_patients_data[,target_index])


MISSING_DATA_IMPUTATION <- TRUE
if(MISSING_DATA_IMPUTATION==TRUE){

    # missing data imputation
    NUM_DATASETS <- 1
    imputed_data <- mice(original_patients_data, m=NUM_DATASETS, maxit = 50, method = 'pmm', seed = 500)
    original_patients_data <- complete(imputed_data, NUM_DATASETS)
}


original_patients_data <- original_patients_data[complete.cases(original_patients_data), ]


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
	list_of_results[p] <- paste0(toString(thisResultMatrix[c("mean"),c("R^2")]), "_", thisFeature)
	
	p <- p + 1
}	

z <- 1
cat("\n ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ \n")
cat("final importance ranking\n")
cat("feature_R^2\n")
print(sort(list_of_results))

cat(" ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ \n ")

computeExecutionTime()
