setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

# agregateTwoSortedRankings
agregateTwoSortedRankings <- function(dd, firstColumnName, secondColumnName) {

    cat("\n[function agregateTwoSortedRankings()]\n")

    # dd_sorted_MSE <- dd[order(-dd$firstColumnName), ]
    dd_sorted_firstColumn <- dd[order(-dd[[firstColumnName]]), ]
    # print(dd_sorted_firstColumn)
    
    dd_sorted_secondColumn <- dd[order(-dd[[secondColumnName]]), ]
    # print(dd_sorted_IncNodePurity);


    # varImpPlot(rf_output)
    dd_sorted_firstColumn_only <- dd_sorted_firstColumn
    dd_sorted_firstColumn_only[[secondColumnName]] <- NULL # we do not need the other values
    dd_sorted_firstColumn_only$firstColPos <- c(1:dim(dd_sorted_firstColumn_only)[1])
    
    dd_sorted_secondColumn_only <- dd_sorted_secondColumn
    dd_sorted_secondColumn_only[[firstColumnName]] <- NULL # we do not need the other values
    dd_sorted_secondColumn_only$secondColPos <- c(1:dim(dd_sorted_secondColumn_only)[1])

    dd_sorted_firstColumn_only$features <- rownames(dd_sorted_firstColumn_only)
    dd_sorted_secondColumn_only$features <- rownames(dd_sorted_secondColumn_only)

    # let's sort alphabetically
    dd_sorted_firstColumn_only <- dd_sorted_firstColumn_only[order(dd_sorted_firstColumn_only$"features"), ]
    dd_sorted_secondColumn_only <- dd_sorted_secondColumn_only[order(dd_sorted_secondColumn_only$"features"), ]
    
    
    cat("\ncbind()\n")
    mergedRanking <- cbind(dd_sorted_firstColumn_only, dd_sorted_secondColumn_only)

    mergedRankingAlphaBeta <- mergedRanking[order(mergedRanking$"features"), ]
    mergedRankingAlphaBeta$posSum <- mergedRankingAlphaBeta$firstColPos + mergedRankingAlphaBeta$secondColPos

    mergedRankingGeneralRank <- mergedRankingAlphaBeta[order(mergedRankingAlphaBeta$"posSum"), ]
    mergedRankingGeneralRank$finalPos <- c(1:dim(mergedRankingGeneralRank)[1])
    
    # remove duplicate columns
    temp <- mergedRankingGeneralRank[, !duplicated(colnames(mergedRankingGeneralRank))]
    mergedRankingGeneralRank <- temp

    # print(mergedRankingGeneralRank)
    
    return (mergedRankingGeneralRank)

}


# EXP_ARG_NUM <- 2
# 
# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   fileNameData <- args[1]
#   targetName <- args[2]
# }



fileNameData <- "../data/hcvdat0_EDITED.csv"
targetName <- "category_0healthy_1sick"

# fileNameData <- "../data/journal.pone.0118297_S1_Dataset_HPV_EDITED_cirrhosis.csv"
# targetName <- "cirrhosis"
MISSING_DATA_IMPUTATION <- TRUE

list.of.packages <- c("easypackages", "randomForest", "ggplot2", "dplyr", "pastecs",  "mice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

NUM_METRICS <- 9
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC")


FEATURE_RANKING_PLOT_DEPICTION <-  FALSE
TWO_FEATURES_PLOT <- FALSE

patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

patients_data$"category_0healthy_1hepatitis_2fibrosis_3cirrhorsis" <- NULL

# rename target
names(patients_data)[names(patients_data) == targetName] <- "target"

cat("application of dplyr::select()\n")
patients_data <- patients_data%>%dplyr::select(-target,target)
target_index <- dim(patients_data)[2]    


num_to_return <- 1
upper_num_limit <- 100000
exe_num <- sample(1:upper_num_limit, num_to_return)


# patients_data$"HCV.RNATaqman.Log.IU.ml." <- as.numeric(patients_data$"HCV.RNATaqman.Log.IU.ml.")

# patients_data$"category_0healthy_1hepatitis_2fibrosis_3cirrhorsis" <- NULL
if(MISSING_DATA_IMPUTATION==TRUE){

    # missing data imputation
    NUM_DATASETS <- 1
    imputed_data <- mice(patients_data, m=NUM_DATASETS, maxit = 50, method = 'pmm', seed = 500)
    patients_data <- complete(imputed_data, NUM_DATASETS)
}


TRAINING_SET_RATIO <- 0.7
TEST_SET_RATIO <- 1 - TRAINING_SET_RATIO


allExecutionsFinalRanking <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)
                 

execution_classification_number <- 100 # 100
cat("Number of classification executions = ", execution_classification_number, "\t", sep="")
for(exe_class_i in 1:execution_classification_number)
{      

    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

    patients_training_set_index_start <- 1
    patients_training_set_index_end <- round(nrow(patients_data) * TRAINING_SET_RATIO)
    patients_test_set_index_start <- patients_training_set_index_end + 1
    patients_test_set_index_end <- nrow(patients_data)

    patients_training_set <- patients_data[patients_training_set_index_start:patients_training_set_index_end,]
    patients_test_set <- patients_data[patients_test_set_index_start:patients_test_set_index_end,]

    execution_FS_number <- 2 # 100
    cat("Number of FS executions = ", execution_FS_number, "\n", sep="")
    for(exe_fs_i in 1:execution_FS_number)
    {

	cat("\n\n\n Execution number FS = ", exe_fs_i, " & classif =  ", exe_class_i,"\n", sep="")
	cat("[Randomizing the rows]\n")
	patients_training_set <- patients_training_set[sample(nrow(patients_training_set)),] # shuffle the rows


	cat("application of randomForest()\n")
	rf_output <- randomForest(as.factor(patients_training_set$target) ~ ., data=patients_training_set, importance=TRUE, proximity=TRUE)
	    

	dd <- as.data.frame(rf_output$importance);
	
	mergedRankingGeneralRank <- agregateTwoSortedRankings(dd, "MeanDecreaseAccuracy", "MeanDecreaseGini")
	
	rownames(mergedRankingGeneralRank) <- (removeDot(removeUnderscore(rownames(mergedRankingGeneralRank))))
	mergedRankingGeneralRank$features <- removeDot(removeUnderscore(mergedRankingGeneralRank$features))

	print(mergedRankingGeneralRank[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE])

	finalRankingOneExecution <- mergedRankingGeneralRank[, c("features", "finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE]
	finalRankingOneExecutionAlphaBeta <- finalRankingOneExecution[order(finalRankingOneExecution$"features"), , drop=FALSE]

	if (exe_fs_i == 1) {
	    allExecutionsFinalRanking <- finalRankingOneExecutionAlphaBeta
	} else {
	    
	    allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy + finalRankingOneExecutionAlphaBeta$MeanDecreaseAccuracy
	    allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini + finalRankingOneExecutionAlphaBeta$MeanDecreaseGini
	    allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos + finalRankingOneExecutionAlphaBeta$finalPos
	}
    }



    allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy / execution_FS_number
    allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini / execution_FS_number
    allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos / execution_FS_number

    # # let's eliminate the target index from the rank
    # targetRow <-  which(allExecutionsFinalRanking==targetName)
    # allExecutionsFinalRanking <- allExecutionsFinalRanking[-c( which(allExecutionsFinalRanking==targetName)), ]

    cat("\n\n\n\n== final ranking after ", execution_FS_number, " executions == \n", sep="")

    allExecutionsFinalRanking_mse_Gini <-  allExecutionsFinalRanking[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")]
    aggregateRankings <- agregateTwoSortedRankings(allExecutionsFinalRanking_mse_Gini, "MeanDecreaseAccuracy", "MeanDecreaseGini")

    # print(aggregateRankings[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini")])

    print(allExecutionsFinalRanking_mse_Gini[order(-allExecutionsFinalRanking_mse_Gini["MeanDecreaseAccuracy"]), ])


    top_features_num <- 2
    selectedFeaturesNames <- rownames((allExecutionsFinalRanking_mse_Gini[order(-allExecutionsFinalRanking_mse_Gini["MeanDecreaseAccuracy"]), ])[1:top_features_num,])

    cat("number of selected top features: ", top_features_num, "\n", sep="")
    cat("selected top features: ", selectedFeaturesNames[1],  " and ", selectedFeaturesNames[2], "\n", sep="")


    patients_training_set_reduced_features <- patients_training_set[, c(selectedFeaturesNames, "target")]
    patients_test_set_reduced_features <- patients_test_set[, c(selectedFeaturesNames, "target")]

    cat("\n[Training Random Forests classifier on the test set with only the top ", top_features_num ," features]\n")
    rf_new <- NULL
    allFeaturesFormula <- as.formula(paste(as.factor("target"), '.', sep=' ~ ' ))
    patients_training_set_reduced_features$"target" <- as.factor(patients_training_set_reduced_features$"target")
    rf_new <- randomForest(allFeaturesFormula, data=patients_training_set_reduced_features, importance=FALSE, proximity=TRUE, type="classification")
	
    cat("\n[Applying the trained Random Forests classifier on the test set with only the top ", top_features_num ," features]\n")
    patients_data_test_PRED <- predict(rf_new, patients_test_set_reduced_features, type="prob")[,"1"]
    patients_data_test_labels <- patients_test_set_reduced_features$"target"
    thisConfMat <- confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED, "@@@ Test set @@@")

    if (exe_class_i == 1)  confMatDataFrame <-  thisConfMat
    else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)
    
 }
 
cat("\n\n\n=== final results ===\n")
cat("Number of executions = ", execution_number, "\n", sep="")

# statistics on the dataframe of confusion matrices
statDescConfMatr <- stat.desc(confMatDataFrame)
meanSigmaRowResults <- (statDescConfMatr)[c("mean","std.dev"),]
print(dec_three(statDescConfMatr))
cat("\n\n=== === === ===\n")
print(dec_three(meanSigmaRowResults))
cat("\n\n=== === === ===\n\n\n")


computeExecutionTime()

#
#	cirrhosis
#
# 		with training set = 70%
# 
#           MCC F1_score accuracy TP_rate TN_rate   PPV   NPV PR_AUC ROC_AUC
# mean    0.210    0.759    0.662   0.797   0.399 0.737 0.492  0.783   0.651
# std.dev 0.178    0.060    0.077   0.101   0.151 0.085 0.193  0.091   0.094
#
# 		with training set = 80%
# 
#           MCC F1_score accuracy TP_rate TN_rate   PPV   NPV PR_AUC ROC_AUC
# mean    0.202    0.752    0.658   0.796   0.389 0.724 0.503  0.775   0.638
# std.dev 0.195    0.081    0.090   0.113   0.145 0.096 0.211  0.112   0.114

#	hepatitis
#
# 		with training set = 80%
# 
#           MCC F1_score accuracy TP_rate TN_rate   PPV   NPV PR_AUC ROC_AUC
# mean    0.781    0.800    0.954   0.780   0.979 0.839 0.970  0.813   0.953
# std.dev 0.099    0.094    0.021   0.130   0.016 0.106 0.019  0.123   0.041


