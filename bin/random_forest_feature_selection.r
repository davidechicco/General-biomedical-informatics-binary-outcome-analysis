setwd(".")
options(stringsAsFactors = FALSE)

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
    dd_sorted_firstColumn_only$secondColPos <- c(1:dim(dd_sorted_firstColumn_only)[1])
    
    dd_sorted_secondColumn_only <- dd_sorted_secondColumn
    dd_sorted_secondColumn_only[[firstColumnName]] <- NULL # we do not need the other values
    dd_sorted_secondColumn_only$firstColPos <- c(1:dim(dd_sorted_secondColumn_only)[1])

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

# fileNameData <-  "/home/davide/projects/breast_cancer_Coimbra/data/dataR2_EDITED.csv"
# targetName <- "DIAGNOSIS"

fileNameData <-  "/home/dave/cervical_cancer/cervical_arranged_NORM_ONLY_BIOPSY_TARGET.csv" 
targetName <- "Biopsy"

# fileNameData<- "../data/dataset_edited_without_time.csv"
# targetName <- "death_event"

list.of.packages <- c("easypackages", "randomForest", "ggplot2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

PLOT_DEPICTION <- FALSE

patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

cat("application of dplyr::select()\n")
patients_data <- patients_data%>%dplyr::select(-targetName,targetName)
target_index <- dim(patients_data)[2]    

num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)

# allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
# rf_output <- randomForest(allFeaturesFormula, data=patients_data, importance=TRUE, proximity=TRUE)

allExecutionsFinalRanking <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

execution_number <- 10 
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("\n\n\n Execution number ", exe_i,"\n", sep="")
    cat("[Randomizing the rows]\n")
    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows


    cat("application of randomForest()\n")
    rf_output <- randomForest(as.factor(patients_data[, targetName]) ~ ., data=patients_data, importance=TRUE, proximity=TRUE)


    dd <- as.data.frame(rf_output$importance);
    
    mergedRankingGeneralRank <- agregateTwoSortedRankings(dd, "MeanDecreaseAccuracy", "MeanDecreaseGini")
    
    lastCol <- dim(mergedRankingGeneralRank)[2]

    featuresCol <- 6

    rownames(mergedRankingGeneralRank) <- (removeDot(removeUnderscore(rownames(mergedRankingGeneralRank))))
    mergedRankingGeneralRank$features <- removeDot(removeUnderscore(mergedRankingGeneralRank$features))

    print(mergedRankingGeneralRank[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE])

    finalRankingOneExecution <- mergedRankingGeneralRank[, c("features", "finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE]
    finalRankingOneExecutionAlphaBeta <- finalRankingOneExecution[order(finalRankingOneExecution$"features"), , drop=FALSE]

    if (exe_i == 1) {
        allExecutionsFinalRanking <- finalRankingOneExecutionAlphaBeta
    } else {
        
        allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy + finalRankingOneExecutionAlphaBeta$MeanDecreaseAccuracy
        allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini + finalRankingOneExecutionAlphaBeta$MeanDecreaseGini
        allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos + finalRankingOneExecutionAlphaBeta$finalPos
    }
}

allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy / execution_number
allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini / execution_number
allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos / execution_number

cat("\n\n\n\n== final ranking after ", execution_number, " executions == \n", sep="")

allExecutionsFinalRanking_mse_Gini <-  allExecutionsFinalRanking[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")]
aggregateRankings <- agregateTwoSortedRankings(allExecutionsFinalRanking_mse_Gini, "MeanDecreaseAccuracy", "MeanDecreaseGini")

print(aggregateRankings[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini")])

# aggregateRankings[c("features",  )


if (PLOT_DEPICTION == TRUE) {

        library("ggplot2")
        # Minimal theme + blue fill color

       #print(colnames(dd_sorted_MSE_only))


        pdfFileNameMSE <-  paste("../results/MSE_features_", exe_num, ".pdf", sep="")
        pdf(pdfFileNameMSE)
        p <- ggplot(data=dd_sorted_MSE_only, aes(x=reorder(features, -msePos), y=MeanDecreaseAccuracy)) +  geom_bar(stat="identity", fill="steelblue")  + labs(title = "Feature importance on accuracy reduction", y = "accuracy reduction", x = "features")
        p <- p + coord_flip()
        plot(p)
        dev.off()

    
        # print(colnames(dd_sorted_IncNodePurity_only))

        pdfFileNameGini <-  paste("../results/Gini_features_", exe_num, ".pdf", sep="")
        pdf(pdfFileNameGini)
        p <- ggplot(data=dd_sorted_IncNodePurity_only, aes(x=reorder(features, -purityPos), y=MeanDecreaseGini)) +  geom_bar(stat="identity", fill="steelblue")  + labs(title = "Feature importance on Gini impurity", y = "Gini impurity", x = "features")
        p <- p + coord_flip()
        plot(p)
        dev.off()
        
        pdfFile_plot_death_serum_creatinine <- paste("../results/plot_death_VS_serum_creatinine_NORM_", exe_num, ".pdf", sep="")
        pdf(pdfFile_plot_death_serum_creatinine)
        plot_death_serum_creatinine <-  cdplot(factor(patients_data[, targetName], labels=c("survived", "dead")) ~ serum_creatinine, data=patients_data, ylab = NA, xlab="serum creatinine")
        # plot(plot_death_serum_creatinine)
        dev.off()
       

        pdfFile_plot_death_ejection_fraction <- paste("../results/plot_death_VS_ejection_fraction_NORM_", exe_num, ".pdf", sep="")
        pdf(pdfFile_plot_death_ejection_fraction)
        plot_death_ejection_fraction <- cdplot(factor(patients_data[, targetName], labels=c("survived", "dead")) ~ ejection_fraction, data=patients_data, ylab = NA, xlab="ejection fraction")
        
        #plot(plot_death_ejection_fraction)
        dev.off()

        # pdfFile_plot_death_age <- paste("../results/plot_death_VS_age_", exe_num, ".pdf", sep="")
        # pdf(pdfFile_plot_death_age)
        # plot_death_age <- cdplot(factor(targetName, labels=c("survived", "dead")) ~ age,, data=patients_data, ylab = NA)
        # dev.off()

        num_of_patients <- dim(patients_data)[1]
        num_of_features <- dim(patients_data)[2] - 1

        #
        # Pearson correlation coefficient
        #

        i <- 1
        for (i in 1:num_of_features) {

            thisPCC <- cor(patients_data[, targetName], patients_data[,i], method = c("pearson"))

            cat("pearson(targetName, ", colnames(patients_data)[i],")  = ", dec_two(thisPCC), "\n", sep="")
        }

        # 
        # Kendall
        # 
        i <- 1
        for (i in 1:num_of_features) {

            thisKendall <- cor(patients_data[, targetName], patients_data[,i], method = c("kendall"))

            cat("kendall(targetName, ", colnames(patients_data)[i],")  = ", dec_two(thisKendall), "\n", sep="")
        }


        # Pearson correlation coefficients
        # 0.37
        # -0.26
        # 0.24
        # 0.03
        # -0.20
        # -0.00
        # -0.04
        # -0.01
        # 0.08
        # -0.00
        # 0.07


        # Print the model tree
        # reprtree:::plot.getTree(rf_new)

        dotSize <- 3
        pdfHeight <- 10 # inches
        pdfWidth <- 20 # inches
        textSize <- 30
        
        print(head(patients_data))

        pdfFile_dfFile_plot_death_age <- paste("../results/scatterplot_serum_creatinine_VS_ejection_fraction_", exe_num, ".pdf", sep="")
        pdf(pdfFile_dfFile_plot_death_age, height=pdfHeight, width=pdfWidth)
        p <- ggplot(patients_data, aes(x=serum_creatinine, y=ejection_fraction, color=factor(patients_data[, targetName], labels = c("survived", "dead")) )) + geom_point(size = dotSize)  + xlab("serum creatinine")   + ylab("ejection fraction") +  labs(colour="patient status", size="") + theme(text = element_text(size=textSize))
        plot(p)
        dev.off()

        pdfFile_dfFile_plot_death_age_withLine <- paste("../results/scatterplot_serum_creatinine_VS_ejection_fraction_withLine_", exe_num, ".pdf", sep="")
        pdf(pdfFile_dfFile_plot_death_age_withLine, height=pdfHeight, width=pdfWidth)
        p <- ggplot(patients_data, aes(x=serum_creatinine, y=ejection_fraction, color=factor(patients_data[, targetName], labels = c("survived", "dead")), shape=factor(patients_data[, targetName], labels = c("survived", "dead")) )) + geom_point(size = dotSize)  + xlab("serum creatinine")   + ylab("ejection fraction") +  labs(colour="patient status", size="", shape="") + geom_abline(intercept = 0.01, slope = 15, linetype="dashed") + theme(text = element_text(size=textSize)) + scale_shape(guide = 'none')
        plot(p)
        dev.off()

}
