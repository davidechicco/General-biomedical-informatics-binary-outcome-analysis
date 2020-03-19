setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

# Software originally developed by Giuseppe Jurman <jurman@fbk.eu> on 1st March 2019
# Edited by Davide Chicco <davide.chicco@gmail.com> on 4th March 2019

# /usr/bin/Rscript biostatistics_analysis_tests_Shapiro_MannWhitney_Kruskal_ChiSquared.r "../data/patients_dataset.csv" "death_event"

# filename <- "../data/journal.pone.0187990.s002_EDITED_length_of_stay.csv"
# TARGET_LABEL <- "length_of_stay_days"

filename <- "/home/dave/hepatocellular_carcinoma/data/hcc-data_EDITED_NAs.csv"
TARGET_LABEL <- "survival"

resultsFolderPath = "../results/"

P_VALUE_THRESHOLD <- 0.005

# tableAnalysis
scientificFormat <- function(var) 
{
    return(formatC(var, format = "e", digits = 3))
}


# isThisNumberInteger
isThisNumberInteger <- function(num) {
    return(num%%1==0)
}

# areAllNumbersIntegers
areAllNumbersIntegers <- function(numArray)  {

    for(i in numArray) {
        if(isThisNumberInteger(i)==FALSE) return(FALSE)
    }
    return(TRUE)
}

# areAllContinuativeNumbersIntegers
areAllContinuativeNumbersIntegers <- function(numArray)  {

    sorteUndArray <- sort(unique(numArray))
    maxValue <- summary(numArray)[[6]]
    minValue <- summary(numArray)[[1]]

    if(areAllNumbersIntegers(numArray)==FALSE) return(FALSE)
    else if(maxValue > length(numArray)) return(FALSE)
    else if(minValue > 1) return(FALSE) # there could be categories starting from 2, though
    else {    
        for(i in sorteUndArray) {
            if(i >  length(numArray)) return(FALSE)
        }
    }
    
    return(TRUE)
}



SAVE_CORRELATIONS_PLOTS <- FALSE
SAVE_CORRELATIONS_LISTS <- FALSE

if (SAVE_CORRELATIONS_PLOTS == TRUE || SAVE_CORRELATIONS_LISTS==TRUE){
        mkDirResultsCommand <- paste0("mkdir -p ", resultsFolderPath)
        system(mkDirResultsCommand)
        cat("just run the command: ", mkDirResultsCommand, "\n", sep="")
}

# Data load

list.of.packages <- c("readr", "easypackages", "nortest", "Information", "stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)
# 
# EXP_ARG_NUM <- 2
# 
# LATEX_MODE <- FALSE
# 
# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   filename <- args[1]
#   TARGET_LABEL <- args[2]
# }

cat("filename: ", filename, "\n", sep="")
cat("TARGET_LABEL: ", TARGET_LABEL, "\n", sep="")

#filename <- "../data/patients_dataset.csv"
#TARGET_LABEL <- "death_event"


patients_dataset <- as.data.frame(read.csv(filename))
ANDERSON_FLAG <- FALSE
SHAPIRO_LIM <- 5000
if(nrow(patients_dataset) > SHAPIRO_LIM) {
    ANDERSON_FLAG <- TRUE
}


# print(head(patients_dataset))
source("utils.r")
num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)

ROUND_NUM <- 6

patients_dataset[[TARGET_LABEL]] <- as.factor(patients_dataset[[TARGET_LABEL]])

BINARY_TARGET_MODE <- FALSE

if ( length(unique(patients_dataset[[TARGET_LABEL]])) == 2 ) {
    BINARY_TARGET_MODE <- TRUE
}

DEATH_LABEL <- 1
SURVIVAL_LABEL <- 0
dead_patients_data <-  patients_dataset[patients_dataset[[TARGET_LABEL]]==DEATH_LABEL, ]
survived_patients_data <-  patients_dataset[patients_dataset[[TARGET_LABEL]]==SURVIVAL_LABEL, ]

# All the outputs of the tests are stored on the alltests lists, that we print at the end of the discussion.

mycols <- names(patients_dataset)
mycols <- mycols[mycols!=TARGET_LABEL]
alltests <- list()
alltests[["MannWhitney_rank"]] <- alltests[["Kruskal"]] <- alltests[["Chi"]] <- alltests[["Anderson"]] <- list()
for(thecol in mycols){
    alltests[["MannWhitney_rank"]][[thecol]] <-  wilcox.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")),     data=patients_dataset)
    alltests[["Kruskal"]][[thecol]] <-  kruskal.test(as.formula(paste(thecol,TARGET_LABEL,sep="~")),     data=patients_dataset)
    alltests[["Chi"]][[thecol]] <-     chisq.test(x=as.factor(patients_dataset[,thecol]),     y=patients_dataset[[TARGET_LABEL]],    simulate.p.value = TRUE)   
    
    if (ANDERSON_FLAG==FALSE) { 
        alltests[["Shapiro"]][[thecol]] <-  shapiro.test(patients_dataset[,thecol])
    } else {
        alltests[["Anderson"]][[thecol]] <- ad.test(patients_dataset[,thecol])
    }
    
}

if (ANDERSON_FLAG==FALSE) { 
    alltests[["Shapiro"]][[TARGET_LABEL]]<- shapiro.test(as.numeric(patients_dataset[[TARGET_LABEL]]))
} else {
    alltests[["Anderson"]][[TARGET_LABEL]]<- ad.test(as.numeric(patients_dataset[[TARGET_LABEL]]))
}

# As a rule of thumb, the validity of the tests is assessed by looking at the resulting p-values.
# # We start with the Shapiro test of normality,
# cat("\n\t\t == Shapiro ==\n")
vectorShapiro <- c()
if (ANDERSON_FLAG==FALSE) {
    for(thecol in c(mycols,TARGET_LABEL)) { 
        vectorShapiro[thecol] <- alltests[["Shapiro"]][[thecol]]$p.value 
    }
}

if (ANDERSON_FLAG) {
    # cat("\n\t\t == Anderson-Darling ==\n")
    vectorAnderson <- c()
    for(thecol in c(mycols,TARGET_LABEL)) { 
        vectorAnderson[thecol] <- alltests[["Anderson"]][[thecol]]$p.value 
    }
}


# cat("\n\n\t\t == MannWhitney_rank ==\n")
vectorMannWhitney <- c()
for(thecol in mycols) { 
   vectorMannWhitney[thecol] <- round(alltests[["MannWhitney_rank"]][[thecol]]$p.value, ROUND_NUM) 
    
  #  cat(names((vectorMannWhitney)[thecol]), "\t \t \t", ((vectorMannWhitney)[[thecol]]), "\n")
}
# print(vectorMannWhitney)

# cat("\n\t\t == Kruskal ==\n")
vectorKruskal <- c()
for(thecol in mycols) { 
   vectorKruskal[thecol] <- round(alltests[["Kruskal"]][[thecol]]$p.value, ROUND_NUM) 
    
    # cat(names((vectorKruskal)[thecol]), "\t \t \t", ((vectorKruskal)[[thecol]]), "\n")
}
# print(vectorKruskal)

# cat("\n\t\t == Chi ==\n")
vectorChi <- c()
for(thecol in mycols) { 
    vectorChi[thecol] <- round(alltests[["Chi"]][[thecol]]$p.value, ROUND_NUM)
    
    # cat(names((vectorChi)[thecol]), "\t \t \t", ((vectorChi)[[thecol]]), "\n")
}
# print(vectorChi)


sortedVectorShapiro <- sort(vectorShapiro)

if (ANDERSON_FLAG) {
    sortedVectorAnderson <- sort(vectorAnderson)
}

sortedVectorMannWhitney <- sort(vectorMannWhitney)
sortedVectorKruskal <- sort(vectorKruskal)
sortedVectorChi <- sort(vectorChi)

# print normal or LaTeX
LATEX_MODE <- FALSE

LATEX_SEP <- "&"
LATEX_END_OF_ROW <- "\\\\"

EMPTY_SEP <- ""
EMPTY_END_OF_ROW <- ""

SEP <- EMPTY_SEP
END_OF_ROW <- EMPTY_END_OF_ROW

if (LATEX_MODE == TRUE ) {
    SEP <- LATEX_SEP
    END_OF_ROW <- LATEX_END_OF_ROW
}


if (ANDERSON_FLAG==FALSE) {
    index <- 1
    cat("\n\t\t == Shapiro-Wilk test ==\n")
    for(thecol in names(sortedVectorShapiro)) {    
        
        cat(index, " ", SEP," \t",  names((sortedVectorShapiro)[thecol]), " ", SEP," \t ", scientificFormat((sortedVectorShapiro)[[thecol]]), " ", END_OF_ROW," \n", sep="")
        index <- index + 1
    }
}

if (ANDERSON_FLAG) {
    index <- 1
    cat("\n\t\t == Anderson-Darling test ==\n")
    for(thecol in names(sortedVectorAnderson)) {    
        
        cat(index, " ", SEP," \t",  names((sortedVectorAnderson)[thecol]), " ", SEP," \t ", scientificFormat((sortedVectorAnderson)[[thecol]]), " ", END_OF_ROW," \n", sep="")
        index <- index + 1
    }
}

ASTE <- ""

index <- 1
cat("\n\t\t == Mann-Whitney U test with target ", TARGET_LABEL, " ==\n", sep="")
for(thecol in names(sortedVectorMannWhitney)) { 
    
   isThisFeatureBinary <-  is.binary(subset(patients_dataset[, c(names(sortedVectorMannWhitney)[index])], !is.na(patients_dataset[, c(names(sortedVectorMannWhitney)[index])])))
   
   isThisFeatureInteger <- areAllContinuativeNumbersIntegers(subset(patients_dataset[, c(names(sortedVectorMannWhitney)[index])], !is.na(patients_dataset[, c(names(sortedVectorMannWhitney)[index])])))
    
    if(isThisFeatureBinary==FALSE & isThisFeatureInteger==FALSE) { 
    
            if( (sortedVectorMannWhitney)[[thecol]] < P_VALUE_THRESHOLD) ASTE <- "*"
    
            cat(index, " ", SEP," \t", ASTE,  names((sortedVectorMannWhitney)[thecol]), " ", SEP," \t ", scientificFormat((sortedVectorMannWhitney)[[thecol]]), " ", END_OF_ROW," \n", sep="")
    }
    index <- index + 1
    ASTE <- ""
}



index <- 1
 cat("\n\t\t == Kruskal-Wallis with target ", TARGET_LABEL, " ==\n", sep="")
for(thecol in names(sortedVectorKruskal)) { 

   isThisFeatureBinary <-  is.binary(subset(patients_dataset[, c(names(sortedVectorKruskal)[index])], !is.na(patients_dataset[, c(names(sortedVectorKruskal)[index])])))
   
   isThisFeatureInteger <- areAllContinuativeNumbersIntegers(subset(patients_dataset[, c(names(sortedVectorKruskal)[index])], !is.na(patients_dataset[, c(names(sortedVectorKruskal)[index])])))
   
   # cat("thecol isThisFeatureInteger\n")
   # cat(thecol, " ",  isThisFeatureInteger, "\n", sep="")
    
     if(isThisFeatureBinary==FALSE & isThisFeatureInteger==TRUE) { 
     
        if( (sortedVectorChi)[[thecol]] < P_VALUE_THRESHOLD) ASTE <- "*"
     
        cat(index, " ", SEP," \t", ASTE,  names((sortedVectorKruskal)[thecol]), " ", SEP," \t ", scientificFormat((sortedVectorKruskal)[[thecol]]), " ", END_OF_ROW," \n", sep="")
    }
    index <- index + 1
    ASTE <- ""
}

index <- 1
cat("\n\t\t == chi squared with target ", TARGET_LABEL, " ==\n", sep="")
for(thecol in names(sortedVectorChi)) { 

    isThisFeatureBinary <-  is.binary(subset(patients_dataset[, c(names(sortedVectorChi)[index])], !is.na(patients_dataset[, c(names(sortedVectorChi)[index])])))
    
    if(isThisFeatureBinary==TRUE) {     
    
        if( (sortedVectorChi)[[thecol]] < P_VALUE_THRESHOLD) ASTE <- "*"
    
        cat(index, " ", SEP," \t", ASTE,  names((sortedVectorChi)[thecol]), " ", SEP," \t ", scientificFormat((sortedVectorChi)[[thecol]]), " ", END_OF_ROW," \n", sep="")
     }
     index <- index + 1     
     ASTE <- ""
}

# let's create the dataframes

# dataframeShapiro <- as.data.frame(sortedVectorShapiro)
# dataframeShapiro$pos <- c(1:dim(dataframeShapiro)[1])
# dataframeShapiro$feature <- rownames(dataframeShapiro)

if(BINARY_TARGET_MODE) {
    dataframeMannWhitney <- as.data.frame(sortedVectorMannWhitney)
    dataframeMannWhitney$pos <- c(1:dim(dataframeMannWhitney)[1])
    dataframeMannWhitney$feature <- rownames(dataframeMannWhitney)
}

if(BINARY_TARGET_MODE==FALSE) { 
    dataframeKruskal <- as.data.frame(sortedVectorKruskal)
    dataframeKruskal$pos <- c(1:dim(dataframeKruskal)[1])
    dataframeKruskal$feature <- rownames(dataframeKruskal)
}

dataframeChiSquared <- as.data.frame(sortedVectorChi)
dataframeChiSquared$pos <- c(1:dim(dataframeChiSquared)[1])
dataframeChiSquared$feature <- rownames(dataframeChiSquared)

# let's create the plots

if(SAVE_CORRELATIONS_PLOTS == TRUE) {    

    x_upper_lim <- 300
    # barPlotOfRanking(dataframeShapiro, abs(log(dataframeShapiro$"sortedVectorShapiro")), dataframeShapiro$feature, dataframeShapiro$pos, exe_num, "feature", "abs(log(Shapiro-Wilk))", x_upper_lim, resultsFolderPath)    
    
    #  x_upper_lim <- 1
    # barPlotOfRanking(dataframeMannWhitney, dataframeMannWhitney$"sortedVectorMannWhitney", dataframeMannWhitney$feature, dataframeMannWhitney$pos, exe_num, "feature", "MannWhitney", x_upper_lim, resultsFolderPath)
    
#     x_upper_lim <- 1
#     barPlotOfRanking(dataframeKruskal, dataframeKruskal$"sortedVectorKruskal", dataframeKruskal$feature, dataframeKruskal$pos, exe_num, "feature", "Kruskal", x_upper_lim, resultsFolderPath)
#     
#     x_upper_lim <- 1
#     barPlotOfRanking(dataframeChiSquared, dataframeChiSquared$"sortedVectorChi", dataframeChiSquared$feature, dataframeChiSquared$pos, exe_num, "feature", "ChiSquared", x_upper_lim, resultsFolderPath)
    
}

if(SAVE_CORRELATIONS_LISTS == TRUE){

    
    # tableFileShapiro <- paste0(resultsFolderPath,"Shapiro_",exe_num, ".csv")
    tableFileMannWhitney <- paste0(resultsFolderPath,"MannWhitney_",exe_num, ".csv")
    tableFileKruskal <- paste0(resultsFolderPath,"Kruskal_",exe_num, ".csv")
    tableFileChiSquared <- paste0(resultsFolderPath,"chiSquared_",exe_num, ".csv")

#    #  write.table(dataframeShapiro[,c(3,1)], file=tableFileShapiro, sep=",", col.names=TRUE, row.names=FALSE)
#     cat("Saved file ", tableFileShapiro, "\n")
#     
#     # write.table(dataframeMannWhitney[,c(3,1)], file=tableFileMannWhitney, sep=",", col.names=TRUE, row.names=FALSE)
#     # cat("Saved file ", tableFileMannWhitney, "\n")
#     
#     write.table(dataframeKruskal[,c(3,1)], file=tableFileKruskal, sep=",", col.names=TRUE, row.names=FALSE)
#     cat("Saved file ", tableFileKruskal, "\n")
#     
#     write.table(dataframeChiSquared[,c(3,1)], file=tableFileChiSquared, sep=",", col.names=TRUE, row.names=FALSE)
#     cat("Saved file ", tableFileChiSquared, "\n")

}

computeExecutionTime()