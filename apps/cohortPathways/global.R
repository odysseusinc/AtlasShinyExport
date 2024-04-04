library(purrr)
library(reactable)
library(shiny)
library(plotly)
library(dplyr)

path = "data"
csv_files <- list.files(path, pattern = ".csv", full.names = TRUE)

appData <-
  purrr::map(
    csv_files,
    ~ data.table::fread(., data.table = T)
    )

# col4 <- data.frame(colnames(appData[[4]]))
# maxSteps <- c(col4[grep("Step", col4$col4name),])
# designations <- c(appData[[4]]$`Step 1`)

steps <- appData[[4]] %>%
  select(grep("Step", colnames(appData[[4]]))) 

steps <- steps[,which(unlist(lapply(steps, function(x)!all(is.na(x))))),with=F]

# for(j in 1:dim(steps)[1]){
#   for (i in 1:dim(steps)[2]-1){
#     if (steps[[j, i+1]] == ""){
#       steps[[j, i+1]] <- steps[[j, i]]
#     }
#   }
# }

#steps <- matrix(steps, nrow = dim(steps)[1], nrow = dim(steps)[2])
steps <- matrix(steps, nrow = dim(steps)[1], nrow = dim(steps)[2])

for (i in 1:dim(steps)[2]) {
  assign(paste0("key_", i), unique(unlist(steps[[i]])))
} 


stepSplit = matrix(NA, nrow = dim(steps)[1], ncol = dim(steps)[2])

for (j in 1:dim(steps)[2]) {
  for (i in 1:dim(steps)[1]) {
    stepSplit[i,j] <- strsplit(steps[[paste0("Step ", j)]][i], '+')
  }
}

stepsNumeric = matrix(NA, nrow = dim(steps)[1], ncol = dim(steps)[2])  

for (j in 1:dim(steps)[2]) {
  for (i in 1:dim(steps)[1]) {
    # print(paste0("i: ", i))
    # print(paste0("j: ", j))
    # print(steps[[paste0("Step ", j)]][i])
    if (steps[[paste0("Step ", j)]][i] != "") {
      print(paste0("Step: ", steps[[paste0("Step ", j)]][i]))
      print(paste0("Key: ", which(key %in% steps[[paste0("Step ", j)]][i])))
      print(paste0("i: ", i))
      print(paste0("j: ", j))
      stepsNumeric[i,j] <- which(key %in% steps[[paste0("Step ", j)]][i])
    }
  }
}
