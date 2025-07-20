library(aplore3)
library(tidyverse)
library(ggmosaic)

DF <- nhanes

str(DF)

summary(DF)

##Removed the 37 records that missing the weight since its just 0.5% of total observations

DF_1 <- DF[!(is.na(DF$wt)), ]

summary(DF_1)

for (i in c(2:ncol(DF_1))) {
  if (is.factor(DF_1[[i]])) {
    print(ggplot(DF_1, aes_string(x = colnames(DF_1)[i])) + geom_bar() + 
            labs(title = paste("Bar plot of", colnames(DF_1)[i]), x = colnames(DF_1)[i], 
                 y = "Count"))
  } else if (is.numeric(DF_1[[i]])) {
    print(ggplot(DF_1, aes_string(x = colnames(DF_1)[i])) + geom_histogram() +
            labs(title = paste("Histogram of", colnames(DF_1)[i]), x = colnames(DF_1)[i],
                 y = "Count"))
  } else {
    print(paste(colnames(DF_1)[i], "is not a factor or numeric. Skipping."))
  }
}

for (i in c(2:(ncol(DF_1)-1))) {
  if (is.factor(DF_1[[i]])) {
    print(ggplot(DF_1, aes_string(x = colnames(DF_1)[i])) + geom_bar(aes(fill = obese)) + 
            labs(title = paste("Bar plot of", colnames(DF_1)[i]), x = colnames(DF_1)[i], 
                 y = "Count"))
  } else if (is.numeric(DF_1[[i]])) {
    print(ggplot(DF_1, aes_string(x = colnames(DF_1)[i])) + geom_histogram(aes(fill = obese)) + facet_grid(~ obese) +
            labs(title = paste("Histogram of", colnames(DF_1)[i]), x = colnames(DF_1)[i],
                 y = "Count"))
  } else {
    print(paste(colnames(DF_1)[i], "is not a factor or numeric. Skipping."))
  }
}


table(DF_1$sedmin)

