library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)

df1 <- read.csv("C:/Users/91930/Downloads/test_Y3wMUE5_7gLdaTN.csv", header = TRUE)
nRow <- nrow(df1)
nCol <- ncol(df1)
cat(sprintf("There are %d rows and %d columns", nRow, nCol))

plotCorrelationMatrix <- function(df, graphWidth){
  df <- df %>% select_if(is.numeric) %>% na.omit()
  if (ncol(df) < 2){
    cat(sprintf("No correlation plots shown: The number of non-NaN or constant columns (%d) is less than 2", ncol(df)))
    return()
  }
  corr <- cor(df)
  options(repr.plot.width = graphWidth, repr.plot.height = graphWidth)
  corrplot::corrplot(corr, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.6)
}
plot.new(); dev.off()
plotCorrelationMatrix(df1, 8)

plotPerColumnDistribution <- function(df, nGraphShown, nGraphPerRow){
  nunique <- sapply(df, function(x) length(unique(x)))
  df <- df[, nunique > 1 & nunique < 50]
  nCol <- ncol(df)
  columnNames <- colnames(df)
  nGraphRow <- ceiling(nCol / nGraphPerRow)
  options(repr.plot.width = 6 * nGraphPerRow, repr.plot.height = 8 * nGraphRow)
  par(mfrow = c(nGraphRow, nGraphPerRow))
  for (i in 1:min(nCol, nGraphShown)){
    columnDf <- df[, i]
    if (!is.numeric(columnDf)){
      valueCounts <- table(columnDf)
      barplot(valueCounts, main = paste(columnNames[i], "(column", i, ")"), xlab = columnNames[i], ylab = "counts", col = "steelblue")
    } else {
      hist(columnDf, main = paste(columnNames[i], "(column", i, ")"), xlab = columnNames[i], ylab = "counts", col = "steelblue")
    }
  }
}
plotPerColumnDistribution(df1, 10, 5)


plotScatterMatrix <- function(df, plotSize, textSize){
  df <- df %>% select_if(is.numeric) %>% na.omit()
  columnNames <- colnames(df)
  if (length(columnNames) > 10){
    columnNames <- columnNames[1:10]
  }
  options(repr.plot.width = plotSize, repr.plot.height = plotSize)
  ggplot(df, aes(x = .data[[columnNames[1]]], y = .data[[columnNames[2]]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Scatter and Density Plot", x = columnNames[1], y = columnNames[2]) +
    theme(plot.title = element_text(size = textSize))
}
plotScatterMatrix(df1, 15, 10)

df2 <- read.csv("C:/Users/91930/Downloads/train_u6lujuX_CVtuZ9i.csv", header = TRUE)
nRow <- nrow(df2)
nCol <- ncol(df2)
cat(sprintf("There are %d rows and %d columns", nRow, nCol))

plotPerColumnDistribution(df2, 10, 5)
plotScatterMatrix(df2, 15, 10)
plot.new();
plotCorrelationMatrix(df2, 1555)
