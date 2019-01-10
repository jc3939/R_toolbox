library(plyr)
# handy function that take data frame as input and then output summary statistics tables
# for numerical columns and categorical columns respectively.
summary_statistics <- function(df) {
  list.of.cols <- colnames(df)
  is.fact <- sapply(df, is.factor)
  factors.df <- df[, is.fact, drop = FALSE]
  is.num <- sapply(df, is.numeric)
  num.df <- df[, is.num, drop = FALSE]
  ## analyze numeric columns
  num.summary.table <- data.frame(column.names = character(),
                                  num.na.values=integer(),
                                  num.total.values=integer(),
                                  na.ratio=double(),
                                  maximum = double(),
                                  p95 = double(),
                                  p90 = double(),
                                  p75 = double(),
                                  p50 = double(),
                                  p25 = double(),
                                  p10 = double(),
                                  minimum = double(),
                                  std = double(),
                                  avg = double(),
                                  stringsAsFactors=FALSE)
  for (col in colnames(num.df)) {
    num.na.values <- sum(is.na(num.df[,col]))
    num.total.values <- length(num.df[,col])
    na.ratio <- round(num.na.values/num.total.values, 2)
    maximum <- max(num.df[,col])
    minimum <- min(num.df[,col])
    std <- sd(num.df[,col])
    avg <- mean(num.df[,col])
    percentiles <- unname(quantile(num.df[,col], c(.95, .90, .75, .5, .25, .1)))
    line.of.interest <- c(col, num.na.values, 
                          num.total.values, na.ratio, 
                          maximum, percentiles, 
                          minimum, std, avg)
    num.summary.table[nrow(num.summary.table)+1, ] <- line.of.interest
  }
  ##analyze categorical column
  cat.summary.table <- data.frame(column.names = character(),
                                  num.total.values = integer(),
                                  factor.levels = character(),
                                  num.factor.values = integer(),
                                  stringsAsFactors=FALSE)
  
  for (col in colnames(factors.df)) {
    col='cat1'
    freq_table <- count(factors.df, col)
    freq_table[, col] <
      freq_table['column.names'] <- rep(col, nrow(freq_table))
    freq_table['num.total.values'] <- rep(length(factors.df[, col]), nrow(freq_table))
    freq_table <- freq_table[c(3,4,1,2)]
    names(freq_table) <- c('column.names', 'num.total.values', 
                           'factor.levels', 'num.factor.values')
    start.row <- nrow(cat.summary.table)+1
    end.row <- nrow(cat.summary.table)+nrow(freq_table)
    cat.summary.table[start.row:end.row, ] <- freq_table
  }
  return(c(num.analysis.table, cat.analysis.table))
}
