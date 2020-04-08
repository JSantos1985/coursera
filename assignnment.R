pollutantmean <- function(directory, pollutant, id = 1:332) {
  files = list.files(file.path("./", directory), pattern=".csv", full.names=T)
  df <- do.call("rbind", lapply(files[id], read.csv))
  mean(df[[pollutant]], na.rm=T)
}

complete <- function(directory, id = 1:332) {
  files = list.files(file.path("./", directory), pattern=".csv", full.names=T)
  df <- do.call("rbind", lapply(files[id], read.csv))
  df_missing <- data.frame()
  for (i in id) {
    df_filtered <- subset(df, ID == i)
    #df_missing = rbind(id = 1, nobs = 0)
    id = i
    nobs_sulfate = colSums(!is.na(df_filtered))[["sulfate"]]
    nobs_nitrate = colSums(!is.na(df_filtered))[["nitrate"]]
    nobs = pmin(nobs_sulfate, nobs_nitrate)
    df_missing = rbind(df_missing, data.frame(id, nobs))
  }
  df_missing
}

corr <- function(directory, threshold = 0) {
  list_of_missings <- complete(directory)
  list_of_ids <- c(subset(list_of_missings, nobs > threshold)[["id"]])
  files = list.files(file.path("./", directory), pattern=".csv", full.names=T)
  df <- do.call("rbind", lapply(files, read.csv))
  df <- df[df$ID %in% list_of_ids, ]
  corrs_vector <- numeric()
  for (i in list_of_ids) {
    working_df <- subset(df, ID == i)
    corrs_vector <- c(corrs_vector, cor(working_df["sulfate"], working_df["nitrate"], use="pairwise.complete.obs"))
  }
  print(corrs_vector)
}