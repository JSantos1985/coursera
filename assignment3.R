df <- read.csv('outcome-of-care-measures.csv', na.strings="Not Available", stringsAsFactors=FALSE)
outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)

best <- function(state, outcome) {
  if (state %in% df$State == FALSE) {
    stop("invalid state")
  }
  if (outcome %in% names(outcomes) == FALSE) {
    stop("invalid outcome")
  } else {
    df <- na.omit(df[, c(2,7,outcomes[outcome])])
    df <- df[order(df[2], df[3], df[1]),]
  }
  m <- subset(df, State == state)
  print(m[1,1])
}
## SORTING ALPHABETICALLY IN FUNCTION 1

rankhospital <- function(state, outcome, num="best") {
  if (num=="best") {num=1}
  if (num=="worst") {
    srt_desc=TRUE
    num=1
  } else {
    srt_desc=FALSE
  }
  if (state %in% df$State == FALSE) {
    stop("invalid state")
  }
  if (outcome %in% names(outcomes) == FALSE) {
    stop("invalid outcome")
  } else {
    df <- na.omit(df[, c(2,7,outcomes[outcome])])
    df <- df[order(df[2], df[3], df[1], decreasing = srt_desc),]
  }
  m <- subset(df, State == state)
  print(m[num,1])

}

rankall <- function(outcome, num = "best") {
  if (num=="best") {num=1}
  if (num=="worst") {
    srt_desc=TRUE
    num=1
  } else {
    srt_desc=FALSE
  }
  if (outcome %in% names(outcomes) == FALSE) {
    stop("invalid outcome")
  } else {
    df <- na.omit(df[, c(2,7,outcomes[outcome])])
    df <- df[order(df[2], df[3], df[1], decreasing = srt_desc),]
  }

  out_df <- data.frame()
  out_df <- do.call("rbind", lapply(split(df, df$State), function(x) c(x[1,1:2])))
  print(out_df)
}