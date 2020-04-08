df <- read.csv('outcome-of-care-measures.csv', na.strings="Not Available", stringsAsFactors=FALSE)
outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)

best <- function(state, outcome) {
  if (state %in% df$State == FALSE) {
    stop("invalid state")
  }
  m <- subset(df, State == state)
  if (outcome == "heart attack") {
    col_ref <- colnames(df)[11]
  } else if (outcome == "heart failure") {
    col_ref <- colnames(df)[17]
  } else if (outcome == "pneumonia") {
    col_ref <- colnames(df)[25]
  } else {
    stop("invalid outcome")
  }
  
  m[, col_ref] <- as.numeric(m[, col_ref])
  minimum <- min(m[, col_ref], na.rm=T)
  m <-subset(m, m[, col_ref] == minimum)
  print(m[, 2])
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
  if (outcome == "heart attack") {
    col_ref <- 11
  } else if (outcome == "heart failure") {
    col_ref <- 17
  } else if (outcome == "pneumonia") {
    col_ref <- 25
  } else {
    stop("invalid outcome")
  }
  m <- subset(df, State == state)
  m[, col_ref] <- as.numeric(m[, col_ref])
  m <- m[order(m[,col_ref], m[2], decreasing = srt_desc),]
  print(m[num,2])

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
  #list_of_states <- unique(na.omit(df[2]))
  #output_df <- data.frame()
  hospitals <- vector()
  states <- vector()
  #print(list_of_states)
  split.data <- split(df, df$State)
  for (i in split.data) {
    hospitals <- c(hospitals, i[num,1])
    states <- c(states, i[num,2])
    #output_df <- rbind(output_df, (c(i[num,1], i[num,2])))
  }
  #print(hospitals)
  #print(states)
  output_df <- data.frame(hospitals, states)
  print(output_df)
}