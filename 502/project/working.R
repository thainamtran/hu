h1b_kaggle <- read.csv("C:/Users/thai/Documents/workspace-new/hu/502/project/h1b_kaggle.csv", header=T)
str(h1b_kaggle)

head(h1b_kaggle[0..2])

library('plyr')
#h1b <- h1b_kaggle[h1b_kaggle['YEAR']==2016,]
h1b <- h1b_kaggle[h1b_kaggle['YEAR']==2016,]
str(h1b)
head(h1b)

job_titles <- count(h1b, c('JOB_TITLE'))
job_titles_sorted <- job_titles[order(-job_titles$freq),]
job_titles_10 <- job_titles_sorted[0:10,]
nrow(job_titles_10)
job_titles_10
barplot(job_titles_10$freq, ylab='freq',names.arg = job_titles_10$JOB_TITLE,cex.names=0.55,las=1.5)


employers <- count(h1b, c('EMPLOYER_NAME'))
employers_sorted <- employers[order(-employers$freq),]
employers_10 <- employers_sorted[1:10,]
employers_10
barplot(employers_10$freq, ylab='freq',names.arg = employers_10$EMPLOYER_NAME,cex.names=0.5,las=1.5)

h1b <- h1b_kaggle
h1b$state <- trimws(gsub("^.*,", "", h1b$WORKSITE))
states <- count(h1b, c('state','YEAR'))
head(states)
unique(states$state)
state_names <- c('CALIFORNIA', 'NEW YORK', 'TEXAS', 'NEW JERSEY', 'FLORIDA', 'VIRGINIA', 'PENNSYLVANIA','OHIO','ILLINOIS','WASHINGTON')
df <- data.frame(State=character(), Year2011=integer(), Year2012=integer(), Year2013=integer(), Year2014=integer(), Year2015=integer(),Year2016=integer())
for(state in state_names){
  row <- states[states$state==state,3]
  row <- c(state, row)
  newrow <- data.frame(State=row[1], Year2011=row[2], Year2012=row[3],Year2013=row[4],Year2014=row[5],Year2015=row[6],Year2016=row[7])
  df <- rbind(df, newrow)
}
df
matplot(df, type = c('b'), pch=1,col=2:7)



h1b <- h1b_kaggle
h1b$state <- trimws(gsub("^.*,", "", h1b$WORKSITE))
state_names <- c('CALIFORNIA', 'NEW YORK', 'TEXAS', 'NEW JERSEY', 'FLORIDA', 'VIRGINIA', 'PENNSYLVANIA','OHIO','ILLINOIS','WASHINGTON')
state_names <- state_names[order(state_names)]
states <- count(h1b[h1b$state %in% state_names,], c('state','YEAR'))
states <- states[states$YEAR %in% 2011:2016,]
states_sorted <- states[order(states$state),]
df <- data.frame(Year=integer(), 'CALIFORNIA'=integer(), 'FLORIDA'=integer(), 'ILLINOIS'=integer(), 'NEW JERSEY'=integer(), 'NEW YORK'=integer(), 'OHIO'=integer(), 'PENNSYLVANIA'=integer(),'TEXAS'=integer(),'VIRGINIA'=integer(),'WASHINGTON'=integer())
for(year in 2011:2016){
  row <- states[states$YEAR==year,3]
  row <- c(year, row)
  newrow <- data.frame(Year=row[1],'CALIFORNIA'=row[2],'FLORIDA'=row[3],'ILLINOIS'=row[4],'NEW JERSEY'=row[5],'NEW YORK'=row[6],'OHIO'=row[7],'PENNSYLVANIA'=row[8],'TEXAS'=row[9],'VIRGINIA'=row[10],'WASHINGTON'=row[11])
  df <- rbind(df, newrow)
}
matplot(df, type = c('b'), pch=1,col=2:7)
legend("topleft", legend =state_names, col=11:2, pch=1)
