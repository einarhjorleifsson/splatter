# https://stackoverflow.com/questions/23342647/how-to-match-by-nearest-date-from-two-data-frames
# https://stackoverflow.com/questions/28072542/merge-nearest-date-and-related-variables-from-a-another-dataframe-by-group?noredirect=1&lq=1
library(tidyverse)
lgs <-
  tibble(vid = 9999,
         lon = seq(-30, -29, by = 0.01),
         lat = seq(64, 65, by = 0.01))


set.seed(42)
df1 <- data.frame(ID = sample(1:3, 10, rep=T),
                  dateTarget=(strptime((paste
                                        (sprintf("%02d", sample(1:30,10, rep=T)), sprintf("%02d", sample(1:12,10, rep=T)),
                                          (sprintf("%02d", sample(2013:2015,10, rep=T))), sep="")),"%d%m%Y")), Value=sample(15:100, 10, rep=T))
df2 <- data.frame(ID = sample(1:3, 10, rep=T),
                  dateTarget=(strptime((paste
                                        (sprintf("%02d", sample(1:30,20, rep=T)), sprintf("%02d", sample(1:12,20, rep=T)),
                                          (sprintf("%02d", sample(2013:2015,20, rep=T))), sep="")),"%d%m%Y")), ValueMatch=sample(15:100, 20, rep=T))

library(data.table)

setDT(df1)
setDT(df2)

setkey(df2, ID, dateTarget)[, dateMatch:=dateTarget]
df2[df1, roll='nearest']

lgs2 <-
  lgs %>%
  select(vid, id, t1, t2) %>%
  gather(event, match, t1:t2) %>%
  arrange(vid, id)
vms2 <-
  vms %>%
  select(vid, time)
setDT(lgs2)
setDT(vms2)
setkey(vms2, vid, time)[, matchtime:=time]
lgs2[vms2, roll = 'nearest']



library(survival)
data1 <- data.frame(id = 1:10,
                    entry.dt = as.Date(paste("2011", 1:10, "5", sep='-')))
temp1 <- c(1,4,5,1,3,6,9, 2,7,8,12,4,6,7,10,12,3)
data2 <- data.frame(id = c(1,1,1,2,2,4,4,5,5,5,6,8,8,9,10,10,12),
                    lab.dt = as.Date(paste("2011", temp1, "1", sep='-')),
                    chol = round(runif(17, 130, 280)))

#first cholesterol on or after enrollment
indx1 <- neardate(data1$id, data2$id, data1$entry.dt, data2$lab.dt)
data2[indx1, "chol"]

# Closest one, either before or after.
#
indx2 <- neardate(data1$id, data2$id, data1$entry.dt, data2$lab.dt,
                  best="prior")
ifelse(is.na(indx1), indx2, # none after, take before
       ifelse(is.na(indx2), indx1, #none before
              ifelse(abs(data2$lab.dt[indx2]- data1$entry.dt) <
                       abs(data2$lab.dt[indx1]- data1$entry.dt), indx2, indx1)))

# closest date before or after, but no more than 21 days prior to index
indx2 <- ifelse((data1$entry.dt - data2$lab.dt[indx2]) >21, NA, indx2)
ifelse(is.na(indx1), indx2, # none after, take before
       ifelse(is.na(indx2), indx1, #none before
              ifelse(abs(data2$lab.dt[indx2]- data1$entry.dt) <
                       abs(data2$lab.dt[indx1]- data1$entry.dt), indx2, indx1)))