#Lee Przybylski
#11/16/2020
#Explore the hitting data from the baseball cube, clean up to one data frame
#for analysis

library(plyr)
library(dplyr)
library(xtable)
source("ComparisonsWithMLB/CompFunctions.R")
options(stringsAsFactors = T)

Summer0 <- read.csv("ComparisonsWithDI/Przybylski_college_summer_batting.csv", header = TRUE, sep = ",")
ncaa0 <- read.csv("ComparisonsWithDI/przybylski_college_batting_recent.csv", header = TRUE, sep = ",")
summary(Summer0)
summary(ncaa0)
names(ncaa0)
names(Summer0)

ncaa_zeros <- ncaa[ncaa$playerid == 0,]
head(ncaa_zeros)
unique(ncaa_zeros$leagueName)
#Store league abreviations
NCAA_lgs <- data.frame(unique(ncaa0[c("leagueName", "LeagueAbbr")]))
names(NCAA_lgs) <- c("Conference", "Abbr")
write.table(NCAA_lgs, file = "NCAA_confs.csv", row.names = FALSE, col.names = TRUE, sep = ",")
#Remove useless observations and columns
names <- names(ncaa0)[c(1:5,7:25,30:45)]
ncaa <- ncaa0[ncaa0$playerid != 0, names]

#Check for data that does not make sense
table(ncaa$Age)
#We have over 200 players older than 23
#Collect the players info
ncaa$id <- as.factor(ncaa$playerid)
t0 <- Sys.time()
players_ncaa <- ddply(ncaa, .(id, lastName, firstName), college.bios)
tf <- Sys.time()
print(tf - t0)
#Takes about 11 minutes to 1.25 hours
write.table(players_ncaa, file = "D1_hitter_bios.csv", col.names = TRUE, row.names = FALSE, sep = ",")
#players_ncaa <- read.csv("D1_hitter_bios.csv", header = TRUE, sep = ",")
#Check that ids are unique
length(unique(players_ncaa$id))
#There are only 26632 unique ids for 26668 players
T <- table(players_ncaa$id)
head(T)
#Here are the ids with duplicates for players with multiple rookie season entries
dup_ids <- names(T[T>1])
dup_players <- players_ncaa[players_ncaa$id %in% dup_ids,]
#Assign Cardullo to ACC for rookie season
#players_ncaa$r.lg[players_ncaa$lastName == "Cardullo"] <- "ACC"
#Get rid of repeated rows in players df
players_ncaa <- distinct(players_ncaa)
#Remove bio information from the ncaa df
names(ncaa)
names <- names(ncaa)[c(1:4,8:24)]
ncaa <- ncaa[,names]
#Change names to be consistent with lahman names
head(ncaa)
summary(ncaa)
names(ncaa) <- c("playerID", "yearID", "teamID", "lgID", "G", "AB", "R",
                 "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "IBB", 
                 "SO", "SH", "SF", "HBP", "GIDP")
ncaa$playerID <- as.factor(ncaa$playerID)
#Write the clean ncaa data to csv
#write.table(ncaa, file = "D1Batting_2010-2020.csv", col.names = TRUE, row.names = FALSE, sep = ",")
#Read in clean ncaa data
#ncaa <- read.csv("D1Batting_2010-2020.csv", header = TRUE, sep = ",")

####Identify the useable observations from Summer
summary(Summer0)
#We look to combine with ncaa, so we only want players who were eligible in 2010
#Remove observations before 2008 since there are barely any ncaa players in the 
#summer data for earlier seasons
Summer0 <- Summer0[Summer0$year > 2007,]
#We are down to 42648 observations
names(Summer0)
sum(Summer0$playerid != 0)
#There are about 5000 nonzero ids that do not appear in the ncaa data
ncaa_to_summer <- Summer0[Summer0$playerid %in% players_ncaa$id,]
head(ncaa_to_summer)
summary(ncaa_to_summer)
xtable(table(ncaa_to_summer$year))

summer_lgs <- unique(Summer0$LeagueName)
summer0s <- Summer0[Summer0$playerid == 0,]
summary(summer0s)
zero_names <- unique(summer0s[,c("lastname", "firstname")])
zero_name.lg <- unique(summer0s[,c("lastname", "firstname", "LeagueName")])
summer0s1 <- summer0s[order(summer0s$lastname),]
#For each unique first name, last name combo with id 0,
# we try to group observations that are likely to belong to the same player
Summer0$playerid <- as.factor(Summer0$playerid)
head(zero_names)
N <- length(zero_names$lastname)
summer_check <- data.frame(matrix(data = NA, nrow = 0, ncol = 45))
names(summer_check) <- c(names(Summer0), "check")
zero_names$firstname[is.na(zero_names$firstname)] <- ""
Summer0$firstname[is.na(Summer0$firstname)] <- ""
#We define check as
#1 if there is exactly one observation for each season in the range
#2 if the range is longer than the number of observations 
#3 if the range is shorter the number of observations
#4 if the range is longer than 5 years
#Assign a new id containing the the first initial and first 5 letters of last name
for (j in 1:N){
  first <- as.character(zero_names$firstname[j])
  last <- as.character(zero_names$lastname[j])
  indx <- Summer0$firstname == first & Summer0$lastname == last & Summer0$playerid == 0 
  newid <- paste(substr(first, 1,1), substr(last, 1,5), j, sep = "")
  d <- Summer0[indx,]
  d$playerid <- newid
  range <- max(d$year) - min(d$year) + 1
  if (range <= 5){
    if(range == sum(indx)){
      d$check <- 1
    }
    if(range > sum(indx)){
      d$check <- 2
    }
    if(range < sum(indx)){
      d$check <- 3
    }
  }else{
    d$check <- 4
  }
  summer_check <- rbind(summer_check, d)
  print(paste(j, "names done", sep = " "))
}
head(summer_check)
summer_check$check <- as.factor(summer_check$check)
summary(summer_check)

#Take nonzeroids and bind together with observations with check 1
#These observations are good to use
summer_1 <- summer_check[summer_check$check == 1, 1:44]
summer_1 <- rbind(summer_1, Summer0[Summer0$playerid != 0,])
#Look at observations with les observations than years
summer_2 <- summer_check[summer_check$check == "2",]
ids <- unique(summer_2$playerid)

#If the team is consistent for all observations with the name, then leave them alone
for (id in ids){
  d <- summer_2[summer_2$playerid == id,]
  if (length(unique(d$teamName)) <= 1){
    summer_2$check[summer_2$playerid == id] <- "1"
  }
  if (length(unique(d$teamName)) > 1 & length(unique(d$LeagueName)) <= 1){
    summer_2$check[summer_2$playerid == id] <- "3"
  }
} 
summary(summer_2)
summer_21 <- summer_2[summer_2$check == "1",]
summer_23 <- summer_2[summer_2$check == "3",]
summer_22 <- summer_2[summer_2$check == "2",]
#We decide that the ids in 21 and 23 are correct.  
#We must decide which ids in 23 to split and which ids to keep
ids_22 <- unique(summer_22$playerid)
#We have 184 ids that we were unsure if we should split or not.
#We decide to split these ids by league.
summer_22$playerid <- as.character(summer_22$playerid)
ids_22 <- unique(summer_22$playerid)
summer_221 <- data.frame(matrix(data = NA, nrow = 0, ncol = 45))
names(summer_221) <- c(names(summer_22))
for (id in ids_22){
  d <- summer_22[summer_22$playerid == id,]
  lgs <- unique(d$LeagueName)
  L <- 1
  for (lg in lgs){
    d$playerid[d$LeagueName == lg] <- paste(d$playerid[d$LeagueName == lg], "L", L, sep = "")
    L <- L+1
  }
  summer_221 <- rbind(summer_221, d)
}
length(c(summer_221$playerid, summer_21$playerid, summer_23$playerid)) 
summary(summer_check)
#Attach all of the category 2 summer observations with the correct ids to summer_1:
#Check the length first
length(summer_check$playerid[summer_check$check == 1]) + length(Summer0$playerid[Summer0$playerid != 0])
#This should equal 4094, then we can add the other rows with checked ids
summer_1 <- rbind(summer_1, summer_21[,1:44])
summer_1 <- rbind(summer_1, summer_23[,1:44])
summer_1 <- rbind(summer_1, summer_221[,1:44])
#Check the length
length(Summer0$playerid[Summer0$playerid != 0]) + sum(summer_check$check %in% c("1", "2"))
length(summer_1$playerid)
#Since they match, we have successfully assigned an id for all category 1 and 2 players
#We move on to category 3 players, who had more observations than seasons
summer_3 <- summer_check[summer_check$check == 3,]
head(summer_3)
ids <- unique(summer_3$playerid)

#If the number of leagues is equal to the number of years, these ids are ok
for (id in ids){
  d <- summer_3[summer_3$playerid == id,]
  range <- max(d$year) - min(d$year) + 1
  n.lgs <- length(unique(d$LeagueName))
  if (range >= n.lgs){
    summer_3$check[summer_3$playerid == id] <- "1"
  }
} 
summary(summer_3)
summer_31 <- summer_3[summer_3$check == 1,]
summer_33 <- summer_3[summer_3$check != 1,]
unique(summer_33$playerid)
#Save the alex rodriguez example for the notes
arods <- summer_33[summer_33$playerid == "ARodri4013",2:6]
xtable(arods)
#Sort summer_33 by grouping observations of the same id and same league
ids_33 <- unique(summer_33$playerid)
summer_331 <- data.frame(matrix(data = NA, nrow = 0, ncol = 45))
names(summer_331) <- c(names(summer_33))
for (id in ids_33){
  d <- summer_33[summer_33$playerid == id,]
  lgs <- unique(d$LeagueName)
  L <- 1
  for (lg in lgs){
    d$playerid[d$LeagueName == lg] <- paste(d$playerid[d$LeagueName == lg], "L", L, sep = "")
    L <- L+1
  }
  summer_331 <- rbind(summer_331, d)
}
#Now we add summer_31 and summer_331 to summer_1
summer_1 <- rbind(summer_1, summer_31[,1:44])
summer_1 <- rbind(summer_1, summer_331[,1:44])
#check the length
length(summer_1$playerid)
length(Summer0$playerid[Summer0$playerid != 0]) + sum(summer_check$check %in% c("1", "2", "3"))
#They both contain 41915 observations so we are good
#Now check category 4 ids
summer_4 <- summer_check[summer_check$check == 4,]
write.table(summer_4, "summer_4.csv", row.names = F, col.names = T, sep = ",")
summer_44 <- read.csv("summer_4.csv", header = TRUE, sep = ",")
summer_44$level <- as.character(summer_44$level)
summer_44$level[is.na(summer_44$level)] <- "1"
summer_441 <- summer_44[,2:45]
head(summer_441)
head(summer_1)
summer_441$playerid <- paste(summer_44$playerid, summer_44$level, sep = "S")
blanks <- summer_1[1, 28:44]
summer_441[,28:44] <- blanks
#Combine the newly id'd observations of summer_4 with summer_1
summer_1 <- rbind(summer_1, summer_441)
#The number of obs in summer_1 match our original count in Summer0, but now associated obs have the same
#playerid

#Find a way to extract colleges from summer_1$colleges
summer_1$colleges <- as.character(summer_1$colleges)
head(summer_1$colleges)
#Collect all the schools with conferences and year
NCAA_schools <- unique(ncaa0[,2:4])
head(NCAA_schools)
NCAA_schools <- NCAA_schools[order(NCAA_schools$teamName, NCAA_schools$year),]
#write.table(NCAA_schools, "DI_schools.csv", row.names = FALSE, col.names = TRUE, sep = ",")
sample_strs <- unique(summer_1$colleges)[2:11]
sample2 <- gsub("^.*?>","", sample_strs)
gsub("\\(.*?(?:\\)|$)", "", sample2)
sample3 <- gsub("\\*<?$","",sample2)

#nonzero summerids
indx <- grepl("\\d", substr(summer_1$playerid, 1, 1))
sum(indx)
#We have 27328 observations with nonzero player ids, belonging to 17208 unique players
ids_summer <- unique(summer_1$playerid[indx])
sum(ids_summer %in% players_ncaa$id)
ids_summer_only <- ids_summer[!(ids_summer %in% players_ncaa$id)]
indx <- summer_1$playerid %in% ids_summer_only
#Get school names for players in indx
need_school <- summer_1[indx,]
head(need_school)
strings <- unique(need_school$colleges)
summary(summer)
schools <- unique(as.character(NCAA_schools$teamName))
n.schools <- rep(0, length(need_school$playerid))
need_school$cut1 <- gsub("^.*?>","", need_school$colleges)
need_school$cut2 <- gsub("\\(.*?(?:\\)|$)", "", need_school$cut1)
for(school in schools){
  indicator <- grepl(school, need_school$cut2)
  n.schools[indicator] <- n.schools[indicator] + 1
}
n.schools <- rep(0, length(need_school$playerid))
match1 <- rep(NA, length(n.schools))
matchf <- rep(NA, length(n.schools))

for(school in schools){
  indicator <- grepl(school, need_school$cut2) 
  match1[indicator & n.schools == 0] <- school
  n.schools[indicator] <- n.schools[indicator] + 1
  matchf[indicator] <- school 
}
matchdf <- data.frame(cbind(need_school$colleges, n.schools, match1, matchf))
summary(matchdf)
match1s <- matchdf[matchdf$n.schools == 1,]
length(unique(matchdf[,1]))

dist_matchdf <- distinct(matchdf)
write.table(dist_matchdf, file = "colleges_df.csv", row.names = F, col.names = TRUE, sep = ",")
#How many times does colleges for non-ncaa id players match an entry in identified players
ncaa_to_summer$colleges <- as.character(ncaa_to_summer$colleges)
sum(dist_matchdf$V1 %in% ncaa_to_summer$colleges)
dist_matchdf <- read.csv("colleges_df.csv", header = TRUE, sep = ",")
head(dist_matchdf)
dist_matchdf$match1 <- as.character(dist_matchdf$match1)
dist_matchdf$matchf <- as.character(dist_matchdf$matchf)
dist_matchdf$school <- NA
dist_matchdf$check[is.na(dist_matchdf$check)] <- 0
indx1 <- dist_matchdf$check == 1
dist_matchdf$school[indx1] <- dist_matchdf$match1[indx1]
indx2 <- dist_matchdf$check == 2
dist_matchdf$school[indx2] <- dist_matchdf$matchf[indx2]
dist_matchdf$check[dist_matchdf$n.schools == 0] <-3
indx3 <- dist_matchdf$check == 3
dist_matchdf$school[indx3] <- "non-DI"
#We write in Florida Intl and LSU
indx <- grepl("Florida International University", dist_matchdf$V1)
dist_matchdf$school[indx] <- "Florida Intl"
indx <- grepl("Louisiana State University", dist_matchdf$V1) & grepl("Baton Rouge, Louisiana", dist_matchdf$V1)
dist_matchdf$school[indx] <- "LSU"
sum(is.na(dist_matchdf$school))
#We still cannot classify 691 of the ids
#We add schools to the ncaa_to_summer df
ncaa_to_summer$playerid <- as.character(ncaa_to_summer$playerid)
players_ncaa$id <- as.character(players_ncaa$id)
players_ncaa$r.tm <- as.character(players_ncaa$r.tm)
ncaa_to_summer$school <- NA
j <- 1
for (id in ncaa_to_summer$playerid){
  v <- players_ncaa$r.tm[players_ncaa$id == id]
  ncaa_to_summer$school[j] <- v[1]
  j <- j+1
}
college_key <- ncaa_to_summer[ncaa_to_summer$colleges %in% dist_matchdf$V1, c("colleges", "school")]
college_key <- distinct(college_key)
j <- 1
for (school in dist_matchdf$school){
  if (is.na(school)){
    txt <- dist_matchdf$V1[j]
    v <- college_key$school[college_key$colleges == txt]
    if (length(v) >0){
      dist_matchdf$school[j] <- v[1]
    }
  }
  j <- j+1
}
#With this, we were able to get school assignments for all but 558 strings
#First gather a school for every ID we can
head(dist_matchdf)
dist_matchdf$colleges <- as.character(dist_matchdf$V1)
college_key <- rbind(college_key, dist_matchdf[,c("colleges", "school")])
college_key <- distinct(college_key)
#We save a data frame of ids and colleges
get.college <- function(pid){
  y <- "non-DI"
  if (grepl("\\d", substr(pid, 1,1))){
    if (pid %in% players_ncaa$id){
      v <- players_ncaa$r.tm[players_ncaa$id == pid]
      y <- v[1]
    }else{
      txt <- summer_1$colleges[summer_1$playerid == pid]
      txt <- txt[1]
      v <- college_key$school[college_key$colleges == txt]
      y <- v[1]
    }
  }
  return(y)
}
#Assign a school to each observation in summer_1
summer_1$school <- NA
N <- length(summer_1$school)
for (j in 1:N){
  summer_1$school[j] <- get.college(summer_1$playerid[j])
  print(paste(j, "schools done"))
}
sum(is.na(summer_1$school))
length(unique(summer_1$playerid[is.na(summer_1$school)]))
#We have 722 observations from players that we were not able to assign a school
summer1 <- summer_1[!is.na(summer_1$school),]
#We now collect bio information for each unique id in summer1
#summer1$id <- as.factor(summer1$playerid)
t0 <- Sys.time()
players_summer <- ddply(summer1, .(id, lastname, firstname), summer.bios)
tf <- Sys.time()
print(tf - t0)
#Takes about 11 minutes to 1.25 hours
write.table(players_summer, file = "summer_hitter_bios.csv", col.names = TRUE, row.names = FALSE, sep = ",")
#Remove bio information from the summer1 df
names(summer1)
names <- names(summer1)[c(1:4,7:23)]
summer <- summer1[,names]
#Change names to be consistent with lahman names
head(summer)
summary(summer)
names(summer) <- c("playerID", "yearID", "teamID", "lgID", "G", "AB", "R",
                 "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "IBB", 
                 "SO", "SH", "SF", "HBP", "GIDP")
summer$playerID <- as.factor(summer$playerID)
#Write the clean summer data to csv
write.table(summer, file = "summerBatting_2008-2019.csv", col.names = TRUE, row.names = FALSE, sep = ",")
#We now have stored data frames of hitting and bio stats for summer and ncaa data

#Check that overlapping players have similar bio information
overlap_summer <- players_summer$id %in% players_ncaa$id 
overlap_ncaa <- players_ncaa$id %in% players_summer$id
sum(overlap_summer)
head(players_summer[overlap_summer,])
head(players_ncaa[overlap_ncaa,])
names(players_ncaa)
ncaa_overlap <- players_ncaa[overlap_ncaa, c(1:3)]
ncaa_overlap <- ncaa_overlap[order(ncaa_overlap$id),]
summer_overlap <- players_summer[overlap_summer, c(1:3)]
summer_overlap <- summer_overlap[order(summer_overlap$id),]
summer_overlap$firstname <- as.character(summer_overlap$firstname)
summer_overlap$lastname <- as.character(summer_overlap$lastname)
ncaa_overlap$firstName <- as.character(ncaa_overlap$firstName)
ncaa_overlap$lastName <- as.character(ncaa_overlap$lastName)
sum(ncaa_overlap$firstName != summer_overlap$firstname)
sum(ncaa_overlap$draft.overall != summer_overlap$draft.overall)
overlaps <- cbind(ncaa_overlap, summer_overlap)
overlaps[ncaa_overlap$firstName != summer_overlap$firstname,]
#The discrepencies in first names are fine, they just used nicknames
overlaps[ncaa_overlap$lastName != summer_overlap$lastname,]
#The discrepancies in lastnames is due to a capitalization error in the ncaa data

#We now prepare to merge the two data frames
#trim ncaa_players
names(players_ncaa)
p_ncaa <- players_ncaa[,c("id", "r.ssn", "r.tm", "r.lg", "f.ssn", "f.tm", "f.lg", "draft.rd", "draft.overall")]
p_summer <- players_summer[,c("id", "r.ssn.summer", "r.tm.summer", "r.lg.summer", "f.ssn.summer", "f.tm.summer",
                              "f.lg.summer", "school", "draft.rd", "draft.overall")]
players <- merge(p_ncaa, p_summer, by =c("id", "draft.rd", "draft.overall"))
summary(players)
#This only merged the observations from both sets
p_ncaa <- players_ncaa[!overlap_ncaa,c("id", "r.ssn", "r.tm", "r.lg", "f.ssn", "f.tm", "f.lg", "draft.rd", "draft.overall")]
p_summer <- players_summer[!overlap_summer,c("id", "r.ssn.summer", "r.tm.summer", "r.lg.summer", "f.ssn.summer", "f.tm.summer",
                              "f.lg.summer", "school", "draft.rd", "draft.overall")]
p_ncaa$r.ssn.summer <- NA; p_ncaa$r.tm.summer <- NA
p_ncaa$r.lg.summer <- NA; p_ncaa$f.ssn.summer <- NA; p_ncaa$f.tm.summer <- NA
p_ncaa$f.lg.summer <- NA; p_ncaa$school <- p_ncaa$r.tm
p_summer$r.ssn <- p_summer$r.ssn.summer; p_summer$r.tm <- p_summer$school;
p_summer$r.lg <- NA; p_summer$f.ssn <- p_summer$f.ssn.summer; p_summer$f.tm <- p_summer$school
p_summer$f.lg <- NA
names(p_summer)
names(p_ncaa)
names(players)
players <- rbind(players, p_ncaa, p_summer)
summary(players)
#We assign missing values for r.lg and f.lg using the NCAA_schools data
players$f.lg <- as.character(players$f.lg)
players$f.lg[players$school == "non-DI"] <- "non-DI"
players$f.lg <- as.factor(players$f.lg)
players$r.lg <- as.character(players$r.lg)
players$r.lg[players$school == "non-DI"] <- "non-DI"
players$r.lg <- as.factor(players$r.lg)
N <- length(players$r.lg)
for (j in 1:N){
  conf <- players$r.lg[j]
  if (is.na(conf)){
    school <- as.character(players$school[j])
    yr1 <- players$r.ssn[j]
    yr2 <- players$f.ssn[j]
    lg1 <- NCAA_schools[ (NCAA_schools$teamName) == school, "LeagueAbbr"]
    lg1 <- lg1[length(lg1)]
    lg2 <- NCAA_schools[(NCAA_schools$teamName) == school, "LeagueAbbr"]
    lg2 <- lg2[length(lg2)]
    players$r.lg[j] <- lg1
    players$f.lg[j] <- lg2
  }
}
summary(players)
#Store the combined hitting and player data sets
write.table(players, "college_hitter_bios.csv", row.names = FALSE, col.names = TRUE, sep = ",")
batting <- rbind(ncaa, summer)
write.table(batting, "college_Batting_2008-2020.csv", row.names = FALSE, col.names = TRUE, sep = ",")
