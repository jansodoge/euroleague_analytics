library(rjson)
library(tidyr)
library(dplyr)
library(WriteXLS)





#' Download shooting data for all games for a season
#'
#' @param limit Number of games to download from the season, if you wish all games, enter 300
#' @param start starting point of games to download, enter 1 if you wish all games
#' @param year Enter the year for the respective season
#'
#' @return
#' @export Creates .csv files in the current directory for each game
#'
#' @examples
get_all_games_shooting <- function(limit, start, year){
while(start <= limit){
		try(get_shooting_data(start, year))
		start <- start +1 		
	}

	
}

#' Download play-by-play data for all games for a season
#'
#' @param limit Number of games to download from the season, if you wish all games, enter 300
#' @param start starting point of games to download, enter 1 if you wish all games
#' @param year Enter the year for the respective season
#'
#' @return
#' @export Creates .csv files in the current directory for each game
#'
#' @examples
get_all_games <- function(limit, start, year){
	while(start <= limit){
		try(get_game(start, year))
		start <- start +1 		
	}
	
	}

#' Get play by play data for a specific game
#'
#' @param nr for the respective game you want to scrape
#' @param year of the respective game you want to scrape
#'
#' @return
#' @export data frame with pbp data
#'
#' @examples
get_game <- function(nr, year){
link <- paste("http://live.euroleague.net/api/PlayByPlay?gamecode=",nr,"&seasoncode=E", year)
link <- gsub(" ","", link)

json_test <- fromJSON(paste(readLines(link), collapse=""))


#get first quarter
len <- length(json_test$FirstQuarter)
count <- 1
frame <- data.frame()
while(count <= len){
actual <- json_test$FirstQuarter[[count]]	

count_2 <- 1
len_2 <- length(actual)
actual_vec <- c()
while(count_2 <= len_2){

if(is.null(actual[[count_2]])){
	actual_vec[count_2] <- "NA" 
	}
else{	
actual_vec[count_2] <- actual[[count_2]]	
}
count_2 <- count_2 +1 
}
current_frame <- as.data.frame(t(actual_vec))	
frame <- rbind(frame, current_frame)
count <- count +1
actual_vec <- NULL
}
first_q <- frame


#second quarter
len <- length(json_test$SecondQuarter)
count <- 1
frame <- data.frame()
while(count <= len){
actual <- json_test$SecondQuarter[[count]]	

count_2 <- 1
len_2 <- length(actual)
actual_vec <- c()
while(count_2 <= len_2){

if(is.null(actual[[count_2]])){
	actual_vec[count_2] <- "NA" 
	}
else{	
actual_vec[count_2] <- actual[[count_2]]	
}
count_2 <- count_2 +1 
}
current_frame <- as.data.frame(t(actual_vec))	
frame <- rbind(frame, current_frame)
count <- count +1
actual_vec <- NULL
}
second_q <- frame


#third quarter
len <- length(json_test$ThirdQuarter)
count <- 1
frame <- data.frame()
while(count <= len){
actual <- json_test$ThirdQuarter[[count]]	

count_2 <- 1
len_2 <- length(actual)
actual_vec <- c()
while(count_2 <= len_2){

if(is.null(actual[[count_2]])){
	actual_vec[count_2] <- "NA" 
	}
else{	
actual_vec[count_2] <- actual[[count_2]]	
}
count_2 <- count_2 +1 
}
current_frame <- as.data.frame(t(actual_vec))	
frame <- rbind(frame, current_frame)
count <- count +1
actual_vec <- NULL
}
third_q <- frame


#forth quarter
len <- length(json_test$ForthQuarter)
count <- 1
frame <- data.frame()
while(count <= len){
actual <- json_test$ForthQuarter[[count]]	

count_2 <- 1
len_2 <- length(actual)
actual_vec <- c()
while(count_2 <= len_2){

if(is.null(actual[[count_2]])){
	actual_vec[count_2] <- "NA" 
	}
else{	
actual_vec[count_2] <- actual[[count_2]]	
}
count_2 <- count_2 +1 
}
current_frame <- as.data.frame(t(actual_vec))	
frame <- rbind(frame, current_frame)
count <- count +1
actual_vec <- NULL
}
forth_q <- frame

#get meta information
info_link <- paste("http://live.euroleague.net/api/Header?gamecode=",nr,"&seasoncode=E",year,"&disp=")
info_link <- gsub(" ","", info_link)
json_info <- fromJSON(paste(readLines(info_link), collapse=""))
compet <- json_info$com
date <- json_info$dat


#_____________________________________________________
final <- rbind(first_q, second_q, third_q, forth_q)
final$competition <- compet
final$datum <- date
basis <- levels(final$V7)
dateiname <- paste(basis[1], basis[2], basis[3], nr, ".xls")
dateiname <- gsub(" ","", dateiname)
dateiname <- gsub("NA","", dateiname)
print(dateiname)
WriteXLS(final, dateiname)

}

#' Download shooting data for a specific game
#'
#' @param nr of the game you wish to download
#' @param year of the game you wish to download
#'
#' @return
#' @export data frame with shooting data
#'
#' @examples
get_shooting_data <- function(nr, year){
library(rjson)
library(tidyr)
library(dplyr)
library(WriteXLS)
	
	
link <- paste("http://live.euroleague.net/api/Points?gamecode=",nr,"&seasoncode=E", year)
link <- gsub(" ","", link)
json_test <- fromJSON(paste(readLines(link), collapse=""))
len <- length(json_test$Rows)	
count <- 1	
frame <- data.frame()




while(count <= len){
current <- json_test$Rows[[count]]
current <- as.vector(current)
size <- length(current)
counter <- 1
vector <- c()
while(counter <= size){
if(is.null(current[[counter]])){
vector[counter] <- "NA"	
}	
else{
vector[counter] <- current[[counter]]
}
counter <- counter +1	
}
current_frame <- as.data.frame(t(vector))
frame <- rbind(frame, current_frame)
count <- count +1



}	
#get meta information
info_link <- paste("http://live.euroleague.net/api/Header?gamecode=",nr,"&seasoncode=E",year,"&disp=")
info_link <- gsub(" ","", info_link)
json_info <- fromJSON(paste(readLines(info_link), collapse=""))
compet <- json_info$com
date <- json_info$dat
frame$competition <- compet
frame$datum <- date


print(nrow(count))
teams <- levels(frame$V2)

dateiname <- paste("Shooting_", json_info$tA,"_", json_info$tB,nr, ".xls")
dateiname <- gsub(" ","", dateiname)
dateiname <- gsub("NA","", dateiname)


WriteXLS(frame, dateiname)
print(dateiname)	
	
}
