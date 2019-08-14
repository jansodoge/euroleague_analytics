library(gdata)
library(dplyr)

source('~/Desktop/Basketball/Statistics/euroleague_pbp_script.R', chdir = TRUE)

#' Identify and create lineup data
#'
#' @param frame the dataframe containing play-by-play data you wish to get lineup data for
#'
#' @return dataframe entered adjusted by lineup information in additonal columns
#' @export
#'
#' @examples
euroleague_lineups <- function(frame){
	starter <- c()
	len <- nrow(frame)
	cast <- as.vector(frame$V6)
	cast <- unique(cast)	
	cast <- cast[!is.na(cast)]
	for(elem in cast){
		#get the number of times one player was subbed out
		out_subbed <- dplyr::filter(frame, V5 == "OUT")
		out_subbed <- dplyr::filter(out_subbed, V6 == elem)
		out_nr <- nrow(out_subbed)
		in_subbed <- dplyr::filter(frame, V5 == "IN")
		in_subbed <- dplyr::filter(in_subbed, V6 == elem)
		in_nr <- nrow(out_subbed)
		
		
		if((in_nr + out_nr) == 0){
		starter <- append(starter, elem)

		}
		
		
		else if(in_nr ==  out_nr){
			#get the information on if the player was subbed out or in first
			time_first_in <- in_subbed$V2[1]
			time_first_in <- as.integer(time_first_in)
			time_first_out <- out_subbed$V2[1]
			time_first_out <- as.integer(time_first_out)
			if(is.na(time_first_out)){
				time_first_out <- 0
			}
			if(is.na(time_first_in)){
				time_first_in <- 0
			}
			
			
			if(time_first_in > time_first_out){
				starter <- append(starter, elem)
				
			}
		}
		else if(out_nr > 0 && in_nr == 0){
			starter <- append(starter, elem)
			
		}
				
				
		else if(in_nr + out_nr > 0 && out_nr > in_nr){
		starter <- append(starter, elem)

		
	}				
		
	
	
			
			
			
			
		}
	ergebnisse <- length(starter)		
	return(starter)
}



#' Supply method for euroleague_lineups
#'
#' @param elem 
#'
#' @return
#' @export
#'
#' @examples
listen <- function(elem){
	
		
		
		countend <- 30
		et <- 0
		while(length(et) < 10){
			
			daten <- dplyr::filter(elem, V2 < countend)
			et <- euroleague_lineups(daten)
			countend <- countend +1
		}
		elem$lineup_1[1] <- et[1]
		elem$lineup_2[1] <- et[2]
		elem$lineup_3[1] <- et[3]
		elem$lineup_4[1] <- et[4]
		elem$lineup_5[1] <- et[5]
		elem$lineup_6[1] <- et[6]
		elem$lineup_7[1] <- et[7]
		elem$lineup_8[1] <- et[8]
		elem$lineup_9[1] <- et[9]
		elem$lineup_10[1] <- et[10]	

		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_1[index])){
				elem$lineup_1[index+1] <- elem$lineup_1[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_1[index]){
				elem$lineup_1[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_1[index+1] <- elem$lineup_1[index]
			}
			index <- index +1
			
		}
		
		
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_2[index])){
				elem$lineup_2[index+1] <- elem$lineup_2[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_2[index]){
				elem$lineup_2[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_2[index+1] <- elem$lineup_2[index]
			}
			index <- index +1
			
		}
		
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_3[index])){
				elem$lineup_3[index+1] <- elem$lineup_3[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_3[index]){
				elem$lineup_3[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_3[index+1] <- elem$lineup_3[index]
			}
			index <- index +1
			
		}
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_4[index])){
				elem$lineup_4[index+1] <- elem$lineup_4[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_4[index]){
				elem$lineup_4[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_4[index+1] <- elem$lineup_4[index]
			}
			index <- index +1
			
		}
		
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_5[index])){
				elem$lineup_5[index+1] <- elem$lineup_5[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_5[index]){
				elem$lineup_5[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_5[index+1] <- elem$lineup_5[index]
			}
			index <- index +1
			
		}
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_6[index])){
				elem$lineup_6[index+1] <- elem$lineup_6[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_6[index]){
				elem$lineup_6[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_6[index+1] <- elem$lineup_6[index]
			}
			index <- index +1
			
		}
		
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_7[index])){
				elem$lineup_7[index+1] <- elem$lineup_7[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_7[index]){
				elem$lineup_7[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_7[index+1] <- elem$lineup_7[index]
			}
			index <- index +1
			
		}
		
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_8[index])){
				elem$lineup_8[index+1] <- elem$lineup_8[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_8[index]){
				elem$lineup_8[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_8[index+1] <- elem$lineup_8[index]
			}
			index <- index +1
			
		}
		
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_9[index])){
				elem$lineup_9[index+1] <- elem$lineup_9[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_9[index]){
				elem$lineup_9[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_9[index+1] <- elem$lineup_9[index]
			}
			index <- index +1
			
		}
		
		
		index <- 3
		end_index <- nrow(elem)	-1	
		while(index <= end_index){
			if(is.na(elem$V6[index]) || is.na(elem$V7[index]) || is.na(elem$lineup_10[index])){
				elem$lineup_10[index+1] <- elem$lineup_10[index]
			}
			else if(elem$V5[index]=="OUT" && elem$V6[index] == elem$lineup_10[index]){
				elem$lineup_10[index+1] <- as.character(elem$V6[index-1])
			}
			else{
				elem$lineup_10[index+1] <- elem$lineup_10[index]
			}
			index <- index +1
			
		}
		datum_name <- as.character(elem$datum[23])
		datei_name <- unique(as.vector(elem$V7))
		datei_name <- paste(datei_name[1],"_", datei_name[2], datei_name[3],"_",".csv")
		datei_name <- gsub(" ","", datei_name)
		datei_name <- gsub("NA","", datei_name)


		print(datei_name)
		write.table(elem, datei_name)	
		return(elem)
	}
	