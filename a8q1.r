Sys.setenv(https_proxy="http://10.3.100.207:8080")
Sys.setenv(http_proxy="http://10.3.100.207:8080")
library(ggplot2)
library(dplyr)
library(lsa)

#Read data
m <- read.csv("~/Downloads/sem1/clab1/a8/clean-file", stringsAsFactors=FALSE)
s = m%>%group_by(color)%>%summarise(n = n(), mn = mean(imdb_score, na.rm = T))

#Run the below code to get rid of random shit in movie_title
#tr -d '\302\240' < movie_metadata.csv > clean-file

#Deal with NA values
duration = m%>%group_by(as.factor(genres))%>%summarise(n= n(), mn = mean(duration,na.rm = T))
m$facenumber_in_poster[is.na(m$facenumber_in_poster)] = 1
m$duration[is.na(m$duration)] = 103.0
m = m[m$movie_title!="" ,]
m = m[m$director_name!="",]
m = m[(m$actor_1_name!="" | m$actor_2_name!="" | m$actor_3_name!=""),]

#Separate numerical and character data
#movie_title is column number 12
numv = c(3,4,5,6,8,9,13,14,16,19,23,25,26,28)
charv = seq(1,28)
charv = charv[!(charv %in% numv)]
mnum = m[,c(numv, 12)] #add movie_title column
mchar = m[,charv]
summary(mnum)

#Calculate cosine similarity. Use only numerical columns. 
usermovie = "Se7en"
v1 = as.numeric(mnum[mnum$movie_title==usermovie,seq(1,12)])
v2 = as.numeric(mnum[mnum$movie_title == "Spectre",seq(1,12)])
cosimilarity = data.frame(movie_title = mnum$movie_title, cosine = 0) #Generate dataframe with two columns: movie_title and cosine

for (i in 1:nrow(mnum)){
  v2 = as.numeric(mnum[i,seq(1,12)])
  cosimilarity$cosine[i] = cosine(v1,v2) #Populate the cosine column of the dataframe generated earlier
}

cosimilarity = cosimilarity[order(-cosimilarity$cosine),]
head(cosimilarity, 10)
