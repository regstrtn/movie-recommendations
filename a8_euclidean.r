Sys.setenv(https_proxy="http://10.3.100.207:8080")
Sys.setenv(http_proxy="http://10.3.100.207:8080")
library(ggplot2)
library(dplyr)
library(lsa)

#Read data
m <- read.csv("~/Downloads/sem1/clab1/a8/clean-file", stringsAsFactors=FALSE)
s = m%>%group_by(color)%>%summarise(n = n(), mn = mean(imdb_score, na.rm = T))
m = unique(m)

#Run the below code to get rid of random shit in movie_title
#tr -d '\302\240' < movie_metadata.csv > clean-file

#Deal with NA values
duration = m%>%group_by(as.factor(genres))%>%summarise(n= n(), mn = mean(duration,na.rm = T))
m$facenumber_in_poster[is.na(m$facenumber_in_poster)] = 1
m$duration[is.na(m$duration)] = 103.0
m = m[m$movie_title!="" ,]
m = m[m$director_name!="",]
m = m[(m$actor_1_name!="" | m$actor_2_name!="" | m$actor_3_name!=""),]
m$gross[is.na(m$gross)] = median(m$gross, na.rm = T)
m$budget[is.na(m$budget)] = median(m$budget, na.rm = T)
m$actor_2_facebook_likes[is.na(m$actor_2_facebook_likes)] = 0
m$actor_3_facebook_likes[is.na(m$actor_3_facebook_likes)] = 0
m$num_user_for_reviews[is.na(m$num_user_for_reviews)] = median(m$num_user_for_reviews, na.rm = T)
m$num_critic_for_reviews[is.na(m$num_critic_for_reviews)] = median(m$num_critic_for_reviews, na.rm = T)

#Separate numerical and character data
#movie_title is column number 12
numv = c(3,4,5,6,8,9,13,14,16,19,23,25,26,28)
mnum = m[,numv]
charv = seq(1,28)
charv = charv[!(charv %in% numv)]
mchar = m[,charv]
summary(mnum)

#Compute JC coefficient of genre and plot keywords
genre = gsub("|", " ", mchar$genres, fixed = T)
plot = gsub("|", " ", mchar$plot_keywords, fixed = T)

jc = function(v1, v2) {
  v1 = gsub("|", " ", v1, fixed = T)
  v2 = gsub("|", " ", v2, fixed = T)
  g1 = strsplit(v1, " ", fixed = T)
  g2 = strsplit(v2, " ", fixed = T)[[1]]
  u = sapply(g1, function(x) length(union(x, g2)))
  i = sapply(g1, function(x) length(intersect(x, g2)))
  return (i/u)
}
#print(jc(mchar$genre, mchar$genre[3]))
ad = paste(mchar$director_name, mchar$actor_1_name, mchar$actor_2_name, mchar$actor_3_name, sep = "|")

usermovie = "Zombieland"  #Get moviename from user

#Assign a numerical value to genre and plot keywords
usermovieinfo = mchar[mchar$movie_title == usermovie,]
g1 = mchar$genres[mchar$movie_title == usermovie]
p1 = mchar$plot_keywords[mchar$movie_title == usermovie]
ad1 = paste(usermovieinfo$director_name, usermovieinfo$actor_1_name, usermovieinfo$actor_2_name, usermovieinfo$actor_3_name, sep = "|")
mnum$genrejc = jc(mchar$genres, g1)
mnum$plotjc = jc(mchar$plot_keywords, p1)
mnum$act_directjc = jc(ad, ad1)
mnum$movie_title = NULL #remove non-numeric columns

#Normalise all numeric values
for (i in 1:ncol(mnum)) {
  mn = mean(mnum[,i],na.rm = T)
  sd = sd(mnum[,i], na.rm = T)
  mnum[,i] = (mnum[,i] - mn)/sd
}

#Assign more weight to important columns
impcol = c(6,13,14,16,17)
#mnum[,impcol] = 2*mnum[,impcol]

mnum$movie_title = mchar$movie_title #Add movie_title column back

#Calculate euclidean similarity. Use only numerical columns. 
cosimilarity = data.frame(movie_title = mchar$movie_title, genres = mchar$genres, euclidean = 0, cosine = 0) #Generate dataframe with two columns: movie_title and cosine
euclidean = rep(1000, times = nrow(mnum))
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2)) #define euclidean distance function

v1 = as.numeric(mnum[mnum$movie_title==usermovie,seq(1,17)])
for (i in 1:nrow(mnum)){
  v2 = as.numeric(mnum[i,seq(1,17)])
  v2[impcol] = 0.7*v2[impcol]
  euclidean[i] = euc.dist(v1,v2) #Populate the cosine column of the dataframe generated earlier
}



#Calculate cosine similarity
cos.sim = function(x1, x2) sum(x1*x2)/(sqrt(sum(x1^2))*sqrt(sum(x2^2)))
cosine = rep(0, times = nrow(mnum))
for (i in 1:nrow(mnum)){
  v2 = as.numeric(mnum[i,seq(1,17)])
  cosine[i] = cos.sim(v1,v2) #Populate the cosine column of the dataframe generated earlier
}

#Display top movies
cosimilarity$euclidean = euclidean
cosimilarity$cosine = cosine

cosimilarity = cosimilarity[order(cosimilarity$euclidean),] #Order by euclidean
head(cosimilarity[-1,], 12)

cosimilarity = cosimilarity[order(-cosimilarity$cosine),] #Order by cosine
head(cosimilarity[-1,], 12)
