Sys.setenv(https_proxy="http://10.3.100.207:8080")
Sys.setenv(http_proxy="http://10.3.100.207:8080")
library(ggplot2)
library(dplyr)
library(lsa)

#Read data
m <- read.csv("~/Downloads/sem1/clab1/a8/clean-file", stringsAsFactors=FALSE)
s = m%>%group_by(color)%>%summarise(n = n(), mn = mean(imdb_score, na.rm = T))

#Run the below code in linux shell to get rid of random shit in movie_title
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
  #v1 = gsub("|", " ", v1, fixed = T)
  #v2 = gsub("|", " ", v2, fixed = T)
  #g1 = strsplit(v1, " ", fixed = T)
  #g2 = strsplit(v2, " ", fixed = T)[[1]]
  #u = sapply(g1, function(x) length(union(x, g2)))
  #i = sapply(g1, function(x) length(intersect(x, g2)))
  u = length(union(v1, v2))
  i = length(intersect(v1, v2))
  return (i/u)
}
print(jc(mchar$genre, mchar$genre[3]))

#Get actor, director, movie dataframe
mjc1 = m[,c("movie_title","director_name", "actor_1_name")]
mjc2 = m[,c("movie_title", "director_name", "actor_2_name")]
mjc3 = m[,c("movie_title", "director_name","actor_3_name")]
names(mjc1) = names(mjc2) = names(mjc3) = c("movie_title","director_name", "actor_name")
mjc = rbind(mjc1, mjc2,mjc3)
mjc = unique(mjc)
a_d_pair = unique(data.frame(director_name = mjc$director_name, actor_name=mjc$actor_name, stringsAsFactors = FALSE))
jcvector = rep(0, times = nrow(a_d_pair))

#Calculate JC similarity for each actor director pair
for (i in 1:nrow(a_d_pair)) {
  actor_movies = unique(mjc$movie_title[mjc$actor_name == a_d_pair$actor_name[i]])
  director_movies = unique(mjc$movie_title[mjc$director_name == a_d_pair$director_name[i]])
  la = length(actor_movies)
  ld = length(director_movies)
  a_d_pair$numdirectormovies[i] = ld
  a_d_pair$numactormovies[i] = la
  if(la > 1 | ld > 1) {
    jcvector[i] = jc(actor_movies, director_movies)*min(c(la,ld))
  }
}

a_d_pair$jc = jcvector
a_d_pair = a_d_pair[order(-a_d_pair$jc),]
head(a_d_pair[a_d_pair$actor_name!=a_d_pair$director_name,],10)
