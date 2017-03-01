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
m = m[m$language!="",]
m = m[(m$actor_1_name!="" | m$actor_2_name!="" | m$actor_3_name!=""),]
m = m[!is.na(m$title_year),]
m$gross[is.na(m$gross)] = median(m$gross, na.rm = T)
m$budget[is.na(m$budget)] = median(m$budget, na.rm = T)
m$actor_2_facebook_likes[is.na(m$actor_2_facebook_likes)] = 0
m$actor_3_facebook_likes[is.na(m$actor_3_facebook_likes)] = 0
m$num_user_for_reviews[is.na(m$num_user_for_reviews)] = median(m$num_user_for_reviews, na.rm = T)
m$num_critic_for_reviews[is.na(m$num_critic_for_reviews)] = median(m$num_critic_for_reviews, na.rm = T)
m = m[m$budget<250000001 & m$budget>=10000,]
m = m[m$imdb_score>2.5,]

#Feature engineering on genres
genres = unlist(strsplit(m$genres,"|",fixed = T))
ugenres  = data.frame(g = unique(genres), imp = 0,stringsAsFactors =  F)
for (i in 1:nrow(ugenres)) {
 #Compute tf-idf score/importance of each genre. 
 #For example, action is pretty generic and does not provide much information about a movie's quality
 #But 'Space' genre is much more specific
  ugenres$imp[i] = 1/length(grep(ugenres$g[i], genres))
}
newgenre = function(genres, ugenres) {
  #Return the highest genre score. A movie can belong to multiple genres.
  g2 = as.character(0)
  for (i in 1:length(genres)) {
    g = unlist(strsplit(genres[i], split="|",fixed = T))
    g1 = ugenres[ugenres$g %in% g,]
    g2[i] = g1[which(g1$imp == max(g1$imp)),1]
  }
  return(g2)
}
dirnummovies = m%>%group_by(director_name)%>%summarise(dirnummovies=n())
dirnummovies = data.frame(dirnummovies)
m = merge(m, dirnummovies, by.x = "director_name", by.y = "director_name", all.x = T)
m$title_decade = (round(m$title_year/10))
m$newg = newgenre(m$genres, ugenres)
m$roi = m$gross/m$budget
m$language[m$language!="English"] = "Non-English"   #Only keep two values for language : English and Non-English

#Test each variable
h = cor(m$title_year, m$imdb_score)
dirsummary = m%>%group_by(actor_1_name)%>%summarise(mnb = mean(budget), mns = round(mean(budget)/10000000), n=n(), r = n*mns)%>%arrange(desc(r))
d = dirsummary[,c(1,5)]
#head(dirsummary, 50)
View(dirsummary)
m = merge(m, d, by.x = "actor_1_name", by.y = "actor_1_name", all.x = T)

#Remove columns not required
notrequired = c("gross","aspect_ratio","country", "movie_title","director_name", "actor_1_name","actor_2_name","actor_3_name","genres", "plot_keywords","movie_imdb_link")
mnew = m[,!names(m) %in%  notrequired]
for(i in 1:ncol(mnew)) {
  if(is.character(mnew[,i])) {
    mnew[,i] = as.factor(mnew[,i])
  }
}
str(mnew)

#Split into test and train
mtr = mnew[1:4300, ]
mts = mnew[4301:nrow(mnew),]

#Get independent and dependent variables
f = imdb_score ~ .

#Apply linear regression
model = lm(f,data = mtr)
p = predict(model, newdata = mts)
act = mts$imdb_score
mts$p = round(p,1)
#head(mts[,c("imdb_score","p")],30)
#View(mts[,c("imdb_score","p")])

#Evaluate Model
errorplot = qplot(act, p, geom = c("point"), xlab = "Actual Score", ylab = "Predicted Score", main = "Predicted vs Actuals")
errorplot + stat_smooth(method="lm", se=FALSE)
#qplot(act, p-act, geom = c("point"))
#summary(model)
rmse = sqrt(sum((p-act)^2)/nrow(mts))
cat("Root mean square error: ", rmse)
