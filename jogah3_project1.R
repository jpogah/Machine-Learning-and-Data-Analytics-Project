library("ggplot2")
library("plyr")
library(tm)
library(magrittr)
library(wordcloud)
library(chron)
library(scales)
library(GGally)
library(reshape2)
library(dplyr)
library(stringr)
library(timeDate)
library(lubridate)
library("cowplot")
library("gridExtra")
library("cowplot")

load("movies_merged")

row = dim(movies_merged)[1]

# Listing 1.0
# create a subset of the data set with movie type only
row = dim(movies_merged)[1]
movie_only_data = subset(movies_merged, Type == "movie")
new_row = dim(movie_only_data)[1]
row_removed = row - new_row
cat("number of rows removed: ", row_removed)


# Listing 2.0:
# remove rows with Runtime value equals N/A
new_data = subset(movie_only_data, Runtime != "N/A")

# create a function that takes a string, split it based
# on whitespace character, take the first string and then 
#return the value as numeric
convert.to.numeric = function(x){
  str = strsplit(x , " ")[[1]]
  len = length(str)
  num = 0;
  if (len == 4){
    num = as.numeric(str[1]) * 60 + as.numeric(str[3])
  }
  if (len == 2){
    if ( str[2] == "h"){
      num = as.numeric(str[1]) * 60
    }
    if( str[2] == "min"){
      num = as.numeric(str[1])
    }
  }
  return(num)
}


ans = data.frame(ldply(new_data[,'Runtime'], convert.to.numeric))
new_data$Runtime = ans$V1

index1 = sample(nrow(new_data), 1000)
sample.number = 1:1000
new_data[index1,] %>%
  ggplot(aes(sample.number,Runtime)) + geom_point() + ggtitle("Fig 2.0: Runtime of movies")

random_index = sample(nrow(new_data), 1000)
new_data[random_index,]  %>%
  ggplot(aes(Year,Runtime)) + geom_point() + ggtitle("Fig 2.1: Runtime vs Year")


new_data2 = subset(new_data, is.na(Budget) == FALSE)
random_index2 = sample(nrow(new_data2),2000)

new_data2[random_index2,]   %>%
  ggplot(aes(log(Budget),Runtime)) + geom_point() + ggtitle("Fig 2.2: Runtime vs Budget")


# Listing 3.0
new_data3 = subset(movie_only_data, Genre != "N/A")
docs = data.frame(new_data3$Genre)
ds = DataframeSource(docs)
rm(docs)
docs = VCorpus(ds)

# view the first three document in the corpus
lapply(docs[1:3], as.character)

#convert all words to lower case
docs = tm_map(docs, content_transformer(tolower))

# remove punctuation

docs = tm_map(docs, removePunctuation)

# remove number if any
docs = tm_map(docs, removeNumbers)

# strip white space
docs = tm_map(docs, stripWhitespace)

# view the first three document in the corpus
lapply(docs[1:3], as.character)


# create a term matrix 
#A document term matrix is simply a matrix with documents as the rows and terms as the columns and a count of the frequency of words as the cells of the matrix

dtm = DocumentTermMatrix(docs)

newcolumns = as.matrix(dtm)
new_data4 = cbind(new_data3,newcolumns)


# By ordering frequency we can least the most frequent and least frequent term

countfreq = colSums(as.matrix(dtm))

orderfreq = order(countfreq)

#most frequent item
top.10.genre = countfreq[tail(orderfreq,10)]
print(top.10.genre)
list_genre = names(top.10.genre)

temp_data = subset(new_data4, select=c("Title",list_genre))
temp_data = melt(temp_data, id=1)
temp_data = subset(temp_data , value != 0)
names(temp_data ) = c("Title" ,"Genre" ,"value" )

temp_data %>%
  filter(Genre %in% names(top.10.genre)) %>%
  group_by(Genre) %>%
  ggplot(aes(x=Genre)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=percent) + ggtitle("Fig 3.0: distribution of movie title with the top 10 movie genre") + theme(axis.text.x=element_text(angle=45,hjust = 1))



temp_data = subset(new_data4, select=c("Title","Gross",list_genre))
temp_data = subset(temp_data, is.na(Gross) == FALSE)
temp_data = melt(temp_data, id=c(1,2))
temp_data = subset(temp_data , value != 0)
names(temp_data ) = c("Title","Gross" ,"Genre" ,"value" )

ggplot(temp_data ,aes(reorder(Genre, -Gross, median), Gross)) + geom_boxplot() + coord_flip() + scale_x_discrete("Genre") + ggtitle("Fig 3.1 : Genre vs Gross")

# Listing 5.0
# new_data4 is the name of the final data set generated in question 3

#remove rows with missing values in the Released column
data_no_na_for_Released = subset(new_data4 , is.na(Released) == FALSE)


# remove NA occuring in Gross variable
data_no_na_for_Released = subset(data_no_na_for_Released, is.na(Gross) == FALSE)

data_no_na_for_Released$ReleasedYear = year(data_no_na_for_Released$Released)

hlist =holidayNYSE(1888:2018)
hlist = c(hlist,holidayLONDON(1888:2018))
hlist = c(hlist, holidayTSX(1888:2018))
hlist = c(hlist, holidayZURICH(1888:2018))
#myholidays  <- dates(as.character(holiday(1890:2018,hlist)),format="Y-M-D")
temp1 = data.frame(isHoliday(timeDate(data_no_na_for_Released$Released), holidays =hlist ))
names(temp1) = "v1"
data_no_na_for_Released$Holidays[temp1$v1 == TRUE] = "Yes"
data_no_na_for_Released$Holidays[temp1$v1 == FALSE] = "No"
temp2 = data.frame(is.weekend(data_no_na_for_Released$Released))
names(temp2) = "v1"
data_no_na_for_Released$Weekend[temp2$v1 == TRUE] = "Yes"
data_no_na_for_Released$Weekend[temp2$v1 == FALSE] = "No"
data_no_na_for_Released$ReleasedWeek = week(data_no_na_for_Released$Released)


data_no_na_for_Released %>%
  mutate(Gross_Billion=Gross/1000000000) %>%
  filter (ReleasedYear %in% 1992:2018) %>%
  ggplot(aes(x=ReleasedWeek, y=Gross_Billion,fill=Holidays))  +        geom_bar(stat="identity") + ggtitle("Fig 5.1 : Gross Revenue vs ReleasedWeek" )




genrelist = names(top.10.genre)

# Extract the Title, Gross and 25 genre columns from data obtained in question 3

temp_data = data_no_na_for_Released[,c("Title","Gross","Holidays","ReleasedWeek",genrelist)]
temp_data_tall = melt(temp_data, id=c(1,2,3,4))

# drop all rows where value == 0 
temp_data_tall = subset(temp_data_tall, value != 0)
names(temp_data_tall) = c("Title","Gross", "Holidays","ReleasedWeek", "Genre", "value")
temp_data_tall$Genre = as.factor(temp_data_tall$Genre)


temp_data_tall %>%
  mutate(Gross_Billion=Gross/1000000000) %>%
  filter(Genre %in% genrelist) %>%
  group_by(Genre) %>%
  ggplot(aes(x=ReleasedWeek, y=Gross_Billion, fill=Holidays)) + geom_bar(stat="identity") + facet_wrap(~Genre) + ggtitle("Fig 5.2  Released Gross  revenue vs ReleasedWeek") +  theme(axis.text.x=element_text(angle=45,hjust = 1))

# Listing 6.0

onlyRatingData = movies_merged[, c("imdbVotes","imdbRating","tomatoRating","tomatoReviews","tomatoUserRating","tomatoUserReviews","tomatoMeter","tomatoRotten","tomatoUserMeter","tomatoFresh")]
onlyRatingDataNoNA = na.omit(onlyRatingData)

index = sample(nrow(onlyRatingDataNoNA),500)
onlyRatingDataNoNA[index,] %>%
  ggpairs( columns=c("imdbVotes","imdbRating","tomatoRating","tomatoReviews","tomatoUserRating","tomatoUserReviews","tomatoMeter","tomatoRotten","tomatoUserMeter","tomatoFresh") ,columnLabels= c("A","B","T1","T2","T3","T4","T5","T6","T7","T8") ) + theme(axis.line=element_blank(),
                                                                                                                                                                                                                                                              axis.text=element_blank(),
                                                                                                                                                                                                                                                              axis.ticks=element_blank()) 
onlyRatingData =movie_only_data[, c("Gross","imdbVotes","imdbRating","tomatoRating","tomatoReviews","tomatoUserRating","tomatoUserReviews","tomatoMeter","tomatoRotten","tomatoUserMeter","tomatoFresh")]
onlyRatingDataNoNA = na.omit(onlyRatingData)


index = sample(nrow(onlyRatingDataNoNA),500)
onlyRatingDataNoNA[index,] %>%
  mutate(Gross_Billion = Gross/1000000000) %>%
  ggpairs( columns=c("Gross_Billion","imdbVotes","imdbRating","tomatoRating","tomatoReviews","tomatoUserRating","tomatoUserReviews","tomatoMeter","tomatoRotten","tomatoUserMeter","tomatoFresh"),columnLabels=c("Gross","A","B","T1","T2","T3","T4","T5","T6","T7","T8" ))  + theme(axis.line=element_blank(), axis.text=element_blank(),
                                                                                                                                                                                                                                                                                     axis.ticks=element_blank()) 
# Listing 7.0
extract.numbers = function(x,awards){
  num = str_extract_all(x,"\\(?[0-9]+\\)?")[[1]]
  num= as.numeric(num)
  num = (sum(num))
  if (awards == "no nomination/awards"){
    return(as.numeric(num == 0))
  }
  
  if (awards == "some nomination/awards"){
    return(as.numeric(num <= 11))
  }
  
  if (awards == "many nomination/awards"){
    return(as.numeric(num > 11))
  }
  
}

temp_data1 = movie_only_data
temp_data1$Awards[temp_data1$Awards == "N/A"] = "0"

ans1 = data.frame(ldply(temp_data1[,"Awards"], extract.numbers, "no nomination/awards"))
temp_data1$NoNominationORAwards = ans1$V1
ans2 = ldply(temp_data1$Awards, extract.numbers, "some nomination/awards")
temp_data1$SomeNominationORAwards = ans2$V1
ans3 = ldply(temp_data1$Awards, extract.numbers, "many nomination/awards")
temp_data1$ManyNominationORAwards = ans3$V1

test_data = temp_data1[,c("Title","Gross","NoNominationORAwards","SomeNominationORAwards","ManyNominationORAwards")]
test_data = subset(test_data, is.na(Gross) == FALSE)
test_data = melt(test_data, id=c(1,2))
test_data = subset(test_data , value != 0)
names(test_data) = c("Title","Gross","Awards", "value")

#plot the boxsplot 
ggplot(test_data ,aes(reorder(Awards, -Gross, median), Gross)) + geom_boxplot() + coord_flip() + scale_x_discrete("Awards") +ggtitle("Fig 7.1: Awards vs Gross")

# Listing 8.0
#check the distribution of gross revnue by Country
temp_data = subset(new_data4, Country != "N/A")
mydocs = data.frame(temp_data$Country)
ds = DataframeSource(mydocs)
rm(mydocs)
mydocs = VCorpus(ds)
#convert all words to lower case
mydocs = tm_map(mydocs, content_transformer(tolower))

toString = content_transformer(function(x, from ,to) gsub(from , to , x))
mydocs = tm_map(mydocs , toString, "new zealand", "new-zealand")
mydocs = tm_map(mydocs , toString, "burkina faso", "burkina-faso")
mydocs = tm_map(mydocs , toString, "united arab emirates", "united-arab-emirates")
mydocs = tm_map(mydocs , toString, "puerto rico", "puerto-rico")
mydocs = tm_map(mydocs , toString, "costa rica", "costa-rica")
mydocs = tm_map(mydocs , toString, "côte d'ivoire", "côte-d'ivoire")
mydocs = tm_map(mydocs , toString, "dominican republic", "dominican-republic")
mydocs = tm_map(mydocs , toString, "solomon islands", "solomon-islands")
mydocs = tm_map(mydocs , toString, "bosnia and herzegovina", "bosnia-and-herzegovina")
mydocs = tm_map(mydocs , toString, "soviet union", "soviet-union")
mydocs = tm_map(mydocs , toString, "east germany", "east-germany")
mydocs = tm_map(mydocs , toString, "federal republic of yugoslavia", "yugoslavia")
mydocs = tm_map(mydocs , toString, "hong kong", "hong-kong")
mydocs = tm_map(mydocs , toString, "west germany", "west-germany")
mydocs = tm_map(mydocs , toString, "the democratic republic of congo", "congo")
mydocs = tm_map(mydocs , toString, "isle of man", "isle-of-man")
mydocs = tm_map(mydocs , toString, "uk,", " united-Kingdom")
mydocs = tm_map(mydocs , toString, "trinidad and tobago", "trinidad-and-tobago")
mydocs = tm_map(mydocs , toString, "turks and caicos islands", "turks-and-caicos-islands")
mydocs = tm_map(mydocs , toString, "serbia and montenegro", "serbia-and-montenegro")
mydocs = tm_map(mydocs , toString, "south africa", "south-africa")
mydocs = tm_map(mydocs , toString, "saudi arabia", "saudi-arabia")
mydocs = tm_map(mydocs , toString, "north korea", "north-korea")
mydocs = tm_map(mydocs , toString, "papua new guinea", "papua-new-guinea")
mydocs = tm_map(mydocs , toString, "czech republic", "czech-republic")
mydocs = tm_map(mydocs , toString, "republic of macedonia", "republic-of-macedonia")



sapply(mydocs[1:3], as.character)


# remove number if any
mydocs = tm_map(mydocs, removeNumbers)

# strip white space
mydocs = tm_map(mydocs, stripWhitespace)

mydocs = tm_map(mydocs, removePunctuation)

lapply(mydocs[1:3], as.character)


# create a term matrix 
#A document term matrix is simply a matrix with documents as the rows and terms as the columns and a count of the frequency of words as the cells of the matrix

dtm1 <- DocumentTermMatrix(mydocs)
newcolumns = as.matrix(dtm1)
temp_data = cbind(temp_data,newcolumns)
country_ds = data.frame(newcolumns)

sample_data = subset(temp_data, select = c("Title","Gross", names(country_ds)))
sample_data = melt(sample_data, id=c(1,2))

sample_data = subset(sample_data, is.na(Gross) == FALSE)
sample_data = subset(sample_data, value != 0)
names(sample_data) = c("Title", "Gross", "Country", "value")
freq2 = colSums(as.matrix(dtm1))
length(freq2)
ord = order(freq2)
# least frequenty term/country
freq2[head(ord)]

#most frequent item
top.10.countries = freq2[tail(ord,10)]



sample_data %>%
  filter(Country %in% names(top.10.countries)) %>%
  ggplot(aes(x=Country)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=percent) + ggtitle("Fig 8.0: distribution of movie title accross top 10 producing country") + theme(axis.text.x=element_text(angle=45,hjust = 1))

# select top 10 countries with highest median revenue
ds = aggregate(sample_data[,2] , list(sample_data$Country), FUN=median)
ds = ds[order(-ds$x),]
Countries.with.high.median.gross =  as.character(ds$Group.1[1:10])

sample_data %>%
  filter(Country %in% Countries.with.high.median.gross) %>%
  ggplot(aes(reorder(Country, -Gross, median), Gross)) + geom_boxplot() + coord_flip() + scale_x_discrete("Country") + ggtitle("Fig 8.1: Country vs Gross") + theme(axis.text.y=element_text(angle=45,hjust = 1))


new_ds = subset(temp_data, is.na(Gross) == FALSE)
new_ds = subset(temp_data,select=c("Title","Gross", names(countfreq)))
new_ds = melt(new_ds, id=c(1,2))
new_ds = subset(new_ds , value != 0)
names(new_ds) = c("Title","Gross","Genre", "value")
data.genre.gross = merge(sample_data,new_ds, by=c("Title","Gross"), all = TRUE)
data.genre.gross = na.omit(data.genre.gross)

index = sample(nrow(data.genre.gross),1000)

data.genre.gross[index,] %>%
  mutate(Gross_Billion = Gross/1000000000) %>%
  filter(Country %in% names(top.10.countries)) %>%
  group_by(Country) %>%
  ggplot(aes(Country )) + geom_bar() + facet_wrap(~Genre) + theme(axis.text.x=element_text(angle=45,hjust = 1)) + ggtitle("Fig 8.2: Distribution of different Genre by Country")