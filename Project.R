setwd("C:/Users/adity/Desktop/R Codes")

#*** R Project ***

library(ggplot2)
library(dplyr)

# To read the given csv file
df<-read.csv("listings.csv",header=TRUE,sep=",",quote="\"", dec =".",fill=TRUE,)

# There are 39453 observation with 75 variables.
dim(df)
print("There are 39453 observation with 75 variables.")

# Statistical summary of all data frame variables.
summary(df)

# To find the Null values
print("From the summary we got above we can say that all values for bathrooms, caldendar_updated and license variables are na/null")
print("host_listing_count and host_total_listing variables have 5 missing values")
print("bedrooms variable 16893 and for beds variable 605 missing values are there")
print("For the review scores folloiwng are the missing values in data :
review_scores_rating = 10241
review_scores_accuracy = 10654
review_scores_cleaniness = 10644
review_scores_checkin = 10658
review_scores_communication = 10650
review_scores_location = 10661
review_scores_value = 10660
reviews_per_month = 10241")

#price conversion price
str(df$price)
print("Price variable has been infered as character and not numeric hence need to be converted to numeric")
df$price <- as.numeric(gsub('[$,]','',df$price))

# Histogram
ggplot(df, aes(x=price)) + geom_histogram()
print("Checking distribution of price using histogram shows that the distribution is positevely skewed")

# Relationship between price and minimum nights
plot(x = df$minimum_nights, y = df$price, xlab = "Minimum nights",
     ylab = "Price", main = "Minimum nights VS Price")
print("Scatter plot showing relationship between price and minimum nights display many data points are near 0 price range. Implies possible data entry error.")

# Box plot
boxplot(df$price, col = "blue")
print("Price data has outliers as shown in box plot. There are listings with price as high as $30000")

# Visualization 
#Pie chart 
verifiedf <- df %>% group_by(host_identity_verified) %>% summarize(n = n())
verifiedf <- filter(verifiedf,host_identity_verified != "")
pct <- round(verifiedf$n/sum(verifiedf$n)*100)
lbls <- paste(verifiedf$host_identity_verified, pct)
lbls <- paste(lbls,"%",sep="")
pie(verifiedf$n, labels = lbls)
print("88% of the host listings are verified but there are 12% of unverified listings")

# bar plot
neighdf <- df %>% group_by(neighbourhood_group_cleansed) %>% summarize(avg_price = mean(price))
neighdf
colors = c("green", "orange", "brown","blue","purple")

barplot(neighdf$avg_price, main = "Price", names.arg = neighdf$neighbourhood_group_cleansed, 
        xlab = "Neighbourhood", ylab = "Price", 
        col = colors, beside = TRUE) 
print("Average price listings in Manhattan is around $300 which is heighest indicating it is a costly area")
print("Average price listings in Bronx, Queens, Staten Island are of similar price")

# Multiple box plot
boxplot(df$price ~ df$room_type, data = df,
        main = "Price by Room Type",
        xlab = "Room Type",
        ylab = "Price")
print("From the above multipe box plot we can see that there are many outliers in the entire home/apt and Shared room room types and there are less outliers in Hotel room and Shared room types")
