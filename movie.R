library(dplyr)
library(ggplot2)
library(tidyr)

## Section 1: Load data

movies = read.csv("movies_metadata.csv")
str(movies)
head(movies)
colnames(movies)


## Section 2: Data Cleaning

movies_cleaned = movies
temp = strsplit(gsub('[^[:alnum:] ]',"",movies_cleaned[,"genres"])," ")
for(i in 1:length(temp)){
  temp[[i]] = temp[[i]][temp[[i]]>="Action" & !(temp[[i]] %in% c("id","name","Fiction"))]
  temp[[i]] = replace(temp[[i]], temp[[i]] == "Science", "Science Fiction")
}
movies_cleaned = movies_cleaned %>% mutate(genre = temp)
movies_cleaned$released_year = as.numeric(substr(movies_cleaned$release_date,1,4))
movies_cleaned$released_month = as.numeric(substr(movies$release_date,6,7))
movies_cleaned$budget = as.numeric(movies_cleaned$budget)
movies_cleaned = movies_cleaned %>% filter(budget >= 10000 & revenue >= 10000 & vote_count >= 3 & runtime >= 75) %>% mutate(roi = (revenue-budget)/budget, rating = vote_average) %>%
  select(title, original_title, released_year, released_month, budget, revenue, roi, runtime, rating, vote_count, genre)

## Section 3: Data Visualization

# Section 3.1: Revenue 
hist(movies_cleaned$revenue, col = "lightblue",breaks = 200, xlab = "Revenue (USD)", main = "Histogram of Revenue")
boxplot(movies_cleaned$revenue, main = "Boxplot of Revenue")
# Right skewed, perform log transformation
par(mfrow = c(1,2))
hist(log(movies_cleaned$revenue),breaks = 50, col = "cyan", xlab = "log(Revenue)", main = "Histogram of log(Revenue)")
boxplot(log(movies_cleaned$revenue), main = "Boxplot of log(Revenue)")
# We can see that there are a lot of movies with low revenue

# Section 3.2: Rating 
hist(movies_cleaned$rating, col = "lightblue", breaks = 50, main="Histogram of Movie Rating",xlab="Rating")

# Section 3.3: Year plot
hist(movies_cleaned$released_year,breaks = 50, col = "lightblue" , xlab = "Released Year", main = "Histogram of Released Years")
# The data is missing some of the most recent movies
barplot(table(movies_cleaned$released_month), col = "lightblue", xlab = "Released Month", main = "Histogram of Released Months")
# There seems to be more movies in September and December

# Section 3.4: Budget
par(mfrow = c(1,3))
hist(movies_cleaned$budget, breaks = 50, col = "lightblue", main = 'Histogram of Budget', xlab = 'Budget (USD)')
# The plot is right skewed, we can perform log transformation
hist(log(movies_cleaned$budget), breaks = 50, col = "cyan", main = 'Histogram of log(Budget)', xlab = 'log(Budget)')
boxplot(log(movies_cleaned$budget), main = "Boxplot of log(Budget)")
par(mfrow = c(1,1))
summary(movies_cleaned$budget)

# Section 3.5: Runtime
hist(movies_cleaned$runtime, breaks = 50, col = "lightblue", xlab = "Runtime (minutes)", main ="Histogram of Movie Runtimes")

# Section 3.6: Genre
genres_table = table(unlist(movies_cleaned$genre))
genres_table = data.frame(genres_table[genres_table > 5])
colnames(genres_table) = c("genre", "amount")
genres_table %>% ggplot(aes(x = genre, y = amount)) + geom_bar(stat = "identity",fill = "lightblue") + coord_flip()

# Section 3.7: ROI
par(mfrow = c(1,3))
# To properly see how the histogram will be like, we need to exclude the outliers that have ROI that are too high
hist(movies_cleaned$roi[movies_cleaned$roi < 100],  breaks = 50, col = "lightblue", xlab = "ROI (Return on Investment)", main = "Histogram of ROI")
# The plot is right skewed, we can perform log transformation
hist(log(movies_cleaned$roi+1), breaks = 50, col = "cyan", xlab = "log(ROI+1)", main = "Histogram of log(ROI+1)")
boxplot(log(movies_cleaned$budget), main = "Boxplot of log(ROI+1)")
par(mfrow = c(1,1))
# The reason for having ROI+1 is that we want to look at the ratio of revenue/budget
summary(movies_cleaned$roi)

## Section 4: Statistical Analyses

# Section 4.1: Correlation plot
# Extract the numerical variables and perform log transformation
numerical = movies_cleaned %>% select(budget, revenue, runtime, vote_average, roi)
numerical$budget = log(numerical$budget)
numerical$revenue = log(numerical$revenue)
numerical$roi = log(numerical$roi + 1)

library(psych)
pairs.panels(numerical, method = "pearson", hist.col = "steelblue", pch = 21, density = TRUE, ellipses = FALSE)
# We can see that 
# -> log(budget) and log(revenue) are positively correlated with correlation coefficient 0.64
# -> log(revenue) and log(roi+1) are also positively correlated with correlation coefficient 0.68

# Section 4.2: Statistical test between continuous variables

# Section 4.2.1: log(budget) vs log(revenue) 
top_budget = movies_cleaned %>% select(budget,revenue)
top_budget = top_budget[order(top_budget$budget, decreasing = T),]
top_revenue = top_budget[order(top_budget$revenue, decreasing = T),]
top_budget = top_budget[1:1000,]
top_revenue = top_revenue[1:1000,]

# Scatterplot of top 1000 budget & revenue samples
top_budget %>% ggplot(aes(x=log(budget), y=log(revenue))) + geom_point() + geom_smooth(method = "lm", col = "blue")
top_revenue %>% ggplot(aes(x=log(budget), y=log(revenue))) + geom_point() + geom_smooth(method = "lm", col = "blue")
# Whole sample
movies_cleaned %>% ggplot(aes(x=log(budget), y=log(revenue))) + geom_point() + geom_smooth(method = "lm", col = "red")

lmodel = lm(log(revenue)~log(budget), data = movies_cleaned)
summary(lmodel)
# Adj RSQ = 0.4151, p-value < 2.2e-16 -> significant relationship
par(mfrow = c(2,2))
plot(lmodel)

var.test(log(movies_cleaned$revenue),log(movies_cleaned$budget))
# p-value < 2.2e-16, therefore reject H_0: variances are the same
# log(revenue) and log(budget) have different variances

t.test(log(movies_cleaned$revenue),log(movies_cleaned$budget), var.equal = F)
# p-value < 2.2e-16, therefore reject H_0: means are the same
# log(revenue) and log(budget) have different means
# 95% CI for log(revenue) - log(budget) = log(revenue/budget) is (0.4552, 0.5950)
# 95% CI for revenue/budget is (1.5764, 1.8131)


# Section 4.2.2: log(budget) vs log(roi+1)
top_budget1 = movies_cleaned %>% select(budget,roi)
top_budget1 = top_budget1[order(top_budget1$budget, decreasing = T),]
top_roi = top_budget1[order(top_budget1$roi, decreasing = T),]
top_budget1 = top_budget1[1:1000,]
top_roi = top_roi[1:1000,]

# Top 1000 budget & ratio samples
top_budget1 %>% ggplot(aes(x=log(budget), y=log(roi+1))) + geom_point() + geom_smooth(method = "lm", col = "blue")
top_roi %>% ggplot(aes(x=log(budget), y=log(roi+1))) + geom_point() + geom_smooth(method = "lm", col = "blue") 
# Whole sample 
movies_cleaned %>% ggplot(aes(x=log(budget), y=log(roi+1))) + geom_point() + geom_smooth(method = "lm", col = "red")

lmodel_budget_roi = lm(log(roi+1)~log(budget), data = movies_cleaned)
summary(lmodel_budget_roi)
# Adj RSQ = 0.01552, p-value < 2.2e-16 -> significant relationship
par(mfrow = c(2,2))
plot(lmodel_budget_roi)

var.test(log(movies_cleaned$roi+1),log(movies_cleaned$budget))
# p-value = 0.00442, therefore reject H_0: variances are the same
# log(revenue) and log(roi+1) have different variances

t.test(log(movies_cleaned$roi+1),log(movies_cleaned$budget), var.equal = F)
# p-value < 2.2e-16, therefore reject H_0: means are the same
# log(revenue) and log(roi+1) have different means


# Section 4.2.3: Revenue vs runtime 
lmodel<-lm(log(revenue)~runtime,data=movies_cleaned)
summary(lmodel)
# R^2 = 3.4%, conclude insignificant 

# Test whether a longer movie (> 120 minutes) perform better in box office
movies_long = movies_cleaned%>%filter(runtime>120)
movies_short = movies_cleaned%>%filter(runtime<=120)
boxplot(log(movies_long$revenue),log(movies_short$revenue),names=c("long movies","short movies"),main="log(Revenue)")
# Boxplot shows long movies perform slightly better on average

# Variance test
var.test(log(movies_short$revenue),log(movies_long$revenue))
# Variance are not equal

# Two sample t test, 95% CI, H0: equal revenue
t.test(log(movies_long$revenue),log(movies_short$revenue),alternative="greater",var.equal=FALSE)
# Reject H0


# Section 4.2.4: Rating vs log(budget)
# Based on the budget, we split it into three categories
n = nrow(movies_cleaned)
movies_rating_budget = movies_cleaned %>% select(budget, rating)
movies_rating_budget %>% mutate(cat = numeric(n))
for (i in 1:n) {
  if (movies_rating_budget[i,'budget'] < 5000000) {
    movies_rating_budget[i,'cat'] = 1 # low budget
  } else if (movies_rating_budget[i, 'budget'] >= 50000000) {
    movies_rating_budget[i,'cat'] = 3 # high budget
  } else {
    movies_rating_budget[i,'cat'] = 2 # mid budget
  } 
}

# Perform boxplot on the three categories
boxplot(movies_rating_budget$rating ~ movies_rating_budget$cat, xlab = 'Budget', ylab = 'Rating', names = c('Low', 'Medium', 'High'))
Xbar = mean(movies_rating_budget$rating); Xbar
abline(h = Xbar, col = 'red', lwd = 2)

# We perform ANOVA test
# We set H0: mu(low) = mu(mid) = mu(high) and H1: not all mu(i) are equal
aov(movies_rating_budget$rating ~ factor(movies_rating_budget$cat))
summary(aov(movies_rating_budget$rating ~ factor(movies_rating_budget$cat)))
# The p-value is 1.27e-15 hence we may reject the null hypothesis

# Since the null hypothesis is rejected, we may perform pairwise comparisons
pairwise.t.test(movies_rating_budget$rating, movies_rating_budget$cat, p.adjust.method = 'none')
# We may accept that mu(mid) = mu(high) but not that mu(low) = mu(mid) nor that mu(low) = mu(high)


# Section 4.2.5: Rating vs log(roi+1)
# Based on the revenue, we split it into two categories
n = nrow(movies_cleaned)
movies_rating_roi = movies_cleaned %>% select(rating, roi)
movies_rating_roi %>% mutate(cat = numeric(n))
for (i in 1:n) {
  if (movies_rating_roi[i,'roi'] < 0) {
    movies_rating_roi[i,'cat'] = 1 # negative profit
  } else {
    movies_rating_roi[i,'cat'] = 2 # positive profit
  } 
}

# Perform boxplot on the three categories
boxplot(movies_rating_roi$rating ~ movies_rating_roi$cat, xlab = 'Profit', ylab = 'Rating', names = c('Negative', 'Positive'))
Xbar = mean(movies_rating_roi$rating); Xbar
abline(h = Xbar, col = 'red', lwd = 2)

# We first perform F-test to check whether both categories have the same variance
var.test(movies_rating_roi$rating ~ movies_rating_roi$cat)
# Since p-value = 0.00097, the test suggests that the variances are different

# We perform t-test
# We set H0: mu(negative) = mu(positive) and H1: mu(negative) != mu(positive)
t.test(movies_rating_roi$rating ~ movies_rating_roi$cat, var.equal = F)
# The p-value < 2.2e-16 hence we can reject the null hypothesis and conclude that the mean are different


# Section 4.3: Statistical test between categorical variables
movies_g <- unnest(movies_cleaned, genre)
movies_g=movies_g%>%filter(genre!= "TV"& genre!="Movie")%>%filter(released_year>=2000)

# Section 4.3.1: Revenue vs genre
ggplot(movies_g,aes(genre,log(revenue)))+geom_boxplot()+theme(axis.text.x = element_text(angle = 45, hjust=1))+geom_hline(aes(yintercept=18,col="red"))

movies_g2=movies_g%>%filter(genre=="Action"|genre=="Adventure"|genre=="Animation"|genre=="Family"|genre=="Fantasy"|genre=="Science Fiction")
ggplot(movies_g2,aes(released_month,log(revenue),col=genre))+geom_smooth(se=F)


# Section 4.3.2: Rating vs genre
ggplot(movies_g, aes(genre, rating)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+geom_hline(aes(yintercept=6.4),col="red")


# Section 4.3.3: Revenue vs released month
ggplot(movies_g, aes(x = factor(released_month), y = log(revenue))) +
  geom_boxplot() +
  labs(x = "Month", y = "Log(Revenue)")

summary(aov(log(movies_g$revenue) ~ factor(movies_g$released_month)))
movies_g=movies_g%>%mutate(quarter = ceiling(released_month/3))
ggplot(movies_g, aes(x = factor(quarter), y = log(revenue))) +geom_boxplot() +labs(x = "Month", y = "Rating")
summary(aov(log(movies_g$revenue) ~ factor(movies_g$quarter)))


# Section 4.3.4: Rating vs released month
ggplot(movies_g, aes(x = factor(released_month), y = rating)) +
  geom_boxplot() +
  labs(x = "Month", y = "Log(Revenue)")+geom_hline(aes(yintercept=6.2,col="red"))

summary(aov(movies_g$rating ~ factor(movies_g$released_month)))
ggplot(movies_g, aes(x = factor(quarter), y = rating)) +geom_boxplot() +labs(x = "Month", y = "Rating")
summary(aov(movies_g$rating ~ factor(movies_g$quarter)))
pairwise.t.test(movies_g$rating,movies_g$quarter,alternative="greater")

top_rating_genres <- movies_g %>% 
  group_by(genre) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  arrange(desc(avg_rating)) %>% 
  head(6)

ggplot(top_rating_genres, aes(x = genre, y = avg_rating)) +
  geom_bar(stat = "identity") +
  xlab("Genre") +
  ylab("Average Rating") +
  ggtitle("Top Genres by Average Rating")

movies_g3=movies_g%>%filter(genre=="Documentary"|genre=="Drama"|genre=="History"|genre=="Music"|genre=="War"|genre=="Western")

ggplot(movies_g3,aes(released_year,rating,col=genre))+geom_smooth(se=F)
ggplot(movies_g3,aes(released_month,rating,col=genre))+geom_smooth(se=F)

# Perform ANOVA test to see if any month have better ratings
# H0: The mean ratings are equal across all of the months
# H1: At least one month has different mean rating than the others
aov(movies_g3$rating ~ factor(movies_g3$released_month))
summary(aov(movies_g3$rating ~ factor(movies_g3$released_month)))
# The p-value is 6.73e-07, much smaller than 0.05
# Hence, we reject the null hypothesis and conclude that there is a significant difference in mean ratings across different months.


# For 4.2.6 table
lmodel_roi_budget = lm(log(roi+1)~log(budget), data = movies_cleaned)
summary(lmodel_roi_budget)
qqnorm(lmodel_roi_budget$residuals)
qqline(lmodel_roi_budget$residuals, col = 'red')

lmodel_roi_revenue = lm(log(roi+1)~log(revenue), data = movies_cleaned)
summary(lmodel_roi_revenue)
qqnorm(lmodel_roi_revenue$residuals)
qqline(lmodel_roi_revenue$residuals, col = 'red')

lmodel_roi_runtime = lm(log(roi+1)~runtime, data = movies_cleaned)
summary(lmodel_roi_runtime)
qqnorm(lmodel_roi_runtime$residuals)
qqline(lmodel_roi_runtime$residuals, col = 'red')

lmodel_roi_rating = lm(log(roi+1)~rating, data = movies_cleaned)
summary(lmodel_roi_rating)
qqnorm(lmodel_roi_rating$residuals)
qqline(lmodel_roi_rating$residuals, col = 'red')

# Multiple Linear Regression
lmodel_roi = lm(log(roi+1)~runtime+log(revenue)+rating, data = movies_cleaned)
summary(lmodel_roi)

# For 4.2.7 table
lmodel_rating_roi = lm(rating~log(roi+1), data = movies_cleaned)
summary(lmodel_rating_roi)
qqnorm(lmodel_rating_roi$residuals)
qqline(lmodel_rating_roi$residuals, col = 'red')

lmodel_rating_budget = lm(rating~log(budget), data = movies_cleaned)
summary(lmodel_rating_budget)
qqnorm(lmodel_rating_budget$residuals)
qqline(lmodel_rating_budget$residuals, col = 'red')

lmodel_rating_revenue = lm(rating~log(revenue), data = movies_cleaned)
summary(lmodel_rating_revenue)
qqnorm(lmodel_rating_revenue$residuals)
qqline(lmodel_rating_revenue$residuals, col = 'red')

lmodel_rating_runtime = lm(rating~runtime, data = movies_cleaned)
summary(lmodel_rating_runtime)
qqnorm(lmodel_rating_runtime$residuals)
qqline(lmodel_rating_runtime$residuals, col = 'red')

# Multiple Linear Regression
lmodel_rating = lm(rating~log(revenue)+runtime+log(budget), data = movies_cleaned)
summary(lmodel_rating)



# Extra codes
# Top 20 movies by highest roi
head(arrange(movies_cleaned, desc(roi)), 20)

# Top 20 movies by highest rating
head(arrange(movies_cleaned, desc(rating)), 20)


# Extra Section 1: Budget by years (categorical)
ggplot(subset(movies_cleaned, released_year >= 2000), aes(x = as.factor(released_year), y = budget/1000000)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Budget (in million)") +
  ggtitle("Relationship between budget and year")

movies_cleaned$released_year <- as.factor(movies_cleaned$released_year)
aov_result <- aov(budget ~ released_year, data = movies_cleaned)
summary(aov_result)


# Extra Section 2: Revenue by years (categorical)
ggplot(subset(movies_cleaned, released_year >= 2000 & revenue <= 1000000000), aes(x = as.factor(released_year), y = revenue/1000000)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Revenue (in million)") +
  ggtitle("Relationship between revenue and year")

aov_result <- aov(revenue ~ released_year, data = movies_completed)
summary(aov_result)