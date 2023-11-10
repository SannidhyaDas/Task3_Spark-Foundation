
# @Spark Foundation
# Name : Sannidhya Das
# Domain : Data Science and Business Analytics
# Batch : GRIPNOVEMBER23

# Setting the working directory and loading Dataset
getwd()
setwd("C:/Users/sanni/OneDrive/Documents/Internship projects/Spark Foundation/Task3")
task3=read.csv("SampleSuperstore.csv")
#Checking the dataset 
head(task3)
tail(task3)
#Summary of the dataset 
summary(task3)
str(task3)
dim(task3)
colnames(task3)
#Checking is there is any null values in any columns 
colSums(is.na(task3))

#Checking the dataset for duplicates and dropping the duplicate elements using unique() 
sum(duplicated(task3))
task3=unique(task3)

#Finding the correlation and covariance of dataset using cor()and cov() method
cor(task3[,c("Sales","Quantity","Discount","Profit")])
cov(task3[,c("Sales","Quantity","Discount","Profit")])


# Group the data by multiple columns and calculate the sum of Quantity, Discount, Sales, and Profit
grouped <- aggregate(cbind(Quantity, Discount, Sales, Profit) ~ Ship.Mode + Segment + Category + Sub.Category + State + Region, data = task3, sum)

# Print the grouped data
print(grouped)

# Group the data by State and calculate the sum, mean, min, max, count, median, standard deviation, and variance of Profit
profit_summary <- task3 %>% group_by(State) %>% summarise(sum = sum(Profit), mean = mean(Profit), min = min(Profit), max = max(Profit), count = n(), median = median(Profit), std = sd(Profit), var = var(Profit))

# Print the summary statistics of Profit by State
print(profit_summary)

##Visualization of dataset

library(ggplot2)
ggplot(task3, aes(x = Sub.Category, y = Category)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Bar plot of Sub-Category vs Category") + 
  xlab("Sub-Category") + 
  ylab("Category")


library(dplyr)

task3 %>%
  count(State) %>%
  ggplot(aes(x = State, y = n)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Count of States") +
  xlab("State") +
  ylab("Count")


##Heatmap plot
library(reshape2)

# Create a correlation matrix
corr <- cor(task3[,c("Sales","Quantity","Discount","Profit")])

# Create a heatmap with annotations
ggplot(melt(corr), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Correlation Matrix") +
  xlab("Variables") +
  ylab("Variables")

# Create a covariance matrix
cov <- cov(task3[,c("Sales","Quantity","Discount","Profit")])

# Create a heatmap with annotations
ggplot(melt(cov), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Covariance Matrix") +
  xlab("Variables") +
  ylab("Variables")

# Create a count plot

ggplot(task3, aes(x = Segment, fill = Segment)) +
  geom_bar() +
  scale_fill_manual(values = c("red", "forestgreen", "steelblue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Count of Segments") +
  xlab("Segment") +
  ylab("Count")

ggplot(task3, aes(x = Region, fill = Region)) +
  geom_bar() +
  scale_fill_manual(values = c("red", "forestgreen", "steelblue","goldenrod")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Count of Region") +
  xlab("Region") +
  ylab("Count")

ggplot(task3, aes(x = Ship.Mode, fill = Ship.Mode)) +
  geom_bar() +
  scale_fill_manual(values = c("red", "forestgreen", "steelblue","goldenrod")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Count of Ship Mode") +
  xlab("Ship Mode") +
  ylab("Count")

ggplot(task3, aes(x = Category, fill = Category)) +
  geom_bar() +
  scale_fill_manual(values = c("forestgreen", "steelblue","goldenrod")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Count of Category") +
  xlab("Category") +
  ylab("Count")

# Create a bar plot with different colors for different bars
ggplot(task3, aes(x = Sub.Category, y = Profit, fill = Sub.Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "green", "blue", "orange","pink","steelblue","lightgreen","gold","purple","maroon","tan","cadetblue","tomato","orchid","navy","gainsboro","indianred")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Profit by Sub-Category") +
  xlab("Sub-Category") +
  ylab("Profit")
# Create a box plot with different colors for different bars
ggplot(task3, aes(x = Sub.Category, y = Profit, fill = Sub.Category)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "green", "blue", "orange","pink","steelblue","lightgreen","gold","purple","maroon","tan","cadetblue","tomato","orchid","navy","gainsboro","indianred")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Profit by Sub-Category") +
  xlab("Sub-Category") +
  ylab("Profit")

# Create a point plot with different colors for different lines
ggplot(task3, aes(x = Discount, y = Profit, color = Sub.Category)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Profit by Discount") +
  xlab("Discount") +
  ylab("Profit")


#Boxplot of Discount
ggplot(task3, aes(x = "", y = Discount)) +
  geom_boxplot(color="steelblue") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ggtitle("Box Plot of Discount") +
  ylab("Discount")

#Boxplot of profit
ggplot(task3, aes(x = "", y = Profit)) +
  geom_boxplot(color="steelblue") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ggtitle("Box Plot of Profit") +
  ylab("Profit")

#Distribution Plot
# Print the summary statistics of Sales
summary(task3$Sales)

# Create a histogram of Sales
hist(task3$Sales, col = "steelblue", breaks = 100, main = "Histogram of Sales", xlab = "Sales", ylab = "Frequency",xlim = c(0,5000))

##Scatter plot of sales 
ggplot(task3, aes(x = Sales, y = Profit)) +
  geom_point(color="seagreen") +
  labs(x = "Sales", y = "Profit") +
  ggtitle("Scatter plot of Sales vs. Profit")

#Histogram of all quantitative variables 

library(tidyr)
h <- gather(task3, key = "variable", value = "value", Sales:Profit)

ggplot(h, aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Value", y = "Frequency") +
  ggtitle("Histogram of Sales, Quantity, Discount, and Profit")

#Profit Associated with Sub-Category
profit_SubCategory <- aggregate(Profit ~ Sub.Category, data = task3, FUN = sum)

ggplot(profit_SubCategory, aes(x = Sub.Category, y = Profit)) +
  geom_bar(stat = "identity",fill= "Maroon") +
  labs(x = "Sub-Category", y = "Profit" ) +
  ggtitle("Profitable Sub-Category") +
  theme(plot.title = element_text(hjust = 0.5))

# Profit Associated with Segment 

profit_Segment <- aggregate(Profit ~ Segment, data = task3, FUN = sum)

ggplot(profit_Segment, aes(x = Segment, y = Profit)) +
  geom_bar(stat = "identity",fill= "brown") +
  facet_wrap(~ Segment, scales = "free_x") +
  labs(x = "Segment", y = "Profit") +
  ggtitle("Profitable Segment") +
  theme(plot.title = element_text(hjust = 0.5))

# Sales in Association with states 
sales_states <- aggregate(Sales ~ State, data = task3, FUN = sum)

ggplot(sales_states, aes(x = State, y = Sales)) +
  geom_bar(stat = "identity",fill="red") +
  labs(x = "State", y = "Sales") +
  ggtitle("Sales in each States")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Segment*Region wise profit

plot1 <- aggregate(Profit ~ Region + Segment, data = task3, FUN = sum)

ggplot(plot1, aes(x = Region, y = Profit, fill = Segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Region", y = "Profit", title = "Comparison of Profit by Region and Segment")  +
  theme(plot.title = element_text(hjust = 0.5))

# Profit associated with different categories 

plot2 <- aggregate(Profit ~ Region + Category, data = task3, FUN = sum)

ggplot(plot2, aes(x = Region, y = Profit, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Region", y = "Profit", title = "Comparison of Profit by Region and Category") +
  theme(plot.title = element_text(hjust = 0.5))

# Lineplots 
# 1.Profit vs Quantity 

plot3 <- aggregate(Profit ~ Quantity, data = task3, FUN = sum)

ggplot(plot3, aes(x = Quantity, y = Profit)) +
  geom_line(color = "steelblue") +
  labs(x = "Quantity", y = "Profit", title = "Profit vs. Quantity") +
  theme(plot.title = element_text(hjust = 0.5))

# 2. Profit vs Discount 

plot4 <- aggregate(Profit ~ Discount, data = task3, FUN = sum)

ggplot(plot4, aes(x = Discount, y = Profit)) +
  geom_line(color = "darkgreen") +
  labs(x = "Discount", y = "Profit", title = "Profit vs. Discount") +
  theme(plot.title = element_text(hjust = 0.5))

