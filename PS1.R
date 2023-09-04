#Problem 1
#a)
winedata<-read.csv("/Users/cathy/Desktop/wine/wine.csv",header=FALSE)
df<-data.frame(winedata)
print(df)
colnames(df)<-c("class","Alcohol","Malicacid","Ash","Alcalinity_of_ash","Magnesium",
                "Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins",
                "Color_intensity","Hue","OD280/OD315_of_diluted_wines","Proline")
print(df)
#b)
# Load dplyr
library(dplyr)
# Group by count using dplyr
wine_count<-df %>% group_by(class) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
wine_count
#The result matches the number of instances in the wine.names file.
# Convert tibble to df
#df2 <- agg_tbl %>% as.data.frame()
#df2

#c)
df[df$Alcohol==max(df$Alcohol),]#Extract the rows with highest alcohol content.
df[df$Alcohol==min(df$Alcohol),]#Extract the rows with lowest alcohol content.
count(df[df$Magnesium>114, ])#Extract the rows with magnesium higher than 114mg/l then count them.
df2<-df[df$Magnesium>114, ]
count_by_magnesium<- df2 %>% group_by(class) %>% summarise(total_count=n(),.groups = 'drop')
count_by_magnesium

#d)
# Using apply
all.means<-apply(df, 2, mean)
# Using colMeans
all.means<-colMeans(df)
#Applying the same method for 3 different classes.
df3<-df[df$class==1,]
means_c1<-apply(df3, 2, mean)
means_c1<-colMeans(df3)

df4<-df[df$class==2,]
means_c2<-apply(df4, 2, mean)
means_c2<-colMeans(df4)

df5<-df[df$class==3,]
means_c3<-apply(df5, 2, mean)
means_c3<-colMeans(df5)

#Combining these dataframe into one table.
avg_table =cbind.data.frame(all.means,means_c1,means_c2,means_c3)
avg_table

#e)
#t test on level of ashes of class 1 and class 2
t.test(df3$Ash, df4$Ash, var.equal = TRUE)
#t test on level of ashes of class 2 and class 3
t.test(df4$Ash, df5$Ash, var.equal = TRUE)
#t test on level of ashes of class 1 and class 3
t.test(df3$Ash, df5$Ash, var.equal = TRUE)
#Choosing a significant level of 0.01. 
#According the t test results of class 1 and 2, 
#since the p-value=3.493e-05 which is smaller than 0.01, the null hypothesis is rejected, 
#the difference between the means of level of ash in class 1 and 2 is statistically significant.

#According the t test results of class 2 and 3, 
#since the p-value=0.0002293 which is smaller than 0.01, the null hypothesis is rejected, 
#the difference between the means of level of ash in class 2 and 3 is statistically significant.

#According the t test results of class 1 and 3, 
#since the p-value=0.6499 which is greater than 0.01, the null hypothesis is not rejected, 
#the difference between the means of level of ash in class 1 and 3 is not statistically significant.


#Problem 2
#a)
isPerfectPower<-function(number,power) {
  if (number<1||power<2) {
    isPerfect<-FALSE
    return(list(isPerfect=isPerfect,root=NA))
  }
  
  root<-round(number^(1/power)) #calculate the root of the number for the given power 
  #and rounds it to the nearest integer. 
  #This root is then used to check if the number is a perfect power when raised to the specified power.
  
  if (root^power==number) {
    isPerfect<-TRUE
    return(list(isPerfect=isPerfect,root=root))
  } else {
    isPerfect<-FALSE
    return(list(isPerfect=isPerfect,root=root))
  }
}

isPerfectPower(16,2)
isPerfectPower(125,3)

#b)
findRootPower<-function(number) {
  for (power in 2:50) {  # This range of powers to check can be adjusted.
    result <- isPerfectPower(number,power)
    if (result[[1]]) {
      return(paste(number,"=",result[[2]],"^",power))
    }
  }
  return("Input is not a perfect power")
}

findRootPower(27)
findRootPower(13060694016)
findRootPower(7776)
findRootPower(170859375)
findRootPower(58247422)
findRootPower(94143178827)