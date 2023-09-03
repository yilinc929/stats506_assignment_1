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