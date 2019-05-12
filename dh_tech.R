#PART 1 


transactions <- read.csv('C:/Users/ncuri_000/OneDrive/Documents/dh_transactions.csv', header=TRUE)
causalLookup <- read.csv('C:/Users/ncuri_000/OneDrive/Documents/dh_causal_lookup.csv', header=TRUE)
product <- read.csv('C:/Users/ncuri_000/OneDrive/Documents/dh_product_lookup.csv', header=TRUE)
store <- read.csv('C:/Users/ncuri_000/OneDrive/Documents/dh_store_lookup.csv', header=TRUE)

head(product)

#Question 1A & 1B

#subset the data to isolate by commodity
panMix <- product[product$commodity == 'pancake mixes',]
sort(table(panMix$product_description),decreasing=TRUE)[1:5]
sort(table(panMix$brand),decreasing=TRUE)[1:5]

"The 5 most common products in pancake mix are Private Label Complete Pancake Mix, Aunt Jemima Buttermilk Complete
Pancake Mix, Aunt Jemima Complete Pancake Mix, Aunt Jemima Buttermilk Pancake Mix, and Aunt Jemima Original
Pancake Mix. The top 5 brands are Hungry Jack, Aunt Jemima, Bisquick, Mrs Butterworth, and Fatshake. This
is interesting because the top 5 products don't necessarily tell us about the most common brands, and vice versa.
This likely means that there are brands with many products that aren't necessarily the most popular. We can see
Aunt Jemima has many brands, and is very popular." 


syrups <- product[product$commodity == 'syrups',]
sort(table(syrups$product_description), decreasing=TRUE)[1:5]
sort(table(syrups$brand), decreasing=TRUE)[1:5]

"The 5 most common products in syrups are Private Label Lite Pancake Syrup, Aunt Jemima Lite Syrup, Karo Blue
Label Syrup, Karo Blue Syrup, and Karo Green Label Syrup. The top 5 brands here are Karo, Log Cabin, Private Label,
Aunt Jemima, and Northwoods. Here a similar thing is happening, and we can also see that Karo drives sales in the
syrups commodity."

pasta <- product[product$commodity == 'pasta',]
sort(table(pasta$product_description), decreasing=TRUE)[1:5]
sort(table(pasta$brand), decreasing=TRUE)[1:5]

"The 5 most common products in pasta are Private Label Elbow Macaroni, Private Label Spaghetti, Private Label
Spaghetti Regular, Private Label Thin Spaghetti, and Barilla Elbow Pasta. Here, the top brands are Barilla, 
Ronzoni, San Giorgio, DaVinci, and Mueller. Here, it is difficult to tell which brand drives sales because some
of the products have a private label. However, I would say Barilla is a good guess."

sauce <- product[product$commodity == 'pasta sauce',]
sort(table(sauce$product_description), decreasing=TRUE)[1:5]
sort(table(sauce$brand), decreasing=TRUE)[1:5]

"The top 5 products in the pasta sauce commodity are Classico Sun Dried Tomatio, Prego Mushroom Spaghetti Sauce,
Prego Italian Sausage, Prego Regular Spaghetti Sauce, and Prego Spaghetti Sauce Meat. The 5 most popular brands 
are Ragu, Prego, Private Label, Classico, and Patsy's. Prego drives sales within the pasta sauce commodity."


##Question 1C
#Here, I realized this analysis is for the pasta commodity only because our client is the category manager for pasta.
dh_merge <- merge(product, transactions, by = "upc")
dh_merge_pasta <- subset(dh_merge, dh_merge$commodity == "pasta")

sort(table(dh_merge_pasta$brand),decreasing=TRUE)[1:6]
prop.table(table(dh_merge_pasta$geography))
sort(table(dh_merge_pasta$week),decreasing=TRUE)[1:6]
prop.table(table(dh_merge_pasta$coupon))

"Within the pasta commodity, Barilla, Creamette, Mueller, and Ronzoni drive sales. While pretty evenly split,
customers in geography 1 contribute slightly more to sales. Customers who shopped for pasta weeks 75-80 also
contributed a lot to sales. Sales are largely made up of customers who did not use a coupon."

#Question 1D
panMerge <- merge(panMix, transactions, by = 'upc')

#each basket/trip will be unique but not each household will be unique
#how many times each household shows up in the data set will give repeat customer
panFreq <- as.data.frame((table(panMerge$household))) #contingency table
panFreq$freq2 <- ifelse(panFreq$Freq == 1,0,1) #binary 
sum(panFreq$freq2==1) #count of repeat
sum(panFreq$freq2==1)/length(panFreq$Freq) #repeat rate for pancake mix is about 40%

syrMerge <- merge(syrups, transactions, by = 'upc')
syrFreq <- as.data.frame((table(syrMerge$household)))
syrFreq$freq2 <- ifelse(syrFreq$Freq == 1,0,1)
sum(syrFreq$freq2==1)
sum(syrFreq$freq2==1)/length(syrFreq$Freq) #repeat rate for syrup is about 51%

pastaMerge <- merge(pasta, transactions, by = 'upc')
pastaFreq <- as.data.frame((table(pastaMerge$household)))
pastaFreq$freq2 <- ifelse(pastaFreq$Freq == 1,0,1)
sum(pastaFreq$freq2==1)
sum(pastaFreq$freq2==1)/length(pastaFreq$Freq) #repeat rate for syrup is about 69%

sauceMerge <- merge(sauce, transactions, by = 'upc')
sauceFreq <- as.data.frame((table(sauceMerge$household)))
sauceFreq$freq2 <- ifelse(sauceFreq$Freq == 1,0,1)
sum(sauceFreq$freq2==1)
sum(sauceFreq$freq2==1)/length(sauceFreq$Freq) #repeat rate for syrup is about 70%

#Question 1E

"Pasta has among the highest repeat rates and I would say it fares well among all commodities."

#Question 1F

"Pasta and pasta sauce have much higher repeat rates than panackes and syrup. I would recommend advertsing these
two commodities more in order to increase repeat rate and drive revenue for these commodities."

#PART 2

# Step 0: Clear everything in the memory
rm(list=ls())
graphics.off()

#call the library
library(dplyr)

# Set the working directory
setwd("/testfolder/jn_custom_progs/SAI/") 

# display the current working directory
getwd()

#### Input variables

# Input is the output from 05 code

#read the csv file
all_add_order <- read.csv("outputs/OLDall_add_order.csv")

#find all unique values for segment in the data set
seglist <- unique(all_add_order$segment)
#create a data frame with these 3 variables as character values
rec <- data.frame(segment = as.character(), category = as.character(), dim_return = as.integer() )

#for loop with the same length as unique segments
for (segIterator in seglist){
  #print(class(storeIterator)) #this is a comment
  #creates a subset of all_add_order, where each iteration is a unique value of segment
  df1 <- subset(all_add_order, segment == segIterator)
  #store unique values in category as catcodes
  catcodes <- unique(df1$category)
  
  #for each unique value in catcodes
  for (cat in catcodes){
    #create a data frame that's a subset of df1, where each iteration is a unique value of category
    df <- subset(df1, category== cat)
    #order df by segment, category, and final rank (respectively) and store it as df2
    df2 <- df[order(df$segment,df$category,df$final_rank),]
    #find the cumulative sum of the mean scores in df2
    df2$cum_cps <- cumsum(df2$mean_score)
    #apply the function to df2 split by segment and category, and store it in a vector that extracts contents
    df2$prev <- unlist(by (df2, list(df2$segment, df2$category), 
                           function(x) c(NA, head(x$cum_cps, -1)))) #creates a function that creates a vector with NA and the last value cumulative mean score value
    #subtract the last sum of mean from the cumulative sum of means
    df2$gradient <- df2$cum_cps - df2$prev
    
    #stores data frames for each unique segment and category, where dim_return is the minimum value of gradient that is less than .01
    record <- data.frame(segment = segIterator, category = cat, dim_return = min(which(df2$gradient < 0.01)) )
    #line plot the cumulative sum of means, title it cat, y axis named cumulative score, x axis named SKU rank
    plot(df2$cum_cps, main = cat, type = "l", ylab = "Cumulative Score", xlab = "SKU rank" )
    #update rec by combining rec and record by row
    rec<-rbind(rec, record)
  }
}
