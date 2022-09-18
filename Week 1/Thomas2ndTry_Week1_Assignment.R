# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment
  # e.g. mine would be Wilson_Week1_Assignemnt


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures:
  # One character vector with all unique values
unique.char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")
  # One character vector with exactly 3 unique values
group.char <- c("a","a","a","a","a","b","b","b","b","b","c","c","c","c","c") #more than three unique values...
  # One numeric vector with all unique values
uniqu.num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  # One numeric vector with some repeated values (number of your choosing)
rep.num <- c(1,1,1,2,3,4,5,5,5,6,7,8,8,8,9)
  # One numeric vector with some decimal values (of your choosing)
dec.num <- c(1,2,3.3,4,5,6.6,7,8,9,10,11,12.2,13,14,15.5)

# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.
df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))
df
df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))
row.names(df) <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o") #sort of cheating/long way to do it, but you got the right answer!
row.names(df)
df

# Remove the character vector with unique values from the data frame.
df[-1]#this only prints it without the values. You forgot to use <- to create a new object including it.
df1 <- df[-1]
df1

# Add 1 row with unique numeric values to the data frame.
add.row <- data.frame("p","c",16,10,16)
add.row
colnames(add.row) <- colnames(df) 
colnames(add.row)
df1 <- rbind(df, add.row)
df1 <- df1[-1]
df1

# Export the data frame as a .csv file
setwd("C:/GitHub/morgan/data")
getwd()
write.csv(df1[-1], file = "Thomas2ndTry.csv")

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading.
summary(df1)

# Summary Statistics:

# unique.char        group.char         uniqu.num       rep.num      
# Length:16          Length:16          Min.   : 1.00   Min.   : 1.000  
# Class :character   Class :character   1st Qu.: 4.75   1st Qu.: 2.750  
# Mode  :character   Mode  :character   Median : 8.50   Median : 5.000  
# Mean   : 8.50      Mean   : 5.188  
# 3rd Qu.:12.25      3rd Qu.: 8.000  
# Max.   :16.00      Max.   :10.000 

# dec.num     
# Min.   : 1.00  
# 1st Qu.: 4.75  
# Median : 8.50  
# Mean   : 8.60  
# 3rd Qu.:12.40  
# Max.   :16.00

# Push your script and your .csv file to GitHub in a new "Week1" folder.
setwd("C:/GitHub/morgan/data")#this is right, but out of order. If you want to save the csv here this line needs to go above line 41.
getwd()