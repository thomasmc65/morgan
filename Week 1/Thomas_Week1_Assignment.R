# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment
  # e.g. mine would be Wilson_Week1_Assignemnt


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures:
  # One character vector with all unique values
a <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")
  # One character vector with exactly 3 unique values
b <- c("a","a","a","b","c","c","c","d","e","e","e","f","g","g","g")
  # One numeric vector with all unique values
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  # One numeric vector with some repeated values (number of your choosing)
d <- c(1,1,1,2,3,4,5,5,5,6,7,8,8,8,9)
  # One numeric vector with some decimal values (of your choosing)
e <- c(1,2,3.3,4,5,6.6,7,8,9,10,11,12.2,13,14,15.5)

# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.
# Rename the columns to what???
cbind(a,b,c,d,e)
data <- cbind(a,b,c,d,e)
data
df <- as.data.frame(data)
df
row.names(df) <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")
df

# Remove the character vector with unique values from the data frame.
# It wouldn't let me do this when I had the comma in but it worked???
df[-1]

# Add 1 row with unique numeric values to the data frame.
p <- data.frame("p","h",16,10,16)
colnames(p) <- colnames(df)
df.r <- rbind(p,df)
df.r

# Export the data frame as a .csv file
write.csv(df.r, file = "Thomas.csv")
setwd("C:/GitHub/morgan/data")
getwd()

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading.
summary(df.r)

# Summary Statistics:

# a                  b                  c            
# Length:16          Length:16          Length:16         
# Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character  

# d                  e            
# Length:16          Length:16         
# Class :character   Class :character  
# Mode  :character   Mode  :character 

# Push your script and your .csv file to GitHub in a new "Week1" folder.
write.csv(df.r, file = "Thomas.csv")


