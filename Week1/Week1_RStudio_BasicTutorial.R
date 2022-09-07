# Welcome to R! ####
# The hashtag (#) at the beginning of this line means R doesn't run it.
"But this line is run by R" # Hit Ctrl+Enter to see what I mean. (This runs the current line or highlighted section)
  # You can also use the "Run" button in the top right corner of this panel.

# And the four hashtags after the text in Line 1 create a section break within the script.
  # Click the pull-down arrow next to the '1' on the first line to see what I mean, then click the arrow again.
  # You can also 'jump' to different sections with the gold hashtag in the bottom left of this pane.

# I will use notes like this throughout my scripts, and expect you to do the same.
  # This is a great way to remind yourself of what you are doing - and why - when you open the file days, or weeks, or months...or decades later.

#First let's start with some basic math: 
1+1 
#Don't forget to hit Crtl + Enter, and no need for the '=' here. The equal symbol has a different meaning in scripts.
  # A double '=' will tell you if the equation is right - try running the next two lines:
1+1==1+1
1+1==1+2

# Other arithmetic works the same way - run the following lines:
2*10
10/2
2^10

#But what if you want to save the answer to one of these equations? You can create an 'object' in R with '<-' as follows:
a <- 2+2
a

#Look in the top right panel - the object 'a' now appears. Anytime you type 'a' it will call the object you've just created. 
  # But say you want to use 'a' as just the letter 'a'? Use quotations:
"a"
# OR
'a'
# will both work and without quotations will still give you the object created earlier.
  # We will come back to creating objects soon.

# Data formats in R ####
# Data in R can follow many formats and structures. 
  # In general, we will be dealing with vectors, factors, data frames, and lists. We will skip lists for now.
## Vectors - the most basic format of data in R. 
  # Vectors can take the forms of: integer, double, character, logical, complex, or raw.
    # For now, we will only look at integer, double, and character formats.
          # Integer and double are both numeric formats while character vectors are read as text.
          # For our purposes the integer and double formats are equivalent since both are numeric. 
# To create a vector we will use the "combine" function abbreviated 'c', and create the object 'a':

a <- c(1,1,2,3,5)
a
    # Notice that we have over-written our value of 'a' from earlier. Remember this feature of R.
    # TIP: If you are unsure what a function will do, you can use the built-in R help files with a question mark:
?c
  # You can read through the usage documentation for the function in the bottom right panel.

# Now to confirm the object created has been 'read' by R in the format we expect we will use the typeof() function:
typeof(a)
    # Notice the output in the console is "double". This tells us it is both a vector and numeric.

# Now see what happens if we add a non-numeric value to the c() function. Don't forget the quotation marks:
b <- c(1,1,2,3,x)
  # Without quotes the text is read as an object by R and creates an error, even if an object with that value does not exist.
b <- c(1,1,2,3,'x')
b
typeof(b)
    # simply adding one non-numeric value resulted in a character (i.e. text) value for the numbers as well.
    # Notice the quotation marks now present? These indicate it is a character value rather than numeric.

# We can 'force' R to read the numbers as numeric values, but it removes text values from the vector.
as.numeric(b)
    # Look at the output below. The red warning message tells you text values have been replaced with NAs.
    # We have not created an object with this line as the '<-' and object name were not included. 
      # Results only printed to the console and were not saved.


## Factors ##
# Factors represent categorical values with a fixed number of "levels".
  # These levels do not need to have a low-high value gradient (e.g. could be colors).
# Factors can be created with the nice and direct "factor" function:
c <- factor(b)
c
    # Note the output in the console now includes a second line for "levels", and '1' is only included once.
    # The levels will list all unique values once, regardless of the number of times they occur.

# Factors are built on top of vector integers. This really is not important for our purposes, except:
typeof(c)
    # results in an output of "integer". So keep this in mind, and a second test of:
is.factor(c)
    # will tell you if it is (TRUE) or isn't (FALSE) a factor.

## Data Frames ##
# Now it's time to bring vectors and factors together into a single object, a data frame.
  # Data frames can be thought of as equivalent to tables or spreadsheets in excel.

# First: let's create a couple longer vectors as strictly numeric or text. I will overwrite the objects we created earlier:

a <- c(0,1,1,2,3,5,8,13,21)
b <- c(8,8,8,4,4,4,2,2,2)
c <- c('a','a','a','b','b','b','c','c','c')
d <- c("Brighton","Notts","Ipswich","Luton","Burton","Oldham","Oxford","Bristol","Leyton")

  # Note that all three new vectors are the same length - they are each exactly 9 values long.
    # This will be important for creating the data frame.
# The next step is to combine this vectors with the "column bind" or cbind() function:

data <- cbind(a,b,c,d)
data
  # Look at the output in the console. You'll see all values are in quotation marks. 
    # Including a vector (now a column) with text converted all values to character.

#To reformat the data to allow for numeric values we will use the as.data.frame() function:
df <- as.data.frame(data)
df
  # 'df' is an abbreviation for 'data frame' that is commonly used by R coders for creating examples.
  # Notice how the quotation marks are no longer present.
    # Also notice this now looks like a table. To the left of the values are row names and to the top are column names.
  # Row names defualt to a numeric count and column names are the vector object names as combined.
    # Let's change the column and row names to something more descriptive, starting with columns.
    # To only change part of the data frame, we will need a function on both sides of '<-' to direct where the change occurs: 
colnames(df) <- c("Fibonacci", "Division", "Factors", "Foosball")
df
    # colnames() indicates these values will replace the column names within data frame 'df'.

# Next we will change the row names to be represented by towns with sporty teams instead:
row.names(df) <- df$Foosball
df
  # The numbers have now been replaced by sporty sport team towns.
  # The dollar sign ($) in the above operation is used to call a column within the data frame by name.

# Now it seems a bit silly to have these towns listed both in the data frame itself and as row names, so let's delete the town column.
  # To do this we can delete the column or overwrite the existing df object with a subset of the data frame.
  # A subset is a portion of the original R object based on some selection criteria.
      # So let's start with how to select part of a data frame using the [row, column] method in R:
df[1,]
#Versus:
df[,1]
  #The location of your comma is very important - the first operation selects a row and the second a column.
  # Or, for a single cell:
df[1,1]
  # Or, for multiple rows or columns we can use a colon:
df[1:2,]
df[,1:2]
  
# To remove a column or row we will use the minus (-) symbol:
df[-1,]
df[,-1]

 # So to remove the "Foosball" column we have two options with the same result:
df.a <- df[,1:3]
df.a
#OR#
df.b <- df[,-4]
df.b

# If you want to be absolutely sure the results are identical, we can use the double equals (==) logical to check all cells:

df.a == df.b
    # All cells return a "TRUE" value so the data frames are identical.

# What if you want your columns as rows and rows as columns (i.e. transposed)? Simply:
df.t <- t(df.a)
df.t

# Or maybe you need to add new data to an existing data frame. This can be done with our friend cbind()

binary.solo <- c(0,0,0,0,0,0,1,1,1)

new.df <- cbind(df.a, binary.solo)
new.df
# Or if we need additional rows instead, the process is similar with an extra step:

Chelsea <- data.frame(34,1,"d")
Chelsea
  # The names of columns do not match, so we need to correct this before binding:
colnames(Chelsea) <- colnames(df.a)    
rbind(df.a, Chelsea)
  # Whoops! We have created an error for the numeric values. This is where the levels of a factor can be a problem.
  # There are many ways to correct this issue. For now, we will use as.numeric() and as.character() to overwrite each numeric column:
  # The need for both functions is a legacy of how R was built initially. Only using as.numeric() will rank values instead.
df.a$Fibonacci <- as.numeric(as.character(df.a$Fibonacci))
df.a$Division <- as.numeric(as.character(df.a$Division))

#Now retry:
df.r <- rbind(df.a, Chelsea)
df.r
  #Great! But notice the new row name? We can fix that with a combination of multiple functions we've learned:
row.names(df.r) <- c(row.names(df.a[1:9,]),"Chelsea")
df.r
  # Look closely at the above operation. We have nested a subset function "df.a[1:9,]" within the row.names() function
  # This is then nested within the c() function to bind all values into a single vector and replaces the row names.


## Performing operations on entire rows in a data frame ##
# Sometimes you might want to create a new vector or quickly compare the values in two columns of the data frame against each other.
# This can be done with similar syntax to the single-value operations we performed at the beginning of this tutorial.

# For example, say you would like to know how each Division value compares to the mean Fibonacci within our data frame:
ratio <- df.r$Division/mean(df.r$Fibonacci) #ratio by a single value
ratio
# you can see all of the Division values are less than the mean Fibonacci, some are 90% of the mean, and some are only 11%.


#You can also compare values row by row. Say you want to know if the Fibonacci or Division values are larger by location (i.e., row) and how much larger one is than the other:

by.row <- df.r$Division/df.r$Fibonacci #for comparing values within individual samples against each other.
by.row

# notice we have much different values here, with different meanings. Values greater than 1 mean that Division is larger, and less than one that Fibonacci is larger.
# This is difficult to read though as a vector across the bottom of the console, so let's add it to the data frame:


df.r <- cbind(df.r, by.row)
df.r
# notice the vector we created has been given a column name that matches its object name. Now it is much easier to look across the columns and identify which city/team compares to which value.



## Checking your Data Frame ##
# There are several functions built into R that can be used to check the structure of your data or summarize it.
    # These will become increasingly important as the size of our datasets increase throughout the semester.

# The summary() function will produce an output with basic descriptive statistics of our data frame or other R objects:
summary(df.r)

# The head() function wil display the first 6 rows of a data frame in the console below. This will be handy for very large data frames:
head(df.r)

# You can also check the dimensions of your data frame with the 'number of rows' and 'number of columns' functions: 
nrow(df.r)
ncol(df.r)

#If you would like to inspect the data frame more closely and have difficulty with the console display, try View():
View(df.r)


# Saving and Reading Data ####

#set the working directory - this is where your data exist and the files you create will be saved.
#Make sure this location follows the GitHub directions EXACTLY. I can not stress enough, if you do not take time to do this correctly, it will cause problems for you all semester.
  # R uses forward slashes instead of backslashes for file paths. 
  # If you copy/paste the path from File Explorer you will need to replace the slashes.
  # The directory needs to be contained within quotes. 

#My working directory - set with the Set Working Directory function setwd():
setwd("C:/GitHub/R4Eco_2022/Week1")
  # Replace "wilsonmj" with your repository name, this should be your lastname+first initial+middle initial.
### If you are using a mac replace the 'C:' at the beginning of the directory with '~':
      # If this does not work, then remove the '~' at the beginning of the folder path.
#Replace my working directory with yours.

#How do you know this line worked? By check with GET Working Directory function getwd():
getwd() 
#Notice there is nothing between the parentheses. All functions, even when checking the 'system',
#    require parentheses for R to recognize them as functions

#Working between Mac's and PC's can get confusing and it is easy to mix up the pathways. To get around this problem, I wrote a new function that you can use, called smart_wd():
#Because this is not a built-in function of R, you will need to rerun it everytime you reopen R to re-add it to your global environment.
smart_wd <- function (repo, folder=NULL) {
  if(missing(folder))
  {
    setwd(print(paste(if (Sys.info()[["sysname"]]=="Windows") {("C:/GitHub")} else {
      if (Sys.info()[["sysname"]]=="Darwin"){("~/GitHub")} else ("for Linux run: setwd('/home/[INSERT YOUR USERNAME]/GitHub')")
    },"/",repo, sep = "")))}
  else 
    
  {
    setwd(print(paste(if (Sys.info()[["sysname"]]=="Windows") {("C:/GitHub")} else {
      if (Sys.info()[["sysname"]]=="Darwin"){("~/GitHub")} else ("for Linux run: setwd('/home/[INSERT YOUR USERNAME]/GitHub')")
    },"/",repo, "/", folder, sep = "")))}
}


#This function assumes you are working with GitHub desktop in the format we have discussed in class. To set the working directory simply identify the name of the repo and, 
# if you would like, the folder you are working in. The folder command is optional. This function will identify what operating system you are running and correctly map the 
# working directory based on that operating system.

smart_wd(repo = "R4Eco_2022", folder = "Week1")
  # notice that it has printed the new working directory in the console - I did this to be able to confirm the location without using getwd().
  # A copy of this function is saved in a stand-alone script in the "Functions" folder for our course repository on GitHub.
  # DON'T FORGET to change the repo to your personal repository!

# With the working directory set, we will 'write' our data frame to a file and save it into the working directory folder.
  # There are many ways to write files in R. We will primarily use comma separated value (.csv) files. 
  # This is a common format that is easily understood by both Microsoft Excel and LibreOffice Calc.

#For this example, let's use the data frame after we added the row for Chelsea:


write.csv(df.r, file = "Example.csv")
  # Now open the working directory folder on your computer. You should see a new file called "Example.csv". 

# Double click on the file, it should open with Excel by default.
  # You can see the data we have been working with in an Excel spreadsheet.

# Conversely, we will typically start with a spreadsheet that we wish to import into R as a data frame.
# There are specialized functions to read almost any file, but we will work primarily with .csv files throughout the semester. 
    # If your data are in a .xlsx format use File -> Save As -> Comma separated (.csv) to reformat the spreadsheet before importing.


#Now try bringing our Example.csv back into R.

read.df <- read.csv('Example.csv')
read.df
    #Hmmm - this data frame looks a little different. Notice the row names have become a row?

# In our case, the data have both row names and column names, so we need to tell the function this:

read.df <- read.csv('Example.csv', row.names = 1, header = TRUE)
read.df
  # The syntax here is slightly different. First we specify that the first column is actually the row names. 
    # Then the 'header' command identifies the first row as column names.
    # The data frame should now follow the structure of df.r as it was exported.




# Next week we will begin plotting and exploring data.


