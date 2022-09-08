# With the data frame you created last week you will:

unique.char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")
group.char <- c("a","a","a","b","c","c","c","d","e","e","e","f","g","g","g")
uniqu.num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
rep.num <- c(1,1,1,2,3,4,5,5,5,6,7,8,8,8,9)
dec.num <- c(1,2,3.3,4,5,6.6,7,8,9,10,11,12.2,13,14,15.5)

df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))
df
df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))
row.names(df) <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")
row.names(df)
df

df[-1]

add.row <- data.frame("p","h",16,10,16)
add.row
colnames(add.row) <- colnames(df) 
colnames(add.row)
df1 <- rbind(df, add.row)
df1
df1[-1]

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values
  # Add error bars with mean and standard deviation to the plot
  # Change the x and y labels and add a title
  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.

df.mean <- aggregate(df1$rep.num ~df1$group.char, FUN = "mean")
df.mean
colnames(df.mean) <- c("Factor","Mean")
df.mean
barplot(df.mean$Mean)
barplot(df.mean$Mean, names.arg = df.mean$Factor)
df.sd <- aggregate(df1$rep.num ~df1$group.char, FUN = "sd")
colnames(df.sd) <- c("Factor","StanDev")
df.sd
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)


# Create a scatter plot between two of your numeric columns.
  # Change the point shape and color to something NOT used in the example.
  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

plot(df1$dec.num ~ df1$rep.num)
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response")
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Morgan's Scatter Plot")

# Upload both plots with the script used to create them to GitHub.
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.