# With the data frame you created last week you will:

unique.char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")
group.char <- c("a","a","a","a","a","b","b","b","b","b","c","c","c","c","c")
uniqu.num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
rep.num <- c(1,1,1,2,3,4,5,5,5,6,7,8,8,8,9)
dec.num <- c(1,2,3.3,4,5,6.6,7,8,9,10,11,12.2,13,14,15.5)

df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))

df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))

add.row <- data.frame("p","c",16,10,16)
colnames(add.row) <- colnames(df) 
df1 <- rbind(df, add.row)
row.names(df1) <- df1$unique.char
df1 <- df1[,-1]
df1

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
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,15))
b.plot <- arrows(b.plot, df.mean$Mean-df.sd$StanDev,
                 b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,15))
b.plot <- arrows(b.plot, df.mean$Mean-df.sd$StanDev,
                 b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
title(main = "Morgan's Barplot")
title(xlab = "Letter", ylab = "Number")

# Create a scatter plot between two of your numeric columns.
  # Change the point shape and color to something NOT used in the example.
  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

plot(df1$dec.num ~ df1$rep.num)
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response")
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Morgan's Scatter Plot")
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Morgan's Scatter Plot", pch=16, col ="slateblue1")

# Upload both plots with the script used to create them to GitHub.
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.

setwd("C:/GitHub/morgan")
jpeg(file = "Thomas_scatterplot", width = 6, height = 6)
par(family = "serif")
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Morgan's Scatter Plot", pch=16, col ="slateblue1")
dev.off
pdf(file = "Thomas_barplot", width = 4, height = 7)
par(family = "serif")
dev.off