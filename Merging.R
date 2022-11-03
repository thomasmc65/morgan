merge(many dataframe, one dataframe, by = "Island", all = ?)  #For one -> many
aggregate(many$Size ~ Island, FUN = mean or FUN = SD)

#TRUE = keep everything from both dataframes
#FALSE = not sure if there something that's in one data frame and not the other so remove it

#Need to make sure data frames are same size before merging.
