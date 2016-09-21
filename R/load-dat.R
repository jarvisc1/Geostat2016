# Aim: load and add vars to the learning dataset

learn = readr::read_csv("dat/train.csv")
validate = readr::read_csv("dat/validation.csv")

# # get frequency of each species within parent
# library(dplyr)
# learn$parent <- as.factor(learn$TAXOUSDA)
# learn$child <- as.factor(learn$TAXNUSDA)
# 
# # count the number within
# parent_count <- learn %>% group_by(parent) %>% summarise(parent_n = n()) 
# child_count <- learn %>% group_by(parent, child) %>% summarise(child_n = n()) 
# 
# # join the counts onto the data
# learn_count <- left_join(learn, parent_count)
# learn_count <- left_join(learn_count, child_count)
# # put overall denominator
# learn_count$denom <- length(learn$X) 
# 
# # can now calculate the frequencies for each
# sort(table(learn$TAXOUSDA))
# 
# plotter <- function(place = "Udolls"){
#   restrict <- learn$TAXOUSDA== place
#   plot(learn$X, learn$Y, type = "n", main = place)
#   points(learn$X[restrict], learn$Y[restrict])
# }

# plotter("Udolls")
# plotter("Udalfs")
# plotter("Aquolls")
# plotter("Aqualfs")
# plot(learn$X, learn$Y)


  
