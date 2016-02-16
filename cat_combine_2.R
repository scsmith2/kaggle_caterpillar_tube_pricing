setwd('C:/Projects/catCleanup')
cat_combine_1 <- read.csv("cat_combine_1.csv", as.is = T)
cat_kaggle_script <- read.csv("cat_kaggle_script.csv", as.is = T)

out <- data.frame(id = cat_combine_1[,1],
                  cost = (cat_combine_1[,2] +
                            cat_kaggle_script[,1])/2)
write.csv(out, file = "cat_combine_2.csv",
          quote = F, row.names = F)