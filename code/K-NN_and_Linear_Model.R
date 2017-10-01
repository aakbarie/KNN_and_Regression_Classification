
# Excercise 2.8 ---------------------------------------------------------------------------------------------------

library(tidyverse)
library(ggfortify)
library(scatterplot3d)
library(caret)

# download data from book site ------------------------------------------------------------------------------------

url_train <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.train.gz"
url_test <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz"

# creete folder for downloading data and dowload data into it
dir.create("data")


download.file(url_train, "data/zip_train.gz")
download.file(url_test, "data/zip_test.gz")

# load data -------------------------------------------------------------------------------------------------------

# function to exract any two digits for classification
digit_subset <- function(dig_1, dig_2, data_in) {
     data_in %>%
          mutate(class = ifelse(V1 == dig_1, dig_1, 
                                ifelse(V1 == dig_2, dig_2,
                                       "other"))) %>%
          dplyr::filter(class != "other") %>%
          select(-V1)
}


train_data <- digit_subset(7, 9, read.table("data/zip_train.gz"))

test_data <- digit_subset(7, 9, read.table("data/zip_test.gz"))


# data preprocessing ----------------------------------------------------------------------------------------------

# EDA -------------------------------------------------------------------------------------------------------------

# first we will consider principal components plots
ans <- train_data %>%
     select(-class) %>%
     princomp(.)

# lets look at the distribution of 7 versus other numbers in the 
#  first two principle components
autoplot(prcomp(train_data[,-257]), data = train_data, colour = "class") +
     xlab("First Principle Component") +
     ylab("Second Principle Component") 


# lets look at the distribution of 7 in the first three principle components     


prcpl_cmpnt <- tibble(pca_1 = ans$scores[,1], 
                      pca_2 = ans$scores[,2], 
                      pca_3 = ans$scores[,3])

p <- plot_ly(prcpl_cmpnt, x = ~pca_1, y = ~pca_2, z = ~pca_3, 
             colors = c('#44A4DB', '#DB7B44')) %>%
     add_markers() %>%
     layout(scene = list(xaxis = list(title = 'PCA 1'),
                         yaxis = list(title = 'PCA 2'),
                         zaxis = list(title = 'PCA 3')))



# linear Regression model for binary class ------------------------------------------------------------------------

lm_fit <- lm(class ~ ., train_data)

summary(lm_fit)


# prediction based on linear model --------------------------------------------------------------------------------

testing <- test_data %>%
     select(-class)
test_results <- lm_fit %>%
     predict.lm(., newdata=testing)
test_results <- data.frame(test_results) %>%
     mutate(returned = 1*(test_results > 8.5))

te.er <- test_data %>%
     mutate(bin_classes = 1*(class > "7")) %>%
     select(bin_classes) %>%
     .$bin_classes

temp_er_table <- table(te.er, test_results$returned)
caret::confusionMatrix(temp_er_table)
error.test = (temp_er_table[1,2] + temp_er_table[2,1])/
     length(test_results$returned)
error.test
DT::datatable(temp_er_table)

train.dat <- train_data %>%
     select(-class)
test.dat  <- test_data %>%
     select(-class)
Y <- train_data$class
Y.test <- test_data$class
###########  k = 1 ###########
ans.knn.1 <- knn(train = train.dat, test=train.dat, 
                 k=1, cl=Y)
table(Y, ans.knn.1)
ans.knn.1 <- knn(train = train.dat, test=test.dat, 
                 k=1, cl=Y)
k.t.e1 = table(Y.test, ans.knn.1)
er.t.knn1 = (k.t.e1[1,2]+k.t.e1[2,1])/length(Y.test)
er.t.knn1

###########  k = 3 ###########
ans.knn.3 <- knn(train = train.dat, test=train.dat, 
                 k=3, cl=Y)
k.t.t3 = table(Y, ans.knn.3)
er.tr.knn3 = (k.t.t3[1,2]+k.t.t3[2,1])/length(Y)
er.tr.knn3

ans.knn.3 <- knn(train = train.dat, test=test.dat, 
                 k=3, cl=Y)
k.t.e3 = table(Y.test, ans.knn.3)
er.t.knn3 = (k.t.e3[1,2]+k.t.e3[2,1])/length(Y.test)
er.t.knn3

###########  k = 5 ###########
ans.knn.5 <- knn(train = train.dat, test=train.dat, 
                 k=5, cl=Y)
k.t.t5 = table(Y, ans.knn.5)
er.tr.knn5 = (k.t.t5[1,2]+k.t.t5[2,1])/length(Y)
er.tr.knn5


ans.knn.5 <- knn(train = train.dat, test=test.dat, 
                 k=5, cl=Y)
k.t.e5 = table(Y.test, ans.knn.5)
er.t.knn5 = (k.t.e5[1,2]+k.t.e5[2,1])/length(Y.test)
er.t.knn5


###########  k = 7 ###########
ans.knn.7 <- knn(train = train.dat, test=train.dat, 
                 k=7, cl=Y)
k.t.t7 = table(Y, ans.knn.7)
er.tr.knn7 = (k.t.t7[1,2]+k.t.t7[2,1])/length(Y)
er.tr.knn7

ans.knn.7 <- knn(train = train.dat, test=test.dat, 
                 k=7, cl=Y)
k.t.e7 = table(Y.test, ans.knn.7)
er.t.knn7 = (k.t.e7[1,2]+k.t.e7[2,1])/length(Y.test)
er.t.knn7

###########  k = 15 ###########
ans.knn.15 <- knn(train = train.dat, test=train.dat, 
                  k=15, cl=Y)
k.t.t15 = table(Y, ans.knn.15)
er.tr.knn15 = (k.t.t15[1,2]+k.t.t15[2,1])/length(Y)
er.tr.knn15

ans.knn.15 <- knn(train = train.dat, test=test.dat, 
                  k=15, cl=Y)
k.t.e15 = table(Y.test, ans.knn.15)
er.t.knn15 = (k.t.e15[1,2]+k.t.e15[2,1])/length(Y.test)
er.t.knn15


# let's put this together in a plot like Figure 2.4 of the text

train <- c(0, 9, 11, 19, 30)/1289
test  <- c(8, 8, 8, 9, 12)/324
k <- c(1,3,5,7,15)
reg.test  <- 14/324
reg.train <- 12/1289

plot(k, test, xlab="k", ylab="", type="b",
     lty=1, col=1, pch="x", ylim=c(0,0.05),
     main="Error Rate as a Function of k")
lines(k, train, lty=2, col=2, pch="o", type="b")
lines(c(1,15), c(reg.test, reg.test), lty=3, col=3)
lines(c(1,15), c(reg.train, reg.train), lty=4, col=4)
legend(9, .03, c("NN - Test Data","NN - Training Data", "Reg - Test", "Reg - Train"),
       lty=c(0,0,3,4), col=1:4, pch=c("x","o","", ""))

# print out error rates

cbind(k, train, test)
reg.train
reg.test
