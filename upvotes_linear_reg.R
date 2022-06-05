
upvotes1 = read.csv("C:/Users/lenovo/Downloads/upvotestraindata.csv",nrows = 20000)
summary(upvotes1)
str(upvotes1)
head(upvotes1)
upvotes1[is.na(upvotes1)]=0
summary(upvotes1)
library(Amelia)
missmap(upvotes1,legend = TRUE,main = "to find out the upvotes",
        y.cex = 0.8,x.cex = 0.8)
str(upvotes1)
upvotes_num = upvotes1[sapply(upvotes1,is.numeric)]
upvotes_char = upvotes1[sapply(upvotes1,is.factor)]
names(upvotes_num)

for (i in 2:6)
{
  upvotes_num[,i+5] <- ifelse(upvotes_num[,i] > quantile(abs(upvotes_num[,i]),c(0.99)),
                              quantile(upvotes_num[,i],c(0.99)),upvotes_num[,i] )
}
names(upvotes_num)
summary(upvotes_num)

upvotes_num = upvotes_num[,c(1,7:11)]
names(upvotes_num) = c("ID","Repuations","Answers","Usernames","Views","Upvotes")

library(caret)
upvotes_nzv = (nearZeroVar(upvotes_num))
upvotes_nzv
merged_data = cbind(upvotes_num,upvotes_char)
names(merged_data)
merged_data = merged_data[,-1]
library(caTools)
set.seed(200)
sample = sample.split(merged_data,SplitRatio = 0.8)
train = subset(merged_data,sample == T)
test = subset(merged_data,sample == F)
names(train)
linear_reg = lm(Upvotes~.,data = train)
summary(linear_reg)
predict_test = predict(linear_reg,newdata = test)
summary(predict_test)
plot(linear_reg)
test$predicted = predict_test
test$predicted
test$error = test$Upvotes-test$predicted
head(upvotes1)
