library(ggplot2)
library(corrplot)
library(data.table)

ggplot(data=housing_con, aes(housing_con$SalePrice))+
  geom_histogram()+
  xlab("SalePrice")

ggplot(data=housing_con, aes(log(housing_con$SalePrice)))+
  geom_histogram()+
  xlab("log scaled SalePrice")

num_col <- housing[, sapply(housing, class) == "numeric"]
corrplot(num_col)

