adult <- read.table("A_MA710/DMPA_data_sets/Data sets/Adult2_Test", header=TRUE, sep="\t",stringsAsFactors = TRUE)
View(adult)
colnames(adult)
x <- adult[,c(1:9)]
y <- adult$Income
c50fit2 <- C5.0(x, y)
summary(c50fit2)
