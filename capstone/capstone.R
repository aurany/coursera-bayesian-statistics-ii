install.packages("kableExtra")

dat = read.csv(file="UCI_Credit_Card.csv", header=TRUE)
cols = read.table(file="columns.txt", header = TRUE, sep = ":")

head(dat)
head(cols)
