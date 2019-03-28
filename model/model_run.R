source("model.R")

args = commandArgs(trailingOnly=TRUE)
genotypefile=args[1]
phenotypefile=args[2]
outfile=args[3]

####training dataset
SNPdata<-read.table(genotypefile,head = T)
pheno<-read.table(phenotypefile,head = F)
trainX<-SNPdata[,-c(1:6)]
trainY<-pheno[,3]

####learn elastic net model from training data
result<-model(trainX,trainY)

write.table(result, outfile,col.names=F, row.names=F, quote=F,sep="\t") 

