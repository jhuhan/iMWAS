source("MWAS.R")

args = commandArgs(trailingOnly=TRUE)

#gwas z-score file
#z-score can be calculated from beta/se
#z-score can also be converted from gwas p-value:
#z[OR < 1] <- qnorm(p[OR < 1])
#z[OR >= 1] <- abs(qnorm(p[OR >= 1]))
gwasfile=args[1]

#weight file
#obtained from elastic net training model:beta~w1*snps1+w2*snps+...
weightfile=args[2]

#genotype reference file coded as 0 1 2
genofile=args[3]



#output file
outfile=args[4]

#gwasfile="cg13896476.gwas"
#weightfile="cg13896476.model"
#genofile="cg13896476.geno"
#outfile="cg13896476.res";

gwas<-read.table(gwasfile,head = T)
weight<-read.table(weightfile,head = T)
geno<-read.table(genofile,head = T)
genoref <- geno[,-c(1:6)]
res <- MWAS(gwas,weight,genoref)
write.table(res, outfile, col.names=T, row.names=F, sep="\t",quote=F)
