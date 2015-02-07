source("func.R")

# chi-square test
p1 <- prob_binommixture(p=sum(fs[,1])/sum(fs))
chisq.test(table(factor(fs[,1], levels=0:9)), p=p1/sum(p1))

# repeat with smaller range
maxn <- max(fs[,1])+1
p1 <- prob_binommixture(0:maxn, p=sum(fs[,1])/sum(fs))
chisq.test(table(factor(fs[,1], levels=0:maxn)), p=p1/sum(p1))

# repeat both with 2nd column (yellow)
p2 <- prob_binommixture(p=sum(fs[,2])/sum(fs))
chisq.test(table(factor(fs[,2], levels=0:9)), p=p2/sum(p2))

maxn <- max(fs[,2])+1
p2 <- prob_binommixture(0:maxn, p=sum(fs[,2])/sum(fs))
chisq.test(table(factor(fs[,2], levels=0:maxn)), p=p2/sum(p2))

# test using SD
z <- replicate(10000, sim_binommixture_sd(p=sum(fs[,1])/sum(fs)))
mean(z >= sd(fs[,1]))

z <- replicate(10000, sim_binommixture_sd(p=sum(fs[,2])/sum(fs)))
mean(z >= sd(fs[,2]))


# should really stratify by the number of snacks in the package?
# (look at n=12 and n=13 and then combine information)
