## Assignment One 
dat <- load('~/Desktop/bddata.Rdata')
dat 
M <- rnow(dat)
indices <- 1:M 
counts <- dat$COUNT
rowindices <- rep(indices, counts)
nltcsmat <- dat[rowindices,]
nltcsmat
nltcsmat <- nltcsmat[,-(17:18)]
nltcsmat <- nltcsmat[,16:1]
nltcsmat <- as.matrix(nltcsmat)
nlts <- as(nltcsmat,'transactions')
apply(nltcsmat,2,table)
#      Y1    Y2    Y3    Y4    Y5    Y6    Y7    Y8    Y9   Y10   Y11   Y12   Y13   Y14   Y15   Y16
# 0 19289 15627 12877 17091 12108 16227  6997 16903 13928 15984 11097  9609 10936 16625 17022 18430
# 1  2285  5947  8697  4483  9466  5347 14577  4671  7646  5590 10477 11965 10638  4949  4552  3144
hist (disabilitycount)

rules <- apriori(nltcs, parameter=list(support=0.1, confidence=0.5))
rules 
# set of 21215 rules 
rules <- apriori(nltcs, parameter=list(support=0.3, confidence=0.5))
rules
# set of 58 rules
inspect(head(sort(rules, by ="lift"),6))
inspect(head(sort(rules, by ="lift"),6))
#    lhs             rhs   support   confidence lift    
# 33 {Y7,Y11}     => {Y9}  0.3054139 0.6858541  1.935210
# 32 {Y7,Y9}      => {Y11} 0.3054139 0.8833624  1.819000
# 4  {Y11}        => {Y9}  0.3114397 0.6413095  1.809523
# 3  {Y9}         => {Y11} 0.3114397 0.8787601  1.809523
# 57 {Y7,Y12,Y13} => {Y11} 0.3103272 0.8700455  1.791578
# 36 {Y7,Y12}     => {Y3}  0.3376750 0.7116343  1.765298

plot(rules, measure=c("support", "lift"), shading="confidence")
plot(rules, shading="order", control=list(main = "Two-key plot"))
subrules <- rules[quality(rules)$support > 0.3]
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
# Itemsets in Antecedent (LHS)
# [1] "{Y11,Y12}"     "{Y7,Y11,Y12}"  "{Y7,Y11,Y13}"  "{Y3,Y7}"       "{Y3}"          "{Y11,Y13}"    
# [7] "{}"            "{Y3,Y12}"      "{Y5,Y12}"      "{Y11,Y12,Y13}" "{Y5,Y11}"      "{Y9,Y11}"     
# [13] "{Y9}"          "{Y12,Y13}"     "{Y7,Y9}"       "{Y7,Y12,Y13}"  "{Y7,Y13}"      "{Y5,Y7}"      
# [19] "{Y5}"          "{Y13}"         "{Y12}"         "{Y7,Y12}"      "{Y7}"          "{Y7,Y11}"     
# [25] "{Y11}"        
# Itemsets in Consequent (RHS)
# [1] "{Y7}"  "{Y13}" "{Y3}"  "{Y5}"  "{Y9}"  "{Y12}" "{Y11}"

plot(rules, method="grouped")
subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")

