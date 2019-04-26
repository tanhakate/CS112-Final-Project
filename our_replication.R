#Replication
library("Matching")

#Recoding categorical variables into numerical format


match.data$occ.media <- ifelse(match.data$occ=="Media",1,0)
match.data$occ.bluecollar <- ifelse(match.data$occ=="Blue collar",1,0)
match.data$occ.whitecollar <- ifelse(match.data$occ=="White collar",1,0)
match.data$occ.none <- ifelse(match.data$occ=="None",1,0)
match.data$occ.politician <- ifelse(match.data$occ=="Politician",1,0)
match.data$occ.government <- ifelse(match.data$occ=="Government",1,0)
match.data$occ.other <- ifelse(match.data$occ=="Other",1,0)
match.data$occ.education <- ifelse(match.data$occ=="Education",1,0)
match.data$edu.some.superior <- ifelse(match.data$edu == "Some Superior or More", 1, 0)
match.data$party.pt <- ifelse(match.data$party=="PT", 1, 0)
match.data$party.psdb <- ifelse(match.data$party=="PSDB", 1, 0)
match.data$party.pmdb <- ifelse(match.data$party=="PMDB", 1, 0)
match.data$party.pfl <- ifelse(match.data$party=="PFL", 1, 0)
match.data$total.assets <- exp(match.data$log.total.assets)-1 ## what's going on here?

#exclude missing values
match.data_new <- na.omit(match.data) 

#Replication
Tr <- match.data_new$treat #original guys made a mistake by including these in the matching results
Y1 <- match.data_new$pctVV
X <- subset(match.data_new,select = c(male, elected, occ.bluecollar, occ.education, occ.government, occ.media, occ.none, occ.other, occ.politician, occ.whitecollar, lat, long, ran.prior, incumbent, log.valid.votes, party.prior.pctVV, prior.pctVV, elec.year, party.pt, party.psdb, party.pmdb, party.pfl, uf.rs, uf.sp,yob,edu.scale, log.total.assets, pt_pres_1998, psdb_2000,hdi_2000, income_2000,log.num.apps, gini_2000)) #creating X vector excluding the treatment assignment and outcome variable and descriptive variables already recorded above

genout <- GenMatch(Tr = Tr, X = X, pop.size = 200, max.generations = 10, wait.generations = 2, replace = FALSE)

match.pctVV_rep<- Match(Y= Y1, Tr = Tr, X = X, Weight.matrix=genout, M=1, BiasAdjust=FALSE, exact = FALSE)


bal.data <- with(match.data, data.frame(male, elected, occ.bluecollar, occ.education, occ.government, occ.media, occ.none, occ.other, occ.politician, occ.whitecollar, lat, long, ran.prior, incumbent, log.valid.votes, party.prior.pctVV, prior.pctVV, elec.year, party.pt, party.psdb, party.pmdb, party.pfl, uf.rs, uf.sp,yob,edu.scale, log.total.assets, pt_pres_1998, psdb_2000,hdi_2000, income_2000,log.num.apps, gini_2000))
bal.fmla <- as.formula(paste("treat~",paste(names(bal.data),collapse="+")))
match.bal_rep <- MatchBalance(bal.fmla, data = data.frame(treat = match.data$treat, bal.data), match.out = match.pctVV_rep,nboots=3000)

library('rbounds')
psens(match.pctVV_rep,Gamma = 2,GammaInc = 0.05)


#Table Code

before.sd.diff <- sapply(match.bal_rep$BeforeMatching, function(x) x[[1]])
after.sd.diff <- sapply(match.bal_rep$AfterMatching, function(x) x[[1]])
before.t.p <- matrix(sapply(match.bal_rep$BeforeMatching, function(x) x$tt[3]))
after.t.p <- matrix(sapply(match.bal_rep$AfterMatching, function(x) x$tt[3]))
before.ks.p <- matrix(sapply(match.bal_rep$BeforeMatching, function(x) x$ks$ks.boot.pvalue))
after.ks.p <- matrix(sapply(match.bal_rep$AfterMatching, function(x) x$ks$ks.boot.pvalue))
bal.stats <- data.frame(var = names(data.frame(model.matrix(as.formula(paste("~",paste(names(bal.data),collapse="+"))), bal.data)[,-1])), before.sd.diff, after.sd.diff, before.t.p = unlist(before.t.p), after.t.p = unlist(after.t.p), before.ks.p, after.ks.p)
bal.stats$before.ks.p[bal.stats$before.ks.p=="NULL"] <- NA
bal.stats$after.ks.p[bal.stats$after.ks.p=="NULL"] <- NA
bal.stats$before.ks.p <- unlist(bal.stats$before.ks.p)
bal.stats$after.ks.p <- unlist(bal.stats$after.ks.p)
bal.stats <- bal.stats[order(abs(bal.stats$before.sd.diff), decreasing = TRUE),]
bal.stats <- bal.stats[,-1]



#Visualizing balance for election year
plot(density(match.data_new$elec.year[match.data_new$treat==0]), lwd = 3, col = "red", main = "Balance: Election Year before Matching")
lines(density(match.data_new$elec.year[match.data_new$treat==1]))


plot(density(match.data_new$elec.year[match.pctVV_rep$index.control]), lwd = 3, col = "red", main = "Balance: Election Year after Matching")
lines(density(match.data_new$elec.year[match.pctVV_rep$index.treated]))

#Visualizing balance for prior pctVV
plot(density(match.data_new$prior.pctVV[match.data_new$treat==0]), lwd = 3, col = "red", main = "Balance: Prior pctVV before Matching")
lines(density(match.data_new$prior.pctVV[match.data_new$treat==1]))


plot(density(match.data_new$prior.pctVV[match.pctVV_rep$index.control]), lwd = 3, col = "red", main = "Balance: Prior pctVV after Matching")
lines(density(match.data_new$prior.pctVV[match.pctVV_rep$index.treated]))