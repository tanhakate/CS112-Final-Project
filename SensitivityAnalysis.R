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

match.pctVV <- Match(Y= match.data$pctVV, Tr = match.data$treat, X = genmatch.covar, Weight.matrix=genmatch.output, BiasAdjust=FALSE, caliper = c(rep(10,43),.5), exact = FALSE)


bal.data <- with(match.data, data.frame(match.party, male, occ, edu, yob, lat, long, ran.prior, incumbent, log.valid.votes, prior.pctVV, party.prior.pctVV, elec.year, log.total.assets, gini_2000, pt_pres_1998, income_2000, psdb_2000, pt_2000, hdi_2000, uf.ba, uf.sp, uf.mg, uf.rs, log.num.apps))
bal.fmla <- as.formula(paste("treat~",paste(names(bal.data),collapse="+")))
match.bal <- MatchBalance(bal.fmla, data = data.frame(treat = match.data$treat, bal.data), match.out = match.pctVV,nboots=3000)
before.sd.diff <- sapply(match.bal$BeforeMatching, function(x) x[[1]])
after.sd.diff <- sapply(match.bal$AfterMatching, function(x) x[[1]])
before.t.p <- matrix(sapply(match.bal$BeforeMatching, function(x) x$tt[3]))
after.t.p <- matrix(sapply(match.bal$AfterMatching, function(x) x$tt[3]))
before.ks.p <- matrix(sapply(match.bal$BeforeMatching, function(x) x$ks$ks.boot.pvalue))
after.ks.p <- matrix(sapply(match.bal$AfterMatching, function(x) x$ks$ks.boot.pvalue))
bal.stats <- data.frame(var = names(data.frame(model.matrix(as.formula(paste("~",paste(names(bal.data),collapse="+"))), bal.data)[,-1])), before.sd.diff, after.sd.diff, before.t.p = unlist(before.t.p), after.t.p = unlist(after.t.p), before.ks.p, after.ks.p)
bal.stats <- bal.stats[c(4,6,12,15,18:27,29:49),]
rownames(bal.stats)  <- c("Party: PFL", "Party: PMDB", "Party: PSDB", "Party: PT", "Male", "Occupation: Blue Collar", "Occupation: Education", "Occupation: Government", "Occupation: Media", "Occupation: None", "Occupation: Other", "Occupation: Politician", "Occupation: White Collar", "Education: Some Superior or More", "Year of Birth", "Latitude", "Longitude", "Ran Previously", "Incumbency", "Log Electorate", "Prior Vote Share", "Party Prior Vote Share", "Election Year", "Total Assets", "2000 Gini", "PT Pres Vote Share (1998)", "GDP Per Capita (2000)", "PSDB Mayor Vote Share (2000)", "PT Mayor Vote Share (2000)", "HDI (2000)", "State: Bahia", "State: Sao Paulo", "State: Minas Gerais", "State: Rio Grande do Sul", "Log Number of Applications")
bal.stats$before.ks.p[bal.stats$before.ks.p=="NULL"] <- NA
bal.stats$after.ks.p[bal.stats$after.ks.p=="NULL"] <- NA
bal.stats$before.ks.p <- unlist(bal.stats$before.ks.p)
bal.stats$after.ks.p <- unlist(bal.stats$after.ks.p)
bal.stats <- bal.stats[order(abs(bal.stats$before.sd.diff), decreasing = TRUE),]
bal.stats <- bal.stats[,-1]


#Visualizing balance for election year
plot(density(match.data$elec.year[match.data$treat==0]), lwd = 3, col = "red", main = "Balance: Prior pctVV before Matching")
lines(density(match.data$elec.year[match.data$treat==1]))


plot(density(match.data$elec.year[match.pctVV$index.control]), lwd = 3, col = "red", main = "Balance: Prior pctVV after Matching")
lines(density(match.data$elec.year[match.pctVV$index.treated]))

#Visualizing balance for prior pctVV
plot(density(match.data$prior.pctVV[match.data$treat==0]), lwd = 3, col = "red", main = "Balance: Prior pctVV before Matching")
lines(density(match.data$prior.pctVV[match.data$treat==1]))


plot(density(match.data$prior.pctVV[match.pctVV$index.control]), lwd = 3, col = "red", main = "Balance: Prior pctVV after Matching")
lines(density(match.data$prior.pctVV[match.pctVV$index.treated]))

#Sensitivity Analysis
library('rbounds')
psens(match.pctVV,Gamma = 2,GammaInc = 0.05)


