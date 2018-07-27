#Pretest 001:
pretest001 <- read.delim("Pretest_results/old/pretest001.txt")
pretest001 <- subset(pretest001, select = -X)
#Pretest 002:
pretest002 <- read.delim("Pretest_results/old/pretest002.txt")
pretest002b <- read.delim("Pretest_results/old/pretest002b.txt")
pretest002 <- pretest002[1:88,1:3]
pretest002 = rbind(pretest002, pretest002b[1:3])
pretest002 <- subset(pretest002, select = -X)
#Pretest 003:
pretest003 <- read.delim("Pretest_results/old/Piloting_003")
pretest003b <- read.delim("Pretest_results/old/pilot_003b")
pretest003 = rbind(pretest003, pretest003b)
pretest003 <- subset(pretest003, select = -X)
#Pretest 004:
pretest004 <- read.delim("Pretest_results/old/pilot_004")
pretest004 <- subset(pretest004, select = -X)
#Pretest 005:
pretest005 <- read.delim("Pretest_results/old/pretest_005")
pretest005 <- subset(pretest005, select = -X)
#Pretest 006:
pretest006 <- read.delim("Pretest_results/old/pilot006")
pretest006 <- subset(pretest006, select = -X)

#Pretest Alex:
pretestAlex <- read.delim("Pretest_results/old/pretestAlex")
pretestAlexa <- read.delim("Pretest_results/old/pretestAlexa")
pretest007 = rbind(pretestAlex, pretestAlexa)
pretest007$Trial = 1:length(pretest007$Trial)

pretest007 <- subset(pretest007, select = -X)

#unify columnn names:

colnames(pretest001)[colnames(pretest001)=="Response"] <- "Answer"
colnames(pretest002)[colnames(pretest002)=="Response"] <- "Answer"
colnames(pretest003)[colnames(pretest003)=="Response"] <- "Answer"
colnames(pretest004)[colnames(pretest004)=="Response"] <- "Answer"
colnames(pretest005)[colnames(pretest005)=="Response"] <- "Answer"
colnames(pretest006)[colnames(pretest006)=="Response"] <- "Answer"
colnames(pretest007)[colnames(pretest007)=="Response"] <- "Answer"

write.csv(pretest007, file = "pretest007.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest006, file = "pretest006.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest005, file = "pretest005.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest004, file = "pretest004.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest003, file = "pretest003.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest002, file = "pretest002.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest001, file = 'pretest001.csv', quote = FALSE, row.names = FALSE)


#Pretest 001:
pretest001 <- read.csv("Pretest_results/pretest001.csv")
pretest001$Run <- 1*(pretest001$Trial < 45)
pretest001$Run[45:88] = 2
pretest001$Run[89:132] = 3
pretest001$Run[133:176] = 4
#pretest001_formatted = reshape(pretest001[2:4], idvar = "Identity", timevar = "Run", direction = "wide")



#Pretest 002:
pretest002 <- read.csv("Pretest_results/pretest002.csv")
pretest002$Run <- 1*(pretest002$Trial < 45)
pretest002$Run[45:88] = 2
pretest002$Run[89:132] = 3
pretest002$Run[133:176] = 4
#pretest002_formatted = reshape(pretest002[2:4],idvar = "Identity", timevar = "Run", direction = "wide")


#Pretest 003:
pretest003 <- read.csv("Pretest_results/pretest003.csv")

#pretest003 = pretest003[1:3]
pretest003$Run <- 1*(pretest003$Trial < 45)
pretest003$Run[45:88] = 2
pretest003$Run[89:132] = 3
pretest003$Run[133:176] = 4

#pretest003_formatted = reshape(pretest003[2:4], idvar = "Identity", timevar = "Run", direction = "wide")

#Pretest 004:
pretest004 <- read.csv("Pretest_results/pretest004.csv")
#pretest004 = pretest004[1:3]
pretest004$Run <- 1*(pretest004$Trial < 45)
pretest004$Run[45:88] = 2
pretest004$Run[89:132] = 3
pretest004$Run[133:176] = 4
#pretest004_formatted = reshape(pretest004[2:4], idvar = "Identity", timevar = "Run", direction = "wide")


#Pretest 005:
pretest005 <- read.csv("Pretest_results/pretest005.csv")
#pretest005 = pretest005[1:3]
pretest005$Run <- 1*(pretest005$Trial < 45)
pretest005$Run[45:88] = 2
pretest005$Run[89:132] = 3
pretest005$Run[133:176] = 4
#pretest005_formatted = reshape(pretest005[2:4], idvar = "Identity", timevar = "Run", direction = "wide")

#Pretest 006:
pretest006 <- read.csv("Pretest_results/pretest006.csv")
#pretest006 = pretest006[1:3]
pretest006$Run <- 1*(pretest006$Trial < 45)
pretest006$Run[45:88] = 2
pretest006$Run[89:132] = 3
pretest006$Run[133:176] = 4
#pretest006_formatted = reshape(pretest006[2:4], idvar = "Identity", timevar = "Run", direction = "wide")

#Pretest 007:
pretest007 <- read.csv("Pretest_results/pretest007.csv")

#pretest007 = pretest007[1:3]
pretest007$Run <- 1*(pretest007$Trial < 45)
pretest007$Run[45:88] = 2
pretest007$Run[89:132] = 3
pretest007$Run[133:176] = 4
colnames(pretest007)[colnames(pretest007)=="Response"] <- "Answer"
#pretest007_formatted = reshape(pretest007[2:4], idvar = "Identity", timevar = "Run", direction = "wide")

pretest008 <- read.csv("Pretest_results/pretest008.csv")
pretest008$Run <- 1*(pretest008$Trial < 45)
pretest008$Run[45:88] = 2
pretest008$Run[89:132] = 3
pretest008$Run[133:176] = 4
#pretest008_formatted = reshape(pretest008[2:5], idvar = "Identity", timevar = "Run", direction = "wide")

#Calculate getting means
pretest001_formatted$MeanAnswer <- rowMeans(pretest001_formatted[2:5])
pretest002_formatted$MeanAnswer <- rowMeans(pretest002_formatted[2:5])
pretest003_formatted$MeanAnswer <- rowMeans(pretest003_formatted[2:5])
pretest004_formatted$MeanAnswer <- rowMeans(pretest004_formatted[2:5])
pretest005_formatted$MeanAnswer <- rowMeans(pretest005_formatted[2:5])
pretest006_formatted$MeanAnswer <- rowMeans(pretest006_formatted[2:5])
pretest007_formatted$MeanAnswer <- rowMeans(pretest007_formatted[2:5])

write.csv(pretest007, file = "pretest007.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest006, file = "pretest006.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest005, file = "pretest005.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest004, file = "pretest004.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest003, file = "pretest003.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest002, file = "pretest002.csv", quote = FALSE, row.names = FALSE)
write.csv(pretest001, file = 'pretest001.csv', quote = FALSE, row.names = FALSE)


