#Pretest Alex:
pretestAlex <- read.delim("Pretest_results/old/pretestAlex")
pretestAlexa <- read.delim("Pretest_results/old/pretestAlexa")
pretest007 = rbind(pretestAlex, pretestAlexa)
#pretest007$Trial[1:88] = pretest007$Trial[1:88]-88
pretest007$Trial[89:176] = pretest007$Trial[89:176] + 88

pretest007 <- subset(pretest007, select = -X)

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
