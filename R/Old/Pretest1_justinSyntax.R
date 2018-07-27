pretest001 <- read.delim("~/Desktop/Matlab_Sandbox/Ito adjustment/pretest001.txt", row.names=1)

library(tidyverse)
library(ggformula)
pretest001 %>% group_by(Identity) %>% summarise(mu = mean(Response), sd = sqrt(var(Response))) %>% gf_point(mu~sd)
