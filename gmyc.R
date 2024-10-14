#install.packages("splits", repos="http://R-Forge.R-project.org")
library(splits)
tr <- read.tree("tree.nwk") 
#OBS: N?O PODE ENRAIZAR A ARVORE. TEM QUE SER DO JEITO QUE SAIU DO TREEANOTATOR E DEPOIS EXPORTAR PARA NWK
result <- gmyc(tr)
summary(result)
plot(result)
spec.list(result)->result
write.matrix(result, "GMYCresult.txt")
