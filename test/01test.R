rm(list=ls());

options(stringsAsFactors = F);
library(NetProp);

data(ppi);
data(genes);

## generate graph or network of PPI
g <- ppi2net(ppi[,c(3, 6)]);
## gene number
length(V(g)$name);
## link number
gsize(g)

## genes
gene <- unique(genes[,3])

## calculate diffusion score
out1 <- diffusion(g, gene, 0.005);
## do random walk with restart 
out2 <- rwr(g, gene, 0.0005);
## do the shortest path
out3 <- sp(g, gene)
## do steiner
out4 <- steiner_net(g=g, steiner.node=gene);
## do cluster
out5 <- ncluster(g, gene, cutoff=20);
