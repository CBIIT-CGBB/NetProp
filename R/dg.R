
dg <- function(g, gene, degree=1){
  gene.in <- intersect(V(g)$name, gene);
  gene    <- gene.in;
  g.dat   <- as.data.frame(get.edgelist(g));
  
  gene.l <- NULL;
  for (i in 1:degree){
    g1.i  <- which(g.dat[,1] %in% gene);
    g2.i  <- which(g.dat[,2] %in% gene);
    g.i   <- unique(c(g1.i, g2.i));
    g.out <- unique(c(g.dat[g.i,1], g.dat[g.i,2]));
    g.f   <- setdiff(g.out, gene);
    gene.l[[i]] <- g.f;
    gene        <- g.out;
  }
  names(gene.l) <- paste0("degree", 1:degree);
  out <- list(gene.l=gene.l, gene.in=gene.in);
  return(out);
}