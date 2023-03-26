

rwr <- function(g, gene, cutoff){
  g.i            <- which(V(g)$name %in% gene);
  input_vec      <- rep(0, length(V(g)$name));
  input_vec[g.i] <- 1;
  names(input_vec) <- V(g)$name;
  
  out <- dRWR(g=g, normalise="laplacian", setSeeds=input_vec,
              restart=0.75, parallel=FALSE)
  out <- as.matrix(out);
  row.names(out) <- V(g)$name;
  r.i   <- which(out > cutoff);
  out.s <- data.frame(name=row.names(out)[r.i], value=out[r.i,1]);
}