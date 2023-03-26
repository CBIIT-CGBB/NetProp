

diffusion <- function(g, gene, cutoff){
  g.i            <- which(V(g)$name %in% gene)
  input_vec      <- rep(0, length(V(g)$name));
  input_vec[g.i] <- 1;
  names(input_vec) <- V(g)$name;
  
  out <- diffuStats::diffuse(
    graph = g, 
    method = "raw", 
    scores = input_vec)
  d.i   <- which(out > cutoff);
  out.s <- data.frame(name=names(out[d.i]), value=out[d.i]);
  return(out.s);
}