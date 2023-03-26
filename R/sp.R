
sp <- function(g, gene){
  g.i   <- which(V(g)$name %in% gene); 
  out.s <- NULL;
  for (i in 1:length(g.i)){
    out  <- igraph::all_shortest_paths(g, from=g.i[i], to=g.i, mode="all");
    out.s <- c(out.s, unlist(out$res))
  }
  out.s <- unique(out.s);
  return(V(g)$name[out.s])
}