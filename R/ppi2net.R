

ppi2net <- function(ppi){
  g <- igraph::graph.data.frame(ppi, directed=F);
  return(g);
}