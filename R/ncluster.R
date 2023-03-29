
## 
divide_large_graph <- function(graph, max_nodes=10) {
  # Apply the Louvain method for community detection
  clusters <- cluster_louvain(graph)
  
  # Initialize an empty list to store the small subgraphs
  small_subgraphs <- list()
  
  # Iterate through each subgraph in the clusters
  for (i in 1:length(clusters)) {
    subgraph <- induced_subgraph(graph, clusters[[i]])
    
    # Check if the subgraph has fewer or equal to max_nodes
    if (vcount(subgraph) <= max_nodes) {
      small_subgraphs[[length(small_subgraphs) + 1]] <- subgraph
    } else {
      # If the subgraph is still too large, recursively divide it further
      small_subgraphs <- c(small_subgraphs, divide_large_graph(subgraph, max_nodes))
    }
  }
  
  return(small_subgraphs)
}

##
find_graph_with_genes <- function(gene_set, graph_list) {
  g.i  <- 0;
  gout <- NULL;
  for (i in 1:length(graph_list)) {
    graph <- graph_list[[i]]
    gene_names <- V(graph)$name
    
    # Check if all genes in the gene set are present in the current graph
    g.j <- which(gene_names %in% gene_set);
    if (length(g.j) > 0){
      g.i <- g.i + 1;
      gout[[g.i]] <- graph;
    }
  }
  # If no graph contains all genes in the gene set, return NULL
  if (is.null(gout)){
    return(NULL)
  } else {
    return(gout);
  }
}

##
ncluster <- function(g, gene, cutoff){
  gout <- divide_large_graph(g, cutoff);
  out  <- find_graph_with_genes(gene, gout);
  out.s <- NULL;
  for (i in 1:length(out)){
    gen   <- V(out[[i]])$name;
    out.s <- c(out.s, gen);
  }
  out.s <- unique(out.s);
  return(out.s);
}


