


library(tidyverse)
library(igraph)
library(visNetwork)



get_nodes_graph <- function(relations, processes) {

  nodes <- unique( c(as.character(relations$from), c(as.character(relations$to))) )

  input <- setdiff( relations$from, processes )
  output <- setdiff( relations$to, processes )
  input_output <- intersect( input, output )

  input <- setdiff( input, input_output )
  output <- setdiff( output, input_output )

  group <- seq_len(length(nodes))
  group[nodes %in% processes] <- "main"
  group[nodes %in% input] <- "input"
  group[nodes %in% output] <- "output"
  group[nodes %in% input_output] <- "input_output"

  nodes = data.frame(id = nodes, group = group)

  nodes
}





#' Returns graph of dependencies for the main processes of relationships
#'
#' @param relations data frame with dependencies (with fields from, to and time)
#' @param processes data frame with main processes involved (with parameters from, to and time)
#' @param view_time Boolean : True (by default), if time as labels of edges shown, False otherwise
#' @return Returns graph of dependencies for the main processes of relationships
#'
get_dependencies_graph <- function(relations, processes, view_time = T){

  processes <- processes[["Processes"]]
  nodes <- get_nodes_graph(relations, processes)

  relations <- relations %>% rename( "label" = "time" )
  if( !view_time ){
    relations <- relations %>%
      select(-"label")
  }

  nodes_filtered <- nodes %>%
    filter(group %in% c("main", "input_output"))

  nodes_removed <- as.character(
    (nodes %>%
       filter(!(group %in% c("main", "input_output"))))[["id"]]
  )

  relations_filtered <- relations[apply(relations, 1, function(x) any(x %in% nodes_removed))==F,]

  network <- visNetwork(nodes = nodes_filtered, relations_filtered, width = "100%") %>%
    visEdges(arrows = "to") %>%
    visGroups(groupname = "main", color = "red")  %>%
    visHierarchicalLayout(sortMethod = "directed") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group")

  network

}
