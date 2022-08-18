#
## Function to convert g object ('igraph') into community files ('cheddar')
#


#' Convert igraph object to cheddar object
#'
#' @param g igraph object to convert
#'
#' @return Directory 'Community' with the 3 needed .csv (comma-separated) files 
#'         to run cheddar functions
#' @export
#'
#' @examples

igraph_to_cheddar <- function(g) {
  
  # create a folder "Community" in working directory to store needed files
  dir.create("Community")
  # obtain trophic links file
  el <- as_edgelist(g)
  readr::write_delim(data.frame(resource = el[,1], consumer = el[,2]), file = "Community/trophic.links.csv", delim = ",")
  # obtain nodes file
  n <- vertex_attr(g, "name")
  readr::write_delim(data.frame(node = n), file = "Community/nodes.csv", delim = ",")
  # create 'food web name' file
  p <- data.frame(title = "Food Web name")
  readr::write_delim(p, file = "Community/properties.csv", delim = ",")
  # load and create Community object
  cc <<- LoadCommunity("Community")

}

