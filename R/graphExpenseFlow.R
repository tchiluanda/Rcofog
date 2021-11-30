#' Generate a sankey graph for a given dataset
#'
#' @param .data tibble
#' @return box plot graph.
#' @examples
#' dataExpenseFlow(year=2020) %>% graphExpenseFlow()
#' @export

graphExpenseFlow <- function(.data){

  nodes<-
    .data %>%
    dplyr::select(descricao_cofog, descricao_cofog_pai)



  links<-
    .data %>%
    dplyr::filter(!is.na(descricao_cofog_pai)) %>%
    dplyr::select(source,
                  destination,
                  total_gasto)


  networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                           Target = "destination", Value = "total_gasto", NodeID = "descricao_cofog",
                           units = "", fontSize = 8, nodeWidth = 30, nodePadding =6, NodeGroup = "descricao_cofog_pai")


}
