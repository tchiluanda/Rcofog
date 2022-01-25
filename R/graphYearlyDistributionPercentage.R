#' Generate a time series graph for top government functions
#'
#' @param .data tibble
#' @return time series graph.
#' @examples
#' dataYearlyDistributionPercentage() %>% graphYearlyDistributionPercentage()
#' @export

graphYearlyDistributionPercentage <- function(.data) {

  .data%>%
    dplyr::mutate(descricao_cofog = reorder(descricao_cofog, total)) %>%
    ggplot2::ggplot()+
    ggplot2::geom_col(ggplot2::aes(x=ano, y= total, fill= descricao_cofog), position = "fill") +
    viridis::scale_fill_viridis(discrete = TRUE)+
    ggplot2::theme_light()+
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )+
    ggplot2::labs(fill="Função",
                  x="",
                  y="% do gasto total")

}

