#' Generate a ranking graph for economic classification for government sub-function
#'
#' @param .data tibble
#' @param free_scale boolean. Indicates if the graph will be presented in free scale
#' @return ranking graph.
#' @examples
#' dataEconomicSubFunction(year=2020) %>% graphEconomicSubFunction()
#' @export

graphEconomicSubFunction<-  function(.data,free_scale=TRUE){

  graph<-
    .data %>%
    dplyr::mutate(descricao_cofog = reorder(descricao_cofog, valor),
                  funcao_economica = reorder(funcao_economica, desc(valor))) %>%
    ggplot2::ggplot(ggplot2::aes(x=factor(descricao_cofog),valor, fill= funcao_economica))+
    ggplot2::geom_col(color = "white")+
    viridis::scale_fill_viridis(discrete=TRUE) +
    ggplot2::scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
    ggplot2::theme_light()+
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90),
      legend.position = "",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(color = "black")
    )+
    ggplot2::labs(color="",
                  x="",
                  y="Valor gasto no ano (R$ mi)")+

    ggplot2::coord_flip()

  if (free_scale){
    graph+
      ggplot2::facet_wrap(funcao_economica~., scales = "free_x")
  } else{
    graph+
      ggplot2::facet_wrap(funcao_economica~.)
  }



}

