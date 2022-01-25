#' Generate a time series graph for for government functions
#'
#' @param .data tibble
#' @return time series graph.
#' @examples
#' dataTimeSeries(sel_function= c("Saúde", "Educação")) %>% graphTimeSeries()
#' @export

graphTimeSeries <- function(.data){



  graph<-
    .data %>%
    dplyr::mutate(percentual =round((total/total_ano)*100,2)) %>%
    dplyr::rename( funcao = descricao_cofog) %>%
    dplyr::mutate(funcao = stringr::str_wrap(funcao,20) ) %>%
    ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(x= ano, y= percentual, group = funcao, color = funcao)) +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::scale_y_continuous(labels=function(x) format(x, big.mark = ".",decimal.mark = ",", scientific = FALSE)) +
    ggplot2::theme_light()+
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )+
    ggplot2::labs(color= "Função",
                  x="",
                  y="% do gasto total")


  plotly::ggplotly(graph, tooltip = c("x","y","colour"))

}

