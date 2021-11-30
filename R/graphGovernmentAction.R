#' Generate a distribution graph for a given dataset
#'
#' @param .data tibble
#' @param cluster number that identifies the clusters to be shown at the graph
#' @param action vector that identifies the government action to be highlighted
#' @return jitter graph.
#' @examples
#' defineActionCluster(year=2020) %>% graphGovernmentAction()
#' @export

graphGovernmentAction <- function(.data, cluster= NULL, action="Juros"){

  if(is.null(cluster)){
    filter_cluster<-c(1,2,3)
  } else{
    filter_cluster<-cluster
  }


  pos<- stringr::str_locate_all(string=action, pattern = "\\|")

  if (length(pos[[1]])>0){
    action<- substr(action,1,pos[[1]][1,1]-1)

  }


  dados_filtrados<-
    .data %>%
    dplyr::filter(acao_governo %in% action,
                  cluster %in% filter_cluster)

  print("passou pela definição dos dados")
  print(.data)


  dados_alerta<-
    .data %>%
    dplyr::filter(cluster==3) %>%
    dplyr::group_by(cluster)%>%
    dplyr::summarise(
      min_total = min(total)/10^3
    )

  graph<-

    .data %>%
    filter(cluster %in% filter_cluster) %>%
    anti_join(dados_filtrados) %>%
    ggplot2::ggplot(ggplot2::aes(x=stringr::str_wrap(cofog_path,15), y= total/10^3))+
    ggplot2::geom_jitter(ggplot2::aes(fill= cofog_path), show.legend = FALSE, size=4, height = 0, width = ifelse(.data$cluster==1,0.4,0.1), alpha = 0.3, pch=21, color="white")+
    ggplot2::geom_jitter( data= dados_filtrados,  size = 4, show.legend = FALSE, height = 0, width = ifelse(dados_filtrados$cluster==1,0.2,0.1),alpha = 0.5, fill = "black", pch=21, color="white") +
    ggrepel::geom_text_repel(data= dados_filtrados,
                             ggplot2::aes(label = stringr::str_wrap(paste(stringr::str_to_lower(acao_governo) ,
                                                                          as.character(round(total/10^3,2)), sep=": "),20) ),
                             show.legend = FALSE,
                             box.padding = ggplot2::unit(1, "lines"),
                             color = "black",
                             alpha =0.8,
                             size =3,
                             set.seed = 1972,
                             segment.linetype = 5,
                             segment.alpha = 0.5) +

    ggplot2::scale_y_continuous(labels=function(x) format(x,decimal.mark=",", big.mark = ".", scientific = FALSE)) +
    colorspace::scale_color_discrete_qualitative(palette = "Dark 3")+
    ggplot2::theme_light()+
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text()

    )+
    ggplot2::labs(fill="",
                  x="",
                  y="Valor gasto no ano (R$ bi)") +
    ggplot2::facet_grid(cluster~., scales = "free_y" ,  space = "free_y")



  if (is.null(cluster)){
    graph<-
      graph+
      ggplot2::geom_text(data = dados_alerta, ggplot2::aes(x=1.2, y= min_total,  label="<---Mudança de escala"), color = "red")
  }

  graph
}
