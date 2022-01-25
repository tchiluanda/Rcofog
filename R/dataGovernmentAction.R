dataGovernmentAction <- function(.data,action=NULL, cluster=NULL){

  if(is.null(cluster)){
    filter_cluster<-c(1,2,3)
  } else{
    filter_cluster<-cluster
  }



  .data<-
    .data %>%
    dplyr::filter(cluster %in% filter_cluster)

  if (!is.null(action)){
    dados_filtrados<-
      .data %>%
      dplyr::filter(info_selecao %in% action,
                    cluster %in% filter_cluster)

    .data<-
      .data %>%
      dplyr::anti_join(dados_filtrados)



  }


  .data
}
