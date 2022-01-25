#' Define clusters for govenrment actions based on total expenses
#'
#' @param year number
#' @return tibble with clusters.
#' @examples
#' defineActionCluster(year=2020)
#' @export

defineActionCluster <- function(year=2020) {

  .data <- get_dados_cofog_completo(year)


  df_cluster<-
    .data %>%
    dplyr::group_by(acao_governo, cofog_path, unidade_orcamentaria_descricao) %>%
    dplyr::summarise(
      total = sum(valor)
    ) %>%
    dplyr::filter(total>0) %>%
    dplyr::arrange(dplyr::desc(total))

  info<-sessioninfo::session_info()

  if (info$platform$version != "R version 3.5.3 (2019-03-11)"){
    eval(parse(text='RNGkind(sample.kind = "Rounding")'))
  }


  print("df_cluster")
  print(df_cluster)
  set.seed(1972)
  #model<- pam(x = df_cluster[,4], k = 3)

  model<- kmeans(x= df_cluster[,4], centers = 3)

  .data<-
    df_cluster %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cluster = model$cluster,
                  cluster = reorder(cluster, dplyr::desc(cluster)),
                  info_selecao = paste(acao_governo,cofog_path,cluster,sep="|"))
  print("passou pela definição de cluster")
  print(.data)
  .data


}
