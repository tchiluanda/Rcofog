#' Get time series data for selected government functions
#'
#' @param sel_function charcter vector
#' @return tibble.
#' @examples
#' dataTimeSeries(sel_function= c("Saúde", "Educação"))
#' @export


dataTimeSeries<- function(sel_function= c("Saúde","Educação")){

  despesa_total_anual<-
    base_grafico %>%
    dplyr::filter(codigo_cofog_pai == 7) %>%
    dplyr::mutate(ano = as.character(ano)) %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(
      total_ano = sum(valor)
    )



  .data<-
    base_grafico %>%
    dplyr::filter(descricao_cofog %in% sel_function) %>%
    dplyr::mutate(ano = as.character(ano)) %>%
    dplyr::group_by(ano, descricao_cofog) %>%
    dplyr::summarise(
      total = sum(valor)
    ) %>%
    dplyr::inner_join(despesa_total_anual)

  .data


}
