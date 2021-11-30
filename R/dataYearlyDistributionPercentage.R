#' Get time series data for top government functions
#'
#' @return tibble.
#' @examples
#' dataYearlyDistributionPercentage()
#' @export


dataYearlyDistributionPercentage <- function() {
  top_4<-
    base_grafico %>%
    dplyr::filter(descricao_cofog %in% get_top_n_funcao(4)) %>%
    dplyr::mutate(ano = as.character(ano)) %>%
    dplyr::group_by(ano, descricao_cofog) %>%
    dplyr::summarise(
      total = sum(valor)
    ) %>%
    dplyr::ungroup()




  demais <-
    base_grafico %>%
    dplyr::filter( codigo_cofog_pai == 7,
                   !descricao_cofog %in% get_top_n_funcao(4) ) %>%
    dplyr::mutate(ano = as.character(ano)) %>%
    dplyr::mutate(descricao_cofog = "Demais funções") %>%
    dplyr::group_by(ano, descricao_cofog) %>%
    dplyr::summarise(
      total = sum(valor)
    ) %>%
    dplyr::ungroup()

  .data<-
    top_4%>%
    dplyr::bind_rows(
      demais
    )

  .data

}
