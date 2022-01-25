get_top_n_funcao <- function(n){
  fun_sel<-
    base_grafico %>%
    dplyr::filter(codigo_cofog_pai == 7) %>%
    dplyr::group_by(descricao_cofog) %>%
    dplyr::summarise(
      total= sum(valor)
    ) %>%
    dplyr::slice_max(order_by = total,n=n) %>%
    dplyr::ungroup() %>%
    dplyr::select(descricao_cofog)

  fun_sel$descricao_cofog


}
