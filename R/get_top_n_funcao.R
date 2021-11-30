get_top_n_funcao <- function(n){
  fun_sel<-
    base_grafico %>%
    filter(codigo_cofog_pai == 7) %>%
    group_by(descricao_cofog) %>%
    summarise(
      total= sum(valor)
    ) %>%
    slice_max(order_by = total,n=n) %>%
    ungroup() %>%
    select(descricao_cofog)

  fun_sel$descricao_cofog


}
