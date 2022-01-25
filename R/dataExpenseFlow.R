#' Get data about a set of government functions used as support for sankey graphs
#'
#' @param year number
#' @return data for sankey graph.
#' @examples
#' dataExpenseFlow(year=2020)
#' @export


dataExpenseFlow <- function(year=2020){

  Base_COFOG<-
    Base_COFOG %>%
    dplyr::filter(ano==year)




  dados_cofog_pai<-
    Base_COFOG %>%
    dplyr::mutate(codigo_cofog = stringr::str_sub(codigo_cofog,1,3)) %>%
    dplyr::select(-descricao_cofog) %>%
    dplyr::inner_join( COFOG_GC) %>%
    dplyr::mutate(descricao_cofog_pai = "Gastos com funções de governo") %>%
    dplyr::group_by(descricao_cofog_pai, descricao_cofog) %>%
    dplyr::summarise(
      total_gasto = sum(valor)
    )

  dados_total<-
    dados_cofog_pai %>%
    dplyr::ungroup() %>%
    dplyr::mutate(descricao_cofog = descricao_cofog_pai) %>%
    dplyr::mutate(descricao_cofog_pai = NA) %>%
    dplyr::group_by(descricao_cofog_pai, descricao_cofog) %>%
    dplyr::summarise(
      total_gasto = sum(total_gasto)
    )




  dados_cofog_raiz<-
    Base_COFOG %>%
    dplyr::mutate(codigo_pai = stringr::str_sub(codigo_cofog,1,3)) %>%
    dplyr::select(-descricao_cofog) %>%
    dplyr::inner_join( COFOG_GC) %>%
    dplyr::mutate(codigo_filho = codigo_cofog) %>%
    dplyr::mutate(descricao_cofog_filho = descricao_cofog) %>%
    dplyr::mutate(codigo_cofog = codigo_pai) %>%
    dplyr::select(codigo_pai,codigo_cofog, descricao_cofog_filho, valor) %>%
    dplyr::inner_join( COFOG_GC) %>%
    dplyr::mutate(descricao_cofog_pai = descricao_cofog ) %>%
    dplyr::mutate(descricao_cofog = descricao_cofog_filho) %>%
    dplyr::group_by(descricao_cofog_pai, descricao_cofog) %>%
    dplyr::summarise(
      total_gasto = sum(valor)
    )

  dados_cofog_completo <-
    dados_cofog_pai %>%
    dplyr::bind_rows(dados_cofog_raiz,
                     dados_total)


  dados_cofog_completo<-
    dados_cofog_completo%>%
    dplyr::ungroup() %>%
    dplyr::mutate(total_gasto = total_gasto / 10^3) %>%
    dplyr::mutate(descricao_cofog= reorder(descricao_cofog, total_gasto)) %>%
    dplyr::arrange(desc(descricao_cofog)) %>%
    dplyr::mutate (source = dplyr::row_number() -1)

  pai<-
    (dados_cofog_completo %>%
       dplyr::filter(!is.na(descricao_cofog_pai)) %>%
       dplyr::distinct(descricao_cofog_pai))$descricao_cofog_pai

  pos_pai <-
    dados_cofog_completo %>%
    dplyr::filter(descricao_cofog %in% pai) %>%
    dplyr::distinct(descricao_cofog, source) %>%
    dplyr::mutate(descricao_cofog_pai =descricao_cofog ) %>%
    dplyr::mutate(destination = source) %>%
    dplyr::select(descricao_cofog_pai, destination)

  dados_net<-
    dados_cofog_completo %>%
    dplyr::left_join(pos_pai)

  .data<-dados_net

  .data



}
