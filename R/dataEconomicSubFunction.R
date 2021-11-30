#' Get data about a set of government sub-functions and economic classifications
#'
#' @param year number
#' @return tibble.
#' @examples
#' dataEconomicSubFunction(year=2020)
#' @export


dataEconomicSubFunction<- function(year=2020, sel_function= "Educação"){

  dados_economicos <- dataEconomicClassification(year)

  .data<-
    dados_economicos %>%
    filter(codigo_cofog_pai %in% (dados_economicos %>%
                                    filter(descricao_cofog == sel_function) %>%
                                    select(codigo_cofog))$codigo_cofog)

  .data


}
