get_dados_cofog_completo<- function(a_ano, a_cofog_path_completo =FALSE){
  names(Base_COFOG)[2]<-"ajustes"

  base_trabalho<-
    Base_COFOG %>%
    dplyr::filter(ano==a_ano) %>%
    dplyr::mutate(codigo_cofog_pai = stringr::str_sub(codigo_cofog,1,3),
           acao_governo = ifelse(acao_governo == "ND", ajustes,acao_governo)) %>%
    dplyr::select(-descricao_cofog) %>%
    dplyr::inner_join(COFOG_GC) %>%
    dplyr::inner_join(COFOG_GC, by = c("codigo_cofog_pai"="codigo_cofog")) %>%
    dplyr::rename(descricao_cofog =  descricao_cofog.x,
           descricao_cofog_pai = descricao_cofog.y)

  if (a_cofog_path_completo){
    base_trabalho%>%
      dplyr::mutate(cofog_path = paste(descricao_cofog_pai, descricao_cofog))
  } else{

    base_trabalho%>%
      dplyr::mutate(cofog_path = descricao_cofog_pai)


  }

}
