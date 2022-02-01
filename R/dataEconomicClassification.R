#' Get data about a set of government functions and economic classifications
#'
#' @param year number
#' @return tibble.
#' @examples
#' dataEconomicClassification(year=2020)
#' @export


dataEconomicClassification<- function(year=2020){

  a_ano<-year

  indice_sheet<-as.numeric(a_ano)-2010+1
  sheet<- paste0("2.",indice_sheet)

  #"./inst/extdata/COFOG-GCO.xlsx"

  COFOG_GC_names<- readxl::read_excel(system.file("extdata", "COFOG-GCO.xlsx", package = "Rcofog") , sheet = sheet,
                              skip = 2, n_max = 1)

  COFOG_GC_dados <- readxl::read_excel(system.file("extdata", "COFOG-GCO.xlsx", package = "Rcofog"), sheet = sheet,
                               skip = 4)

  COFOG_GC<- COFOG_GC_dados

  names(COFOG_GC)<- names(COFOG_GC_names)
  names(COFOG_GC)[1]<-"codigo_cofog"
  names(COFOG_GC)[2]<-"descricao_cofog"

  COFOG_GC[,3]<-  COFOG_GC_dados[,3]+
    COFOG_GC_dados[,4]+
    COFOG_GC_dados[,5]

  COFOG_GC <- COFOG_GC[,-c(4:5)]

  COFOG_GC_gather<-
    COFOG_GC %>%
    dplyr::select(-11)%>%
    tidyr::gather(funcao_economica, valor, -c(1,2)) %>%
    dplyr::filter(codigo_cofog!="7") %>%
    dplyr::mutate(funcao_economica = stringr::str_sub(funcao_economica,5,100),
           codigo_cofog_pai = ifelse(stringr::str_length(codigo_cofog)==3,"7",stringr::str_sub(codigo_cofog,1,3)) )

  COFOG_GC_gather<-
    COFOG_GC_gather%>%
    dplyr::mutate(descricao_cofog = dplyr::case_when(
      descricao_cofog == "Despesa total3" ~  "Despesa total" ,
      descricao_cofog == "Transações da dívida pública4" ~ "Transações da dívida pública",
      descricao_cofog == "Educação infantil e ensino fundamental I5"~ "Educação infantil e ensino fundamental I",
      descricao_cofog == "Sobreviventes" ~"Pensionistas",
      TRUE ~descricao_cofog
    )) %>%
    dplyr::mutate(funcao_economica = dplyr::case_when(
      funcao_economica == " Juros4" ~  "Juros",
      funcao_economica == " Remuneração de empregados6" ~ "Remuneração de empregados",
      funcao_economica == " Investimento bruto 7/" ~  "Investimento bruto",
      funcao_economica == " Investimento bruto em ativos não financeiros7" ~ "Investimento bruto em ativos não financeiros",
      TRUE ~ funcao_economica
    ))

  COFOG_GC_gather$ano<- a_ano

  .data<- COFOG_GC_gather
  .data
}
