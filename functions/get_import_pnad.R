#' @title Importação de dados da pnad 
#' @details Função parametrizada para importar os dados trimestrais da PNAD, de 2013 a 2019, para ase seguintes variáveis:
#' @author Luiz Paulo Tavares Gonçalves

get_import_pnad <- function(anos = 2013:2019, trimestres = 1:4) {
  
  lista_dados <- list()
  i <- 1
  
  for (ano in anos) {
    for (tri in trimestres) {
      
      cat("Baixando:", ano, "T", tri, "\n")
      
      tryCatch({
        
        dados <- PNADcIBGE::get_pnadc(year = ano,
                                      quarter = tri,
                                      design = FALSE) %>%
                 dplyr::select(UF, UPA, Estrato, V2009, V2010, VD3004, V1032) %>%
                 dplyr::mutate(ano = ano, trimestre = tri)   
        
        lista_dados[[i]] <- dados
        i <- i + 1
        
      }, error = function(e) {
        cat("Erro em:", ano, "T", tri, "\n")
      })
    }
  }
  
  dplyr::bind_rows(lista_dados)
}

dados_pnad = get_import_pnad()


