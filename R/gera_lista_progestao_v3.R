#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

library(tabulizer)
library(writexl)
library(dplyr)
library(lubridate)


dt_lista <- "2023-11-14"

# extracao dos dados
progestao <- extract_tables("pdf-lista-progestao/PROGESTAORELAOENTES14112023.pdf")
progestao <- as.data.frame(do.call(rbind, progestao))
progestao <- progestao[-c(1, 2),-1] # exclui 2 primeiras linhas e 1a coluna
names(progestao) <- c("cnpj",
                      "ente",
                      "uf",
                      "dt_recebimento",
                      "dt_termo_adesao",
                      "dt_certificacao",
                      "nivel_inic",
                      "dt_renovacao",
                      "nivel_atual")



progestao <- progestao %>% 
  mutate(dt_recebimento = dmy(dt_recebimento),
         dt_termo_adesao = dmy(dt_termo_adesao),
         dt_certificacao = dmy(dt_certificacao),
         dt_renovacao = dmy(dt_renovacao),
         dt_vencimento = case_when(!is.na(dt_renovacao) ~ dt_renovacao + ddays(365 * 3 + 1),
                                   !is.na(dt_certificacao) ~ dt_certificacao + ddays(365 * 3 + 1),
                                   TRUE ~ dt_recebimento + ddays(365 * 3 + 1)),
         status = case_when(dt_vencimento - as.Date(dt_lista) < 0 ~ "VENCIDO",
                            dt_vencimento - as.Date(dt_lista) <= 30 ~ "30 dias",
                            dt_vencimento - as.Date(dt_lista) <= 60 ~ "60 dias",
                            dt_vencimento - as.Date(dt_lista) <= 90 ~ "90 dias",
                            dt_vencimento - as.Date(dt_lista) <= 180 ~ "180 dias", # colocar o caso onde dt_recebimento está em branco
                            TRUE  ~ "+180 dias",)) %>% 
  arrange(dt_vencimento)

# Excluir linhas em branco
# Modificação

progestao <- progestao %>% 
  mutate(nivel_inic = ordered(nivel_inic, levels=c("I", "II", "III", "IV")),
         nivel_atual   = ordered(nivel_atual,   levels=c("I", "II", "III", "IV")),
         cnpj = gsub("[[:punct:]]", "", cnpj)) %>% 
  filter(cnpj != "")


comment(progestao) <- format(as.Date(dt_lista), "%d/%m/%Y")

# Exportar os dados
save(progestao,  file="dados/Pro-Gestao.RData")

#write_xlsx(progestao, "ProGestao_2023-10-30.xlsx")






