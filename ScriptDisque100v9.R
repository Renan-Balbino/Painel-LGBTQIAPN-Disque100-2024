
# FUNCOES ================================================================================================================

# Auxiliar na ordem dos fatores:
Orden.Fator <- function(VARIAVEL, VALORES) {

  TodosNiveis <- levels(VARIAVEL)
  OutrosNiveis <- setdiff(TodosNiveis, VALORES)
  NovaOrdem <- c(sort(OutrosNiveis), VALORES)

  fct_relevel(VARIAVEL, NovaOrdem)

}


# Salvar graficos como imagem:
Salv.Graf <- function(NOME, DIMENSOES, PLOT){

  LARGURA <- if_else(DIMENSOES == 1, 6.75, 12)

  ggsave(filename = NOME, plot = PLOT,
         device = png, type = "cairo", dpi = 300,
         width = LARGURA, height = 6.75, units = "in")

}



person <- function(name, title, company, email = NULL, linkedin = NULL, github = NULL) {
  div(
    class = "person",
    h3(class = "name", name),
    div(class = "title", title),
    div(class = "company", company),
    if (!is.null(email) || !is.null(linkedin) || !is.null(github)) {
      div(
        class = "links",
        if (!is.null(email)) {
          a(
            href = paste0("mailto:", email), 
            bs_icon("envelope"), 
            class = "social-link",
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "top",
            title = email
          )
        },
        if (!is.null(linkedin)) {
          a(
            href = linkedin, 
            target = "_blank", 
            bs_icon("linkedin"), 
            class = "social-link",
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "top",
            title = linkedin
          )
        },
        if (!is.null(github)) {
          a(
            href = github, 
            target = "_blank", 
            bs_icon("github"), 
            class = "social-link",
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "top",
            title = github
          )
        }
      )
    }
  )
}





# ESTRUTURA DOS DADOS ====================================================================================================

# CONHECENDO VARIAVEIS ---------------------------------------------------------------------------------------------------

# Variaveis disponiveis:
# names(Dados2024.1)
# names(Dados2024.2)
# 
# 
# # Total de respostas por variavel 2024.1:
# levels(factor(Dados2024.1$UF))
# levels(factor(Dados2024.1$UF_da_vítima))
# levels(factor(Dados2024.1$Município))
# levels(factor(Dados2024.1$Faixa_etária_da_vítima))
# levels(factor(Dados2024.1$Gênero_da_vítima))
# levels(factor(Dados2024.1$Orientação_sexual_da_vítima))
# levels(factor(Dados2024.1$Raça_Cor_da_vítima))
# levels(factor(Dados2024.1$Faixa_etária_do_suspeito))
# levels(factor(Dados2024.1$Orientação_sexual_do_suspeito))
# levels(factor(Dados2024.1$Gênero_do_suspeito))
# levels(factor(Dados2024.1$Raça_Cor_do_suspeito))
# levels(factor(Dados2024.1$Grupo_vulnerável))
# levels(factor(Dados2024.1$Motivação))
# levels(factor(Dados2024.1$Relação_vítima_suspeito))
# levels(factor(Dados2024.1$Religião_da_vítima))
# levels(factor(Dados2024.1$Religião_do_suspeito))
# levels(factor(Dados2024.1$Natureza_Jurídica_do_Suspeito))
# levels(factor(Dados2024.1$Deficiência_da_vítima))
# levels(factor(Dados2024.1$Deficiência_do_suspeito))
# 
# 
# # Total de respostas por variavel 2024.2:
# levels(factor(Dados2024.2$UF))
# levels(factor(Dados2024.2$UF_da_vítima))
# levels(factor(Dados2024.2$Município))
# levels(factor(Dados2024.2$Faixa_etária_da_vítima))
# levels(factor(Dados2024.2$Sexo_da_vítima))
# levels(factor(Dados2024.2$Orientação_sexual_da_vítima))
# levels(factor(Dados2024.2$Raça_Cor_da_vítima))
# levels(factor(Dados2024.2$Faixa_etária_do_suspeito))
# levels(factor(Dados2024.2$Orientação_sexual_do_suspeito))
# levels(factor(Dados2024.2$Sexo_do_suspeito))
# levels(factor(Dados2024.2$Raça_Cor_do_suspeito))
# levels(factor(Dados2024.2$Grupo_vulnerável))
# levels(factor(Dados2024.2$Motivação))
# levels(factor(Dados2024.1$Relação_vítima_suspeito))
# levels(factor(Dados2024.1$Religião_da_vítima))
# levels(factor(Dados2024.1$Religião_do_suspeito))
# levels(factor(Dados2024.1$Natureza_Jurídica_do_Suspeito))
# levels(factor(Dados2024.1$Deficiência_da_vítima))
# levels(factor(Dados2024.1$Deficiência_do_suspeito))
# 
# 
# # Variaveis utilizadas:
# Dados2024.1_2 <- Dados2024.1 %>% select(Data_de_cadastro, UF, Município,
#                                         Faixa_etária_da_vítima,
#                                         Gênero_da_vítima,
#                                         Orientação_sexual_da_vítima,
#                                         Raça_Cor_da_vítima, Religião_da_vítima,
#                                         Deficiência_da_vítima,
#                                         Relação_vítima_suspeito,
#                                         Faixa_etária_do_suspeito,
#                                         Gênero_do_suspeito,
#                                         Orientação_sexual_do_suspeito,
#                                         Raça_Cor_do_suspeito, Religião_do_suspeito,
#                                         Deficiência_do_suspeito,
#                                         Natureza_Jurídica_do_Suspeito,
#                                         Grupo_vulnerável, Motivação)
# 
# 
# Dados2024.2_2 <- Dados2024.2 %>% select(Data_de_cadastro, UF, Município,
#                                         Faixa_etária_da_vítima,
#                                         Sexo_da_vítima,
#                                         Orientação_sexual_da_vítima,
#                                         Raça_Cor_da_vítima, Religião_da_vítima,
#                                         Deficiência_da_vítima,
#                                         Relação_vítima_suspeito,
#                                         Faixa_etária_do_suspeito,
#                                         Sexo_do_suspeito,
#                                         Orientação_sexual_do_suspeito,
#                                         Raça_Cor_do_suspeito, Religião_do_suspeito,
#                                         Deficiência_do_suspeito,
#                                         Natureza_Jurídica_do_Suspeito,
#                                         Grupo_vulnerável, Motivação)
# 
# # PADRONIZANDO VARIAVEIS -------------------------------------------------------------------------------------------------
# 
# # Renomeando variaveis:
# Dados2024.2_2 <- Dados2024.2_2 %>% rename(Gênero_da_vítima = Sexo_da_vítima,
#                                           Gênero_do_suspeito = Sexo_do_suspeito)
# 
# 
# # Juntando dados:
# Dados <- bind_rows(Dados2024.1_2, Dados2024.2_2)
# 
# 
# # Apenas vitimas LGBTQIA+:
# Dados2 <- Dados %>% dplyr::filter(!Orientação_sexual_da_vítima
#                            %in% c("NÃO", "NÃO INFORMADO", "NULL"))
# 
# 
# # Recodificando respostas:
# Dados3 <- Dados2 %>% rename(Data = Data_de_cadastro) %>%
# 
#   mutate(Ano = str_sub(Data, 1, 4),
#          Data = as.character(Data),
# 
#          Orientação_sexual_do_suspeito =
#            if_else(Orientação_sexual_do_suspeito == "NÃO",
#                    "Heterossexual", Orientação_sexual_do_suspeito),
# 
#          Motivação = str_replace_all(Motivação, "\\.", " "),
# 
#          across(-Data, ~ if_else(str_detect(.x, "\\."),
#                                  str_extract(.x, "(?<=\\.).*"), .x)),
# 
#          across(everything(), ~
#                   if_else(str_detect(.x, "N/D|NULL|INTERROMPIDO|NÃO INFORMAD|NÃO SOUBE INFORMAR|NÃO SABE"),
#                           "Não identificado", .x)),
# 
#          across(-Data, ~ if_else(nchar(.x) > 2, str_to_sentence(.x), .x)),
# 
#          across(-Data, ~ str_replace_all(.x, "(?<=/)(\\p{L})", ~ toupper(.x))),
# 
#          Natureza_Jurídica_do_Suspeito =
#            if_else(Natureza_Jurídica_do_Suspeito
#                    %in% c("Órgão público", "Pessoa física"),
#                    str_to_title(Natureza_Jurídica_do_Suspeito),
#                    Natureza_Jurídica_do_Suspeito),
# 
#          Natureza_Jurídica_do_Suspeito =
#            if_else(str_detect(Natureza_Jurídica_do_Suspeito,
#                               "Pessoa jurídica de direito privado"),
#                    "Pessoa Jurídica de direito privado",
#                    Natureza_Jurídica_do_Suspeito),
# 
#          across(-Data, ~ as.factor(.x)),
# 
#          across(c(Deficiência_da_vítima, Deficiência_do_suspeito), ~
#                   fct_relevel(.x, "Não", "Não identificado", after = Inf)),
# 
#          across(c(Orientação_sexual_da_vítima, Orientação_sexual_do_suspeito), ~
#                   fct_relevel(.x, "Outro", "Não identificado", after = Inf)),
# 
#          across(c(Raça_Cor_da_vítima, Raça_Cor_do_suspeito), ~
#                   fct_relevel(.x, "Não identificado", after = Inf)),
# 
#          Motivação = fct_relevel(Motivação, "Não identificado", after = Inf),
# 
#          Município = str_to_title(Município))
# 
# 
# # rio::export(Dados4, "Dados Filtrados.xlsx")
# 
# 
# # Criando variaveis:
# Dados4 <- Dados3 %>%
#   mutate(Semestre = if_else(
#     str_sub(Data, 6, 7) %in% c("01", "02", "03", "04", "05", "06"),
#     paste0(Ano, ".1"), paste0(Ano, ".2")),
# 
# 
#     Mes = fct_recode(factor(str_sub(Data, 6, 7)),
#                      Jan = "01", Fev = "02", Mar = "03", Abr = "04", Mai = "05",
#                      Jun = "06", Jul = "07", Ago = "08", Set = "09", Out = "10",
#                      Nov = "11", Dez = "12"),
# 
# 
#     Regiao =
#       Orden.Fator(
#         fct_collapse(UF,
#                      Norte = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
#                      Nordeste = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
#                      `Centro-Oeste` = c("DF", "GO", "MT", "MS"),
#                      Sudeste = c("ES", "MG", "RJ", "SP"),
#                      Sul = c("PR", "RS", "SC")), "Não identificado"),
# 
# 
#     MunicipioCod = as.numeric(str_extract(Município, ".*(?=\\|)")),
# 
# 
#     Faixa_etária_da_vítima2 =
#       fct_collapse(Faixa_etária_da_vítima,
#                    `0 a 4 anos` = c("Recém-nascido (até 28 dias)",
#                                     "Menos de 01 ano", "01 ano",
#                                     "02 anos", "03 anos", "04 anos"),
#                    `5 a 9 anos` = c("05 anos", "06 anos", "07 anos",
#                                     "08 anos", "09 anos"),
#                    `10 a 14 anos` = c("10 anos", "11 anos", "12 anos",
#                                       "13 anos", "14 anos"),
#                    `15 a 19 anos` = c("15 anos", "16 anos", "17 anos",
#                                       "18 a 19 anos")),
# 
# 
#     Faixa_etária_do_suspeito2 =
#       fct_collapse(Faixa_etária_do_suspeito,
#                    `12 a 14 anos` = c("12 anos", "13 anos", "14 anos"),
#                    `15 a 19 anos` = c("15 anos", "16 anos", "17 anos",
#                                       "18 a 19 anos")),
# 
# 
#     Relação_vítima_suspeito2 =
#       Orden.Fator(
#         fct_collapse(
#         Relação_vítima_suspeito,
# 
#         `Núcleo Familiar Direto` =
#           c("Pai", "Mãe", "Filho(a)", "Neto(a)", "Bisneto(a)", "Trisavô(ó)",
#           "Avô(ó)", "Bisavô(ó)", "Irmão(ã)", "Esposa(o)", "Companheiro(a)",
#           "Companheiro(a) da mãe/Do pai", "Namorado(a)", "Padrasto/Madrasta",
#           "Enteado(a)", "Genro/Nora", "Sogro(a)", "Cunhado(a)", "Tio(a)",
#           "Primo(a)", "Sobrinho(a)", "Ex-esposa(o)", "Ex-companheiro(a)",
#           "Ex-namorado(a)"),
# 
#         `Familiares Indiretos/Não Específicos` =
#           c("Outros familiares",
#           "Pessoa com quem mantém/Manteve convivência familiar",
#           "Padrinho/Madrinha"),
# 
#         `Rede de Convivência Próxima` =
#           c("Amigo(a)", "Amigo(a) da família", "Vizinho(a)",
#           "Mora na mesma residência mas não é familiar",
#           "Morou na mesma residência mas não é familiar",
#           "Aluno(a)", "Colega de trabalho (mesmo nível hierárquico)",
#           "Cuidador(a)"),
# 
#         `Relações de Poder/Hierarquia` =
#           c("Diretor(a) de escola", "Diretor(a) de unidade prisional",
#           "Diretor/Gestor de instituição", "Professor(a)",
#           "Outros profissionais da educação", "Líder religioso(a)",
#           "Treinador(a)/Técnico(a)", "Profissional de saúde",
#           "Empregador/Patrão (hierarquicamente superior)",
#           "Empregado(a) doméstico", "Empregado (hierarquicamente inferior)",
#           "Funcionário, voluntário ou prestador de serviço para instituição",
#           "Prestador(a) de serviço"),
# 
#         Outros = c("Não se aplica", "Outros")),
# 
#         c("Outros", "Não identificado")),
# 
# 
#     Religião_da_vítima2 =
#       Orden.Fator(
#         fct_collapse(
#           Religião_da_vítima,
# 
#           `Religiões Católicas` = c("Católica apostólica romana",
#                                     "Católica apostólica brasileira",
#                                     "Católica ortodoxa"),
# 
#           `Evangélicas/Protestantes` = c("Evangélica", "Testemunhas de jeová"),
# 
#           `Outras religiões cristãs` = c("Outras religiosidades cristãs",
#                                          "Igreja de jesus cristo dos santos dos últimos dias"),
# 
#           `Religiões Afro-brasileiras` = c("Candomblé", "Umbanda",
#                                            "Umbanda e candomblé",
#                                            "Outras declarações de religiosidades afrobrasileira"),
# 
#           `Religiões Espiritualistas` = c("Espírita", "Espiritualista"),
# 
#           Outros = c("Outras religiosidades", "Tradições indígenas",
#                      "Não determinada e multiplo pertencimento",
#                      "Tradições esotéricas", "Budismo", "Hinduísmo",
#                      "Igreja messiânica mundial", "Islamismo", "Judaísmo",
#                      "Outras religiões orientais")),
# 
#         c("Outras religiões cristãs", "Outros", "Não identificado")),
# 
# 
#     Religião_do_suspeito2 =
#       Orden.Fator(fct_collapse(
#           Religião_do_suspeito,
# 
#           `Religiões Católicas` = c("Católica apostólica romana",
#                                     "Católica apostólica brasileira",
#                                     "Católica ortodoxa"),
# 
#           `Evangélicas/Protestantes` = c("Evangélica", "Testemunhas de jeová"),
# 
#           `Outras religiões cristãs` = c("Outras religiosidades cristãs",
#                                          "Igreja de jesus cristo dos santos dos últimos dias"),
# 
#           `Religiões Afro-brasileiras` = c("Candomblé", "Umbanda",
#                                            "Umbanda e candomblé",
#                                            "Outras declarações de religiosidades afrobrasileira"),
# 
#           `Religiões Espiritualistas` = c("Espírita", "Espiritualista"),
# 
#           Outros = c("Outras religiosidades", "Tradições indígenas",
#                      "Não determinada e multiplo pertencimento",
#                      "Tradições esotéricas", "Budismo", "Hinduísmo",
#                      "Igreja messiânica mundial", "Islamismo", "Judaísmo",
#                      "Outras religiões orientais")),
# 
#           c("Outras religiões cristãs", "Outros", "Não identificado")),
# 
# 
#     Deficiência_da_vítima2 =
#       fct_collapse(
#         Deficiência_da_vítima,
#         Sim = c("Auditiva/Surdez", "Autismo", "Física/Motora",
#                    "Mental/Intelectual", "Visual")),
# 
# 
#     Deficiência_do_suspeito2 =
#       fct_collapse(
#         Deficiência_do_suspeito,
#         Sim = c("Auditiva/Surdez", "Autismo", "Física/Motora",
#                    "Mental/Intelectual", "Visual"))) %>%
# 
#   select(Ano, Semestre, Mes, Regiao, UF, Município, MunicipioCod, everything(), -Data)


OptionsGirafe <- list(
  opts_sizing(rescale = TRUE),      # Torna o gráfico responsivo
  opts_toolbar(saveaspng = TRUE),   # Exibe botão salvar
  opts_zoom(max = 5),               # Zoom interativo
  opts_sizing(rescale = TRUE),      # Importante para escalabilidade
  opts_toolbar(saveaspng = FALSE),  # Retirar opcao nativa de download
  opts_tooltip(css = "background-color: #5E213F; color: white; padding: 4px; border-radius: 5px; font-size: 14px;"))




# Geral <- function(){
# TABELAS E GRAFICOS =====================================================================================================

# Dados4 <- import("Dados Filtrados.rds")
  
# Denuncias por Regiao e Orientacao sexual -------------------------------------------------------------------------------
  
# Tabela:
DenunciaRegiao <- Dados4 %>% 
  
  select(Regiao, Orientação_sexual_da_vítima, Orientação_sexual_do_suspeito) %>% 
  
  pivot_longer(cols = -Regiao, 
               names_to = "Individuo", values_to = "Orientação Sexual") %>% 
  
  group_by(Regiao, Individuo, `Orientação Sexual`) %>% count(name = "Total") %>% 
  
  mutate(Individuo = as.factor(Individuo), 
         
         Individuo = fct_recode(Individuo, 
                                "Vítima" = "Orientação_sexual_da_vítima", 
                                "Suspeito" = "Orientação_sexual_do_suspeito"))


# Grafico:
GDenunciaRegiao <- function(.){

  GRAF <- 
  ggplot(., aes(x = `Orientação Sexual`, 
                tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")), 
                y = Total, fill = `Orientação Sexual`)) +

    geom_bar_interactive(stat = "identity") + scale_fill_paletteer_d("rcartocolor::Prism") +

    facet_wrap_interactive(~ Regiao, scales = "free") +

    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.2))) +

    labs(x = "Região", y = "Quantidade total/percentual",
         fill = "Orientação sexual") +

    geom_text_interactive(aes(label = paste0(format(Percentual, nsmall = 2), "%")),
              # fontface = "bold", 
              vjust = -0.5, size = 3.2) +

    theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
          # text = element_text(face = "bold"),
          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, unit = "cm")), 
          
          axis.title = element_text(color = "#5e5e5e", face = "bold"))
  
  # girafe(ggobj = GRAF, 
  #        options = OptionsGirafe, 
  #        height_svg = 6, width_svg = 10.67)

}


# Denuncias por Mes e Orientacao sexual ----------------------------------------------------------------------------------

# Tabela:
DenunciaMes <- Dados4 %>%

  select(Mes, Orientação_sexual_da_vítima, Orientação_sexual_do_suspeito) %>%

  pivot_longer(cols = -Mes,
               names_to = "Individuo", values_to = "Orientação Sexual") %>%

  group_by(Mes, Individuo, `Orientação Sexual`) %>% count(name = "Total") %>%

  mutate(Individuo = as.factor(Individuo),

         Individuo = fct_recode(Individuo,
                                "Vítima" = "Orientação_sexual_da_vítima",
                                "Suspeito" = "Orientação_sexual_do_suspeito"))


# Grafico:
GDenunciaMes <- function(.){

  GRAF <- 
  ggplot(., aes(x = Mes, y = Total, 
                tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")), 
                group = `Orientação Sexual`,
                fill = `Orientação Sexual`)) +

    geom_bar_interactive(stat = "identity") + scale_fill_paletteer_d("rcartocolor::Prism") +

    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +

    labs(x = "Mês", y = "Quantidade total/percentual", fill = "Orientação sexual") +

    geom_text_interactive(aes(label = paste0(format(Percentual, nsmall = 2), "%")),
              # fontface = "bold", 
              color = "white", position = position_stack(vjust = 0.5), size = 3.2) +

    theme(axis.ticks = element_blank(),
          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, unit = "cm")), 
          
          text = element_text(color = "#5e5e5e", face = "bold"))
  
  # girafe(ggobj = GRAF, height_svg = 6, width_svg = 10.67)
  
  }


# Denuncias por Cor/Raca e Orientacao sexual -----------------------------------------------------------------------------

# Tabela 1:
DenunciaCorRacaVitima <- Dados4 %>%

  select(Raça_Cor_da_vítima, Orientação_sexual_da_vítima) %>%

  group_by(Raça_Cor_da_vítima, Orientação_sexual_da_vítima) %>%

  count(name = "Total") %>% mutate(Individuo = "Vítima") %>%

  rename(`Cor/Raça` = Raça_Cor_da_vítima,
         `Orientação Sexual` = Orientação_sexual_da_vítima)


# Tabela 2:
DenunciaCorRacaSuspeito <- Dados4 %>%

  select(Raça_Cor_do_suspeito, Orientação_sexual_do_suspeito) %>%

  group_by(Raça_Cor_do_suspeito, Orientação_sexual_do_suspeito) %>%

  count(name = "Total") %>% mutate(Individuo = "Suspeito") %>%

  rename(`Cor/Raça` = Raça_Cor_do_suspeito,
         `Orientação Sexual` = Orientação_sexual_do_suspeito)


# Tabela 3:
DenunciaCorRaca <- bind_rows(DenunciaCorRacaVitima, DenunciaCorRacaSuspeito)


# Grafico:
GDenunciaCorRaca <- function(.){

  GRAF <- ggplot(., aes(x = `Orientação Sexual`, y = Total, 
                        tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")), 
                        fill = `Orientação Sexual`)) +
    
    geom_bar_interactive(stat = "identity") + scale_fill_paletteer_d("rcartocolor::Prism") +
    
    facet_wrap_interactive(~ `Cor/Raça`, scales = "free") +
    
    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.2))) +
    
    labs(x = "Cor/Raça", y = "Quantidade total/percentual",
         fill = "Orientação sexual") +
    
    geom_text_interactive(aes(label = paste0(format(Percentual, nsmall = 2), "%")),
              # fontface = "bold", 
              # color = "#3e3e3e", 
              vjust = -0.5, size = 3.2) +
    
    theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
          # text = element_text(face = "bold"),
          # text = element_text(color = "#3e3e3e", face = "bold"),
          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, unit = "cm")), 
          
          axis.title = element_text(color = "#5e5e5e", face = "bold"))
  
  # girafe(ggobj = GRAF, height_svg = 6, width_svg = 10.67)

}


# Denuncias por Faixa Etaria e Orientacao sexual -------------------------------------------------------------------------

# Tabela 1:
DenunciaFaixaEtariaVitima <- Dados4 %>%

  select(Faixa_etária_da_vítima2, Orientação_sexual_da_vítima) %>%

  dplyr::filter(Faixa_etária_da_vítima2 != "Não identificado") %>%

  group_by(Faixa_etária_da_vítima2, Orientação_sexual_da_vítima) %>%

  count(name = "Total") %>% mutate(Individuo = "Vítima") %>%

  rename(`Faixa Etária` = Faixa_etária_da_vítima2,
         `Orientação Sexual` = Orientação_sexual_da_vítima)


# Tabela 2:
DenunciaFaixaEtariaSuspeito <- Dados4 %>%

  select(Faixa_etária_do_suspeito2, Orientação_sexual_do_suspeito) %>%

  dplyr::filter(Faixa_etária_do_suspeito2 != "Não identificado") %>%

  group_by(Faixa_etária_do_suspeito2, Orientação_sexual_do_suspeito) %>%

  count(name = "Total") %>% mutate(Individuo = "Suspeito") %>%

  rename(`Faixa Etária` = Faixa_etária_do_suspeito2,
         `Orientação Sexual` = Orientação_sexual_do_suspeito)


# Tabela 3:
DenunciaFaixaEtaria <- bind_rows(DenunciaFaixaEtariaVitima, DenunciaFaixaEtariaSuspeito)


# Grafico:
GDenunciaFaixaEtaria <- function(., PercentualFaixaEtaria){

  GRAF <- ggplot(., aes(x = `Faixa Etária`, y = Total, 
                        tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")), 
                        fill = `Orientação Sexual`)) +
    
    geom_bar_interactive(stat = "identity") + scale_fill_paletteer_d("rcartocolor::Prism") +
    
    coord_flip() +
    
    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.1))) +
    
    labs(x = "Faixas etárias", y = "Quantidade total/percentual",
         fill = "Orientação sexual") +
    
    geom_text(data = PercentualFaixaEtaria,
              aes(x = `Faixa Etária`, y = TotalFaixa,
                  label = paste0(format(Percentual, nsmall = 2), "%")),
              fontface = "bold",
              color = "#3e3e3e",
              inherit.aes = FALSE, hjust = -0.1, size = 3.2) +
    
    theme(axis.ticks = element_blank(),
          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, b = 0.2, unit = "cm")),
          
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D8D8D8"), 
          
          axis.title = element_text(color = "#5e5e5e", face = "bold"))
  
  # girafe(ggobj = GRAF, height_svg = 6, width_svg = 10.67)

}


# Religiao ---------------------------------------------------------------------------------------------------------------

# Tabela 1:
DenunciaReligiaoVitima <- Dados4 %>%

  select(Religião_da_vítima2) %>%

  group_by(Religião_da_vítima2) %>%

  count(name = "Total") %>% mutate(Individuo = "Vítima") %>%

  rename(Religião = Religião_da_vítima2)


# Tabela 2:
DenunciaReligiaoSuspeito <- Dados4 %>%

  select(Religião_do_suspeito2) %>%

  group_by(Religião_do_suspeito2) %>%

  count(name = "Total") %>% mutate(Individuo = "Suspeito") %>%

  rename(Religião = Religião_do_suspeito2)


# Tabela 3:
DenunciaReligiao <- bind_rows(DenunciaReligiaoVitima, DenunciaReligiaoSuspeito)


# Grafico:
GDenunciaReligiao <- function(.){

  GRAF <- 
  ggplot(., aes(x = fct_rev(Religião), y = Total, 
                tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")))) +

    geom_bar_interactive(stat = "identity", fill = "#1d6996") +

    scale_fill_paletteer_d("rcartocolor::Prism") + coord_flip() +

    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.1))) +

    labs(x = "Religião", y = "Quantidade total/percentual", fill = "") +

    geom_text(aes(label = paste0(format(Percentual, nsmall = 2), "%")),
              hjust = -0.1, size = 3.2) +

    theme(axis.ticks = element_blank(), legend.position = "none",

          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, b = 0.2, unit = "cm")),

          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D8D8D8"), 
          
          axis.title = element_text(face = "bold", color = "#5e5e5e"))
  
  # girafe(ggobj = GRAF, height_svg = 3.913, width_svg = 6.96)

}


# Denuncias por Motivacao ------------------------------------------------------------------------------------------------

# Tabela:
DenunciaMotivacao <- Dados4 %>%

  select(Motivação) %>% group_by(Motivação) %>% count(name = "Total")


# Grafico:
GDenunciaMotivacao <- function(.){

  GRAF <- 
  ggplot(., aes(x = fct_rev(Motivação), y = Total, 
                tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")), )) +

    geom_bar_interactive(stat = "identity", fill = "#1d6996") +

    scale_fill_paletteer_d("rcartocolor::Prism") + coord_flip() +

    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.2))) +

    labs(x = "Motivação", y = "Quantidade total/percentual", fill = "") +

    geom_text(aes(label = paste0(format(Percentual, nsmall = 2), "%")),
              hjust = -0.1, size = 3.2) +

    theme(axis.ticks = element_blank(), legend.position = "none",

          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, b = 0.2, unit = "cm")),

          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D8D8D8"), 
          
          axis.title = element_text(color = "#5e5e5e", face = "bold"))
  
  # girafe(ggobj = GRAF, height_svg = 6, width_svg = 10.67)

}


# Classificacao do suspeito ----------------------------------------------------------------------------------------------

# Tabela:
DenunciaClassiSuspeito <- Dados4 %>%

  dplyr::filter(Relação_vítima_suspeito2 != "Própria vítima") %>%

  group_by(Relação_vítima_suspeito2) %>% count(name = "Total") %>%

  rename(`Classificação do Suspeito` = Relação_vítima_suspeito2)


# Grafico:
GDenunciaClassiSuspeito <- function(.){

  GRAF <- 
  ggplot(., aes(x = fct_rev(`Classificação do Suspeito`), y = Total, 
                tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")), )) +

    geom_bar_interactive(stat = "identity", fill = "#1d6996") +

    scale_fill_paletteer_d("rcartocolor::Prism") + coord_flip() +

    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.12))) +

    labs(x = "Classificação do suspeito", y = "Quantidade total/percentual", fill = "") +

    geom_text(aes(label = paste0(format(Percentual, nsmall = 2), "%")),
              hjust = -0.1, size = 3.2) +

    theme(axis.ticks = element_blank(), legend.position = "none",

          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, b = 0.2, unit = "cm")),

          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D8D8D8"), 
          
          axis.title = element_text(color = "#5e5e5e", face = "bold"))
  
  # girafe(ggobj = GRAF, options = OptionsGirafe,  
  #        height_svg = 3.913, width_svg = 6.96)

}


# Deficiencia da vitima --------------------------------------------------------------------------------------------------

# Tabela:
DenunciaDeficiencia <- Dados4 %>%

  select(Deficiência_da_vítima2, Deficiência_do_suspeito2) %>%

  pivot_longer(everything(), names_to = "Individuo", values_to = "Condição") %>%

  group_by(Individuo, Condição) %>% count(name = "Total") %>%

  mutate(Individuo = as.factor(Individuo),
         Individuo = fct_recode(Individuo,
                                "Vítima" = "Deficiência_da_vítima2",
                                "Suspeito" = "Deficiência_do_suspeito2"))


# Grafico:
GDenunciaDeficiencia <- function(.){

  GRAF <- 
  ggplot(., aes(x = fct_rev(Condição), y = Total, 
                tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ",")), )) +

    geom_bar_interactive(stat = "identity", fill = "#1d6996") +

    scale_fill_paletteer_d("rcartocolor::Prism") + coord_flip() +

    scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
                       expand = expansion(mult = c(0, 0.12))) +

    labs(x = "Pessoa com deficiência", y = "Quantidade total/percentual", fill = "") +

    geom_text(aes(label = paste0(format(Percentual, nsmall = 2), "%")),
              hjust = -0.1, size = 3.2) +

    theme(axis.ticks = element_blank(), legend.position = "none",

          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = 0.3, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, b = 0.2, unit = "cm")),

          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D8D8D8"), 
          
          axis.title = element_text(face = "bold", color = "#5e5e5e"))
  
  # girafe(ggobj = GRAF, height_svg = 3.913, width_svg = 6.96)

}


# Denuncias por Estado ---------------------------------------------------------------------------------------------------

# Geolocalizacoes estaduais:
GeoEstado <- read_state(code_state = "all", year = 2020) %>%

  mutate(abbrev_state = as.factor(abbrev_state))


# Simplificando geolocalizacoes:
GeoEstado2 <- ms_simplify(GeoEstado, keep = 0.05, keep_shapes = TRUE)


# Denuncias por estado:
DenunciasEstado <- Dados4 %>%

  dplyr::filter(UF != "Não identificado") %>%

  group_by(UF) %>% count(name = "Total") %>% ungroup() %>%

  mutate(Percentual = round(100 * Total / sum(Total), 2))


# Juntando denuncias com geolocalizacao:
DenunciasEstadoFull <- left_join(GeoEstado2, DenunciasEstado,
                                 by = c("abbrev_state" = "UF"))


# Manipulando posicao dos rotulos:
DenunciasEstadoFull2 <- DenunciasEstadoFull %>%

  mutate(centroid = sf::st_centroid(geom)) %>%
  mutate(lat = sf::st_coordinates(centroid)[,1],
         lon = sf::st_coordinates(centroid)[,2],

         lat2 = case_when(abbrev_state %in%
                            c("RN", "PB", "PE", "AL", "SE",
                              "ES", "RJ", "SC") ~ lat + 2.5,
                          abbrev_state == "DF" ~ lat + 2.0,
                          abbrev_state == "GO" ~ lat - 0.8, TRUE ~ lat),

         lon2 = case_when(abbrev_state == "RN" ~ lon + 0.3,
                          abbrev_state == "SE" ~ lon - 0.8,
                          abbrev_state == "AL" ~ lon - 0.5,
                          abbrev_state == "PE" ~ lon - 0.3,
                          abbrev_state == "DF" ~ lon + 0.6,
                          abbrev_state == "GO" ~ lon - 1.3, TRUE ~ lon))


# Grafico:
GDenunciaMapa <- function(.){

  # DenunciasEstadoFull2 %>%

  GRAF <- 
    ggplot(., aes(fill = Total)) +
    
    geom_sf_interactive(aes(tooltip = paste0("Total: ", comma(Total, big.mark = ".", decimal.mark = ","))),
                        color = "black") +
    
    geom_label_interactive(aes(x = latFim, y = lonFim,
                               label = paste0(abbrev_state, ": ",
                                              format(Percentual, nsmall = 2), "%")),
                           size = 2.5, color = "black", fill = "white", alpha = 0.9,
                           fontface = "bold") +

    scale_fill_distiller_interactive(
      palette = "Reds",
      direction = 1,

      breaks = scales::pretty_breaks(n = 5),
      labels = scales::label_comma(decimal.mark = ",", big.mark = "."),

      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth = unit(12, units = "mm"),
        title.position = "top",         
        title.hjust = 0, 
        label.position = "bottom",
        label.hjust = 0.1, 
        # override.aes = list(size = 1.2), 
        nrow = 1)) +

    labs(x = "", y = "", fill = "Escala de denúncias") + 
    
    theme(text = element_text(face = "bold",
                              size = 15),
          panel.border = element_blank(),

          axis.title.x = element_text(margin =
                                        margin(t = 0.6,
                                               b = 0.3,
                                               unit = "cm")),

          axis.title.y = element_text(margin =
                                        margin(r = 0.6,
                                               l = 0.3,
                                               unit = "cm")),

          axis.text = element_blank(),
          axis.ticks = element_blank(),

          legend.position = "bottom",
          legend.justification = "right",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          # legend.position = c(0.80, 0.10),
          )
  
  # girafe(ggobj = GRAF, height_svg = 6.75, width_svg = 6.75)

}
