####intro ####
library(ngramr)
library(tidyverse)
library(plotly)
library(ggdark)
library(ggthemes)

#### Functions ####
preparing_function <- function(ngram_table, duties_vector, rights_vector){
  
  ngram_table$Phrase <- as.character(ngram_table$Phrase)
  ngram_table$word <- "new"  
  ngram_table$word[grep(paste0(rights_vector, "*"), tolower(ngram_table$Phrase), value=FALSE)] = "Rights" 
  ngram_table$word[grep(paste0(paste0(duties_vector, collapse="*|"), "*"),tolower(ngram_table$Phrase),value=FALSE)] = "Duties & synonyms"
  
  aggregated_words_search <- aggregate(ngram_table$Frequency,
                                       by=list(Year=ngram_table$Year,
                                               Phrase=ngram_table$word), FUN=sum)
  names(aggregated_words_search)[3] <- "Frequency"
  
  
  ratio <- filter(aggregated_words_search, aggregated_words_search$Phrase=="Rights")$Frequency/
    filter(aggregated_words_search, aggregated_words_search$Phrase=="Duties & synonyms")$Frequency
  
  ratio_df <- data.frame(Year=unique(aggregated_words_search$Year), Phrase="Rights-to-Duties Ratio", Frequency = ratio)
  aggregated_words_search <- rbind(aggregated_words_search, ratio_df)
  return(aggregated_words_search)
  
}

scaler_finder <- function(aggregated_table) {
  scaling_coef <- max(filter(aggregated_table, Phrase=="Rights-to-Duties Ratio")$Frequency, na.rm = T) /
    max(filter(aggregated_table, Phrase!="Rights-to-Duties Ratio")$Frequency)
  
  return(scaling_coef)
}

#### English ####

eng <- ngram(c("duty", "right"
                ,"rights", "duties", "responsibility", "responsibilities"
                ,"obligation","obligations"
                ), year_start = 1782, case_ins = T, smoothing = 3)

eng_gb <- ngram(c("duty", "right"
                  ,"rights", "duties", "responsibility", "responsibilities"
                  ,"obligation","obligations"
), year_start = 1782, case_ins = T, smoothing = 3, corpus="eng_gb_2019")

eng_us <- ngram(c("duty", "right"
                  ,"rights", "duties", "responsibility", "responsibilities"
                  ,"obligation","obligations"
), year_start = 1782, case_ins = T, smoothing = 3, corpus="eng_us_2019")

eng_rights_roots <- "right"
eng_duties_roots <- c("dut", "resp", 'obl')

english <- preparing_function(eng, eng_duties_roots, eng_rights_roots)
english_us <- preparing_function(eng_us, eng_duties_roots, eng_rights_roots)
english_gb <- preparing_function(eng_gb, eng_duties_roots, eng_rights_roots)

#### French ####
fre <- ngram(c("Droits","Droit","Responsabilité","Responsabilités","Obligation","Obligations","devoir","devoirs"
               ), year_start = 1782, case_ins = T, smoothing = 3, corpus="fre_2019")

french_duties_roots <- c("respons","oblig", "devoir")
french_rights_roots <- "droit"

french <- preparing_function(fre, french_duties_roots, french_rights_roots)
#### German ####

ger <- ngram(c("rechte","rechten","Pflichten","Pflich","Verantwortungen","Verantwortung"
                               ), year_start = 1782, case_ins = T, smoothing = 3, corpus="ger_2019")

germ_duties_roots <- c("pflicht", "verantwortung")
germ_rights_roots <-  "recht"

german <- preparing_function(ger, germ_duties_roots, germ_rights_roots)
#### Italian ####
ita <- ngram(c("diritti","doveri","responsabilità","diritto","dovere"
 ), year_start = 1782, case_ins = T, smoothing = 3, corpus="ita_2019")

ita_duties_roots <- c("dover", "respons")
ita_rights_roots <-  "diritt"

italian <- preparing_function(ita, ita_duties_roots, ita_rights_roots)

#### Spanish ####

spa <- ngram(c("Derecho","Derechos","responsabilidades","responsabilidad","deberes", "deber", "obligación", "obligaciones"
), year_start = 1782, case_ins = T, smoothing = 3, corpus="spa_2019")

spa_duties_roots <- c("respons", "deber", "oblig")
spa_rights_roots <-  "derech"

spanish <- preparing_function(spa, spa_duties_roots, spa_rights_roots)

#### Russian ####
rus <- ngram(c("Права","обязанности"), 
              year_start = 1782, case_ins = T, smoothing = 3, corpus="rus_2019")
rus_duties_roots <- "обязанности"
rus_rights_roots <-  "право"

russian <- preparing_function(rus, rus_duties_roots,  rus_rights_roots)
#### Merged ####

all_in_all <- data.frame(Year = filter(english, Phrase!="Rights-to-Duties Ratio")$Year,
                         Phrase = filter(english, Phrase!="Rights-to-Duties Ratio")$Phrase,
                         Frequency = filter(english, Phrase!="Rights-to-Duties Ratio")$Frequency +
  filter(french, Phrase!="Rights-to-Duties Ratio")$Frequency + 
  filter(german, Phrase!="Rights-to-Duties Ratio")$Frequency + 
  filter(italian, Phrase!="Rights-to-Duties Ratio")$Frequency +
  filter(spanish, Phrase!="Rights-to-Duties Ratio")$Frequency +
  filter(russian, Phrase!="Rights-to-Duties Ratio")$Frequency)

all_in_all_ratio <- data.frame(Year = filter(english, Phrase=="Rights-to-Duties Ratio")$Year,
                               Phrase = "Rights-to-Duties Ratio", 
                               Frequency = filter(all_in_all, Phrase=="Rights")$Frequency/filter(all_in_all, Phrase!="Rights")$Frequency) 

all_in_all <- rbind(all_in_all, all_in_all_ratio)
#### plotting all & eng####
ay <- list(
  tickfont = list(size=11.7),
  titlefont=list(size=14.6),
  range = c(0,5),
  overlaying = "y",
  nticks = 8,
  side = "right",
  title = "Rights-to-Duties Ratio")

scaling_coef <- scaler_finder(all_in_all)

plot_all <- ggplotly(
ggplot() + 
  geom_line(data=filter(all_in_all , Phrase!="Rights-to-Duties Ratio"),
            aes(x=Year, y=Frequency, color=Phrase, label = Frequency, label2 = Year)) +
 labs(color = "Content") +
  scale_y_continuous(name="Frequency of Occurrence %", 
      sec.axis = sec_axis(~.*scaling_coef, "Rights-to-Duties Ratio"))+
  scale_color_manual(values = c("coral", "#00BFC4"))
+dark_theme_classic()
, tooltip = c("label", "label2")) %>% 
    add_lines(x=~Year, name="Rights-to-Duties Ratio",
            y=~Frequency, colors=NULL, yaxis="y2",
            data=filter(all_in_all , Phrase=="Rights-to-Duties Ratio"),
      showlegend=TRUE, inherit=FALSE, 
      line = list(shape = 'linear', color = '#C77CFF', width= 1.2, dash = 'dot', alpha=1),
      hovertemplate = "Ratio: %{y}<extra></extra><br>Year: %{x}")  %>%
  layout(yaxis2 = ay, hovermode = "x unified")

plot_eng <- ggplotly(
  ggplot() + 
    geom_line(data=filter(english , Phrase!="Rights-to-Duties Ratio"),
              aes(x=Year, y=Frequency, color=Phrase, label = Frequency, label2 = Year)) +
    labs(color = "Content") +
    scale_y_continuous(name="Frequency of Occurrence %", 
                       sec.axis = sec_axis(~.*scaler_finder(english), "Rights-to-Duties Ratio"))+
    scale_color_manual(values = c("coral", "#00BFC4"))
  +dark_theme_classic()
  , tooltip = c("label", "label2")) %>% 
  add_lines(x=~Year, name="Rights-to-Duties Ratio",
            y=~Frequency, colors=NULL, yaxis="y2",
            data=filter(english , Phrase=="Rights-to-Duties Ratio"),
            showlegend=TRUE, inherit=FALSE, 
            line = list(shape = 'linear', color = '#C77CFF', width= 1.2, dash = 'dot', alpha=1),
            hovertemplate = "Ratio: %{y}<extra></extra><br>Year: %{x}")  %>%
  layout(yaxis2 = ay, hovermode = "x unified")

#### plotting ratios ####
ratio_df <- 
rbind(
cbind(filter(english, Phrase=="Rights-to-Duties Ratio"), Corpus= "English (All)"),
cbind(filter(german, Phrase=="Rights-to-Duties Ratio"), Corpus="German"),
cbind(filter(french, Phrase=="Rights-to-Duties Ratio"), Corpus="French"),
cbind(filter(spanish, Phrase=="Rights-to-Duties Ratio"), Corpus="Spanish"),
cbind(filter(italian, Phrase=="Rights-to-Duties Ratio"), Corpus="Italian"),
cbind(filter(english_us, Phrase=="Rights-to-Duties Ratio"), Corpus="Egnlish (US)"),
cbind(filter(english_gb, Phrase=="Rights-to-Duties Ratio"), Corpus="English (GB)")
,cbind(filter(russian, Phrase=="Rights-to-Duties Ratio"), Corpus="Russian")
)
names(ratio_df)[3] <- "Ratio"

style(ggplotly(
  ggplot(data=ratio_df, aes(x=Year, y=Ratio, color=Corpus)) + 
  geom_line(linetype="dashed")+dark_theme_classic() + labs(title="Ratio of Rights to Responsibilities in the Zeitgeist")
  ) %>% layout(hovermode = "x unified", 
               yaxis=list(range = c(0,12), tickmode="linear", tick0 = 0, dtick=1))
  ,visible="legendonly", traces = 8
)
