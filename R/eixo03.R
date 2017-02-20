graf_tempo <- function(d) {
  d %>%
    mutate(tempo = dt_pena - dt_propositura) %>%
    ggplot(aes(x = tempo)) +
    geom_histogram(fill = 'transparent', colour = 'black') +
    theme_bw()
}
