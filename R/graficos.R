mapa_perda_valor_ilicito <- function(tidy, map, pop){
  p <- tidy %>%
    filter(teve_multa == 'sim') %>%
    mutate(id = uf) %>%
    group_by(id) %>%
    summarise(vl_perda_bens = sum(vl_perda_bens)) %>%
    inner_join(map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat, map_id = id, fill = vl_perda_bens),
                 color = 'gray30', map = ., data = .) +
        scale_fill_continuous(low = 'white', high = 'green') +
        coord_equal() +
        theme_void()
    }
p}

mapa_perda_valor_ilicito_percapita <- function(tidy, map, pop){
  p <- tidy %>%
    filter(teve_perda_bens == 'sim') %>%
    mutate(id = uf) %>%
    group_by(id) %>%
    summarise(vl_perda_bens = sum(vl_perda_bens)) %>%
    inner_join(pop, 'id') %>%
    mutate(perda_bens_p_cpt = vl_perda_bens / popt * 100000) %>%
    inner_join(map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat, map_id = id, fill = perda_bens_p_cpt),
                 color = 'gray30', map = ., data = .) +
        scale_fill_continuous(low = 'white', high = 'green') +
        coord_equal() +
        theme_void()
    }
  p}

mapa_multa <- function(tidy, map, pop){
  p <- tidy %>%
    filter(teve_multa == 'sim') %>%
    mutate(id = uf) %>%
    group_by(id) %>%
    summarise(vl_multa = sum(vl_multa)) %>%
    inner_join(map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat, map_id = id, fill = vl_multa),
                 color = 'gray30', map = ., data = .) +
        scale_fill_continuous(low = 'white', high = 'green') +
        coord_equal() +
        theme_void()
    }
  p}

mapa_multa_percapita <- function(tidy, map, pop){
  p <- tidy %>%
    filter(teve_multa == 'sim') %>%
    mutate(id = uf) %>%
    group_by(id) %>%
    summarise(vl_multa = sum(vl_multa)) %>%
    inner_join(pop, 'id') %>%
    mutate(multa_p_cpt = vl_multa / popt * 100000) %>%
    inner_join(map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat, map_id = id, fill = multa_p_cpt),
                 color = 'gray30', map = ., data = .) +
        coord_equal() +
        scale_fill_continuous(low = 'white', high = 'green') +
        theme_void()
    }
  p}


mapa_ress <- function(tidy, map, pop){
  p <- tidy %>%
    filter(teve_ressarcimento == 'sim') %>%
    mutate(id = uf) %>%
    group_by(id) %>%
    summarise(vl_ress = sum(vl_ressarcimento)) %>%
    inner_join(map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat, map_id = id, fill = vl_ress),
                 color = 'gray30', map = ., data = .) +
        scale_fill_continuous(low = 'white', high = 'green') +
        coord_equal() +
        theme_void()
    }
  p}

mapa_ress_percapita <- function(tidy, map, pop){
  p <- tidy %>%
  filter(teve_ressarcimento == 'sim') %>%
  mutate(id = uf) %>%
  group_by(id) %>%
  summarise(vl_ress = sum(vl_ressarcimento)) %>%
  inner_join(pop, 'id') %>%
  mutate(ress_p_cpt = vl_ress / popt * 100000) %>%
  inner_join(map) %>% {
    ggplot(.) +
      geom_map(aes(x = long, y = lat, map_id = id, fill = ress_p_cpt),
               color = 'gray30', map = ., data = .) +
      scale_fill_continuous(low = 'white', high = 'green') +
      coord_equal() +
      theme_void()
  }
p}
