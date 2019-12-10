library(tidyverse)
library(igraph)
library(viridis)

repository <- list()

for(i in 1:40){
    
    f <- i
    t <- 81 - i
    
    all_options <-
        expand.grid(f:t, f:t) %>%
        setNames(., c('x', 'y')) %>%
        #sample_frac(.7) %>%
        mutate(ord = i)
    
    repository[[i]] <- all_options
    
}

df <-
    bind_rows(repository)

# colors_vec <-
#     viridis(n = 4, option = 'A')
# 
# set.seed(10)
# df$node_col <-
#     sample(colors_vec, nrow(df), replace = TRUE)


### Red + black and white

pdf('rayo_black-red.pdf', width = 10, height = 10)

df %>%
    arrange(ord) %>%
    mutate(calcu = x + y) %>%
    ggplot() +
    geom_point(aes(x = x, y = y, col = ord, alpha = ord),
               shape = 15, size = 5) +
    scale_color_continuous(guide = FALSE, low = '#201B1C', high = '#E72F31') + #black - red
    theme_void() +
    theme(legend.position = 'none') +
    #theme(plot.background = element_rect(fill = 'black')) +
    scale_alpha(guide = FALSE, range = c(.01, .2))

dev.off()


### Red + black and white

pdf('rayo_blue-orange.pdf', width = 10, height = 10)

# Bakground: #3A4249

df %>%
    arrange(ord) %>%
    mutate(calcu = x + y) %>%
    ggplot() +
    geom_point(aes(x = x, y = y, col = ord, alpha = ord),
               shape = 15, size = 5) +
    scale_color_continuous(guide = FALSE, low = '#71CDE5', high = '#FF7300') + #black - red
    theme_void() +
    theme(legend.position = 'none') +
    #theme(plot.background = element_rect(fill = 'black')) +
    scale_alpha(guide = FALSE, range = c(.01, .2))

dev.off()


pdf('rayo_pink-yellow.pdf', width = 10, height = 10)

df %>%
    arrange(ord) %>%
    mutate(calcu = x + y) %>%
    ggplot() +
    geom_point(aes(x = x, y = y, col = ord, alpha = ord),
               shape = 15, size = 5) +
    scale_color_continuous(guide = FALSE, low = '#B700AA', high = '#FFF200') + #black - red
    theme_void() +
    theme(legend.position = 'none') +
    #theme(plot.background = element_rect(fill = 'black')) +
    scale_alpha(guide = FALSE, range = c(.01, .2))

dev.off()



png('rayo_white-green.png', width = 1500, height = 1500)

df %>%
    arrange(ord) %>%
    mutate(calcu = x + y) %>%
    ggplot() +
    geom_point(aes(x = x, y = y, col = ord, alpha = ord),
               shape = 15, size = 5) +
    scale_color_continuous(guide = FALSE, low = 'white', high = '#A8CF37') + #black - red
    theme_void() +
    theme(legend.position = 'none') +
    #theme(plot.background = element_rect(fill = 'black')) +
    scale_alpha(guide = FALSE, range = c(.01, .2))

dev.off()

