library(dplyr)
library(bayesboot)
library(ggplot2)

# Read and mutate data
seasons <- 2020
pbp_i <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

#Prepare data frame
pbp = pbp_i %>% 
filter(
  season_type == 'REG',
  play_type %in% c('pass'),
  !is.na(epa),
  !is.na(name)
) %>% 
  group_by(passer_player_id) %>%
  mutate(
    num_plays = n()
  ) %>% 
  ungroup() %>%
  filter(
    num_plays >= 120
  )


#Bootstrapping
#Here you will use your data and RYOE instead of EPA
lst = list()
qbs = unique(pbp$name)
for (qb in qbs){
  pbp_qb = pbp %>% filter(name == qb)
  b <- bayesboot(as.vector(pbp_qb$epa), mean)
  s = summary(b)
  mean_epa = s$value[1]
  lci = s$value[3]
  uci = s$value[4]
  df = data.frame('mean'=mean_epa,'LCI'=lci,'UCI'=uci)
  lst[[qb]] = df
}
df = dplyr::bind_rows(lst)
df$QB = qbs
df = df %>% arrange(mean)

#Plot
#This is the basic, the rest in terms of aesthetics and 
#fancy stuff is up to you!
 
df %>%
  ggplot(aes(x=factor(QB, level = QB),y=mean)) + 
  geom_pointrange(aes(ymin=(LCI),
                      ymax=(UCI)))+
  coord_flip()+
  theme_bw() + ggsave('plot1.png',height = 8, width = 10)


#You can do a whole lot of things with this code for example:
lst = list()
qbs = c('D.Prescott','J.Goff','C.Wentz')
for (qb in qbs){
  pbp_qb = pbp %>% filter(name == qb)
  b <- bayesboot(as.vector(pbp_qb$epa), mean)
  df = data.frame('estimate'=b$V1,'QB'=qb,team_abbr  = unique(pbp_qb$posteam))
  lst[[qb]] = df
}

df = dplyr::bind_rows(lst)
colors = nflfastR::teams_colors_logos %>% filter(team_abbr %in% c('DAL','LA','PHI'))

plot %>% ggplot(aes(x=estimate)) +
  geom_density(aes(fill=QB),alpha=.6)+
  scale_fill_manual(
    values = c(colors$team_color[3],colors$team_color[1],colors$team_color2[2])
  ) + theme_bw() + theme(plot.title = element_text(size=18))+
  labs(
    title = 'Using Bayesian Bootstrapping to estimate range of probabilities \nfor QB class 2016'
  )


