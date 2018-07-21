data(iris)
res <- umap(iris[,1:4])
tibble(x = res$layout[,1],y = resres$layout[,2],species = iris$Species) %>% 
  ggplot(aes(x = x,y = y,col = species))+
  geom_point()+
  theme(legend.position = "bottom")