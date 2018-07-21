#library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
#library("umap", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
#library("magrittr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
#library("tibble", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

res <- umap(ts[,1:3])
tibble(x = res$layout[,1], y = res$layout[,2], Severity = ts$Severity) %>% 
  ggplot(aes(x = x,y = y,col = Severity))+
  geom_point()+
  theme(legend.position = "bottom")