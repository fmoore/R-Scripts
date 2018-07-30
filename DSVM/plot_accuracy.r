#library("ggplot2", lib.loc="C:/Program Files/Microsoft/ML Server/R_SERVER/library")
#library("reshape2", lib.loc="C:/Program Files/Microsoft/ML Server/R_SERVER/library")

# remove previous plots
graphics.off()

log <- read.csv("C:/Users/iris/Projects/run/src/log.csv")
df <- data.frame(Epoch=log$epoch, Acc=log$val_acc)

df_melted <- melt(df, id="Epoch")
p <- ggplot(data=df_melted, aes(x=Epoch, y=value, colour=variable, shape=variable)) + 
  geom_point(size=1) +
  geom_smooth(data=df_melted, method="loess", color="blue", size=0.5) +
  labs(x="Epoch", y="Accuracy") +
  theme_bw()

print(p)