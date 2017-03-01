library(dplyr)
library(ggplot2)
library(Hmisc)

df <- read.csv("crawled_rawdata.csv")

df$height[df$height==0.0] <- NA
df$weight[df$height==0.0] <- NA

df$plays <- as.character(df$plays)
df$plays[grepl('^Right', df$plays)] <- "Right"
df$plays[grepl('^Left', df$plays)] <- "Left"
df$plays[grepl('^Ambi', df$plays)] <- NA
df$plays[df$plays==""] <- NA
df$plays <- as.factor(df$plays)

str(df)

lm_eqn <- function(y, x, df){
  m <- lm(y ~ x, df)
  eq <- substitute( italic(r)^2~"="~r2, list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}

ggplot(df, aes(x=height, y=current_rank)) +
  ggtitle("Ranking vs Height (Both Gender)") +
  geom_point(shape=1) +
  geom_smooth(method=lm) + labs(y = "Ranking", x = "Height (cm)") +
  geom_text(x = 205, y = 1750, 
            label = lm_eqn(df$current_rank, df$height, df), 
            parse = TRUE)

ggplot(df[df$gender=='male',], aes(x=height, y=current_rank)) +
  ggtitle("Ranking vs Height (Male)") +
  geom_point(shape=1) +
  geom_smooth(method=lm) + labs(y = "Ranking", x = "Height (cm)") +
  geom_text(x = 205, y = 1750, 
            label = lm_eqn(df[df$gender=='male',]$current_rank, df[df$gender=='male',]$height, df[df$gender=='male',]), 
            parse = TRUE)

ggplot(df[df$gender=='female',], aes(x=height, y=current_rank)) +
  ggtitle("Ranking vs Height (Female)") +
  geom_point(shape=1) +
  geom_smooth(method=lm) + labs(y = "Ranking", x = "Height (cm)") +
  geom_text(x = 185, y = 1150, 
            label = lm_eqn(df[df$gender=='female',]$current_rank, df[df$gender=='female',]$height, df[df$gender=='female',]), 
            parse = TRUE)

ggplot(df[!is.na(df$plays),], aes(x=plays, y=current_rank, fill=plays)) + 
  ggtitle("Ranking vs Plays (Both Gender)") +
  geom_boxplot() + labs(y = "Ranking", x = "Plays") +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  guides(fill=guide_legend(title="Plays"))

ggplot(df[df$gender=='male' & !is.na(df$plays),],aes(x=plays, y=current_rank, fill=plays)) + 
  ggtitle("Ranking vs Plays (Male)") +
  geom_boxplot() + labs(y = "Ranking", x = "Plays") +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  guides(fill=guide_legend(title="Plays"))

ggplot(df[df$gender=='female' & !is.na(df$plays),], aes(x=plays, y=current_rank, fill=plays)) + 
  ggtitle("Ranking vs Plays (Female)") +
  geom_boxplot() + labs(y = "Ranking", x = "Plays") +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  guides(fill=guide_legend(title="Plays"))

write.csv(df, file = "final_table.csv")


final_matrix <- as.matrix(df[,c('height',
                                'current_rank')])

rcorr(final_matrix, type="pearson")
rcorr(final_matrix, type="spearman")

cor.test(df$height,
         df$current_rank,
         method="pearson")

cor.test(df$height,
         df$current_rank,
         method="spearman")

cor.test(df$height,
         df$current_rank,
         method="kendall")

