
tit = "Connaissance selon la profession"
tabz <- NULL
llt <-  c("cause", "lieu","partie","afaire")
for (l in 22:25){
  zz <- prop.table(table(tt$recode_sante,pull(tt[,l])),1)
  zz <- zz*100
  print(zz)
   zz <- c(llt[l-21], zz[,1])
tabz <- rbind(tabz,zz)
}
aa <- as.tibble(tabz) |>
  pivot_longer(2:4)
  ggplot(aa) +
  aes (x = V1, y = value, fill = name) +
  geom_bar(stat = "identity", position = "dodge")
  labs(title = tit,
       y = "%",
       caption = tit)+
  theme_light() +
  scale_fill_discrete_qualitative(palette = "Dynamic") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(
      size = 12,
      angle = 0,
      vjust = .5
    ),
    axis.text.x = element_text(
      size = 12 ,
      angle = 50,
      hjust = 1
    ),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )