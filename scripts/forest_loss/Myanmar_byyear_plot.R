setwd('Z:/Interns/David/Myanmar/ArcMap_working_folder/Myanmar_townships/Scripts')

source("Myanmar_table.R")

byyear$Year <- as.integer(byyear$Year)



plot <- ggplot(byyear, aes(x = Year, y = perloss, fill = perloss)) +
  geom_col() +
  scale_fill_gradient2(low = '#003300', high = '#990000', mid = '#eae43f',
                       midpoint = mean(byyear$perloss), limits = c(0, .011), oob = squish) +
  ylab("Deforested Area as % of Forest Cover in 2000") +
  coord_cartesian(xlim = NULL, ylim = c(0.00, 0.015), expand = TRUE) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 24, face = 'bold')) +
  theme(axis.title.y = element_text(size = 24, face = 'bold')) + 
  scale_x_continuous(breaks=seq(2001, 2016, 5 )) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle('Forest Loss Per Year in Myanmar') +
  theme(plot.title = element_text(size = 40))
labs(subtitle = 'township') +
  labs(subtitle = 'Total Deforestation in township')
theme(plot.title = element_text(size = 20))

plot

#Save plot to a file named after the township
ggsave('byyear_plot.png', width = 10, height = 10)