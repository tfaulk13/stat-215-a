# define a nice ggplot2 theme with legend at bottom right
theme_nice_br <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"), 
        legend.justification = c(0.8, 0.1), 
        legend.position = c(0.8, 0.1))

# define a nice ggplot2 theme with legend at bottom right
theme_nice_bt <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"), 
        legend.position = "bottom")

# define a nice ggplot2 theme with legend at bottom right and no y axis
theme_nice_bt_ny <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"), 
        legend.position = "bottom", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# define a nice ggplot2 theme
theme_nice <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"))

# define a nice ggplot2 theme without legend nor y axis
theme_nice_woly <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,face = "bold"),
        title = element_text(size = 14,face = "bold"), 
        legend.position = "none", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# define a nice ggplot2 theme without legend
theme_nice_wol <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,face = "bold"),
        title = element_text(size = 14,face = "bold"), 
        legend.position = "none")

# define a nice ggplot2 theme with huge legend 
theme_nice_big <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        legend.text=element_text(size=16, face = "bold"))

# define a nice ggplot2 theme with huge legend without y axis
theme_nice_big_ny <- theme_classic() + 
  theme(axis.line.y = element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        legend.text=element_text(size=16, face = "bold"), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


