install.packages("gganimate")
install.packages("gifski")

library(tidyverse)
library(ggplot2)
library(gifski)
library(gganimate)

setwd("~/Documents/GitHub/RProjects/AssetCorrelationsVisual/")

#Copy the output from Python
system("cp /Users/Ramiz/Documents/GitHub/PyProjects/AssetCorrelations/equity_corrs_final.csv /Users/Ramiz/Documents/GitHub/RProjects/AssetCorrelationsVisual/")

#Read in data 
equity_corrs_output <- read_csv(file = "./equity_corrs_final.csv", col_names = TRUE)

#For easy plotting, we need to get our data to "tall" format from its current "wide" format
# Also, rename the Asset var to Asset Pair
tall_data <- gather(equity_corrs_output, `Asset Pair`, Correlation, `WIL5000`:`WIL5000:Gold`)

#When plotting, I want to have a trace point that follows the animation. It needs to be neat though, so it'll just be the second half of the asset pair label, like BoFA_AAA or Gold.
tall_data <- tall_data %>% mutate(LabelTrack = sub(".*:", "", `Asset Pair`))


#Since it's a 180 day rolling correlation, there will be NAs we have to exclude. We also need to exclude the WIL500 correlation (with itself)
tall_data_filt <- tall_data %>% filter(!is.na(Correlation) & `Asset Pair` != "WIL5000")

#A more narrow look since the entire data from 2010 ish looks busy
tall_data_filt_period <- tall_data %>% filter(!is.na(Correlation) & `Asset Pair` != "WIL5000" & Date >= "2016-01-01")


#For plotting
anim_corr <- ggplot(tall_data_filt_period, aes(Date, Correlation)) + 
  geom_line(aes(group = `Asset Pair`, color = `Asset Pair`)) + 
  scale_color_brewer(palette = "Set2") + #Specifcy a custom color palette
  labs(title = 'Asset Pair Correlations (with Equities) - Rolling 180 Day Period', y = 'Correlation', caption = "Source: FRED") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.005)) +
  geom_hline(yintercept=0.0, linetype="dashed", color = "black") +
  transition_reveal(Date) + #Animate the plot by the Date var
  coord_cartesian(clip = 'on') + 
  geom_point(size = 1) + 
  geom_text(aes(label = LabelTrack), hjust = 0) #Assign a label to follow the line

#Run animation

#Adjust resolution
options(gganimate.dev_args = list(width = 8, height = 5, units = 'in', res=500))

animate_set <- animate(anim_corr, fps = 12, rewind = FALSE)

#For MP4
renderer <- ffmpeg_renderer()
options(gganimate.dev_args = list(width = 8, height = 5, units = 'in', res=500))
animate_set_vid <- animate(anim_corr, renderer = ffmpeg_renderer(), fps = 12, rewind = FALSE)

anim_save("anim_asset_corr_vid.mp4", animate_set_vid)

#Saving the static plot
png("asset_corr_static.png", units="in", width=10, height=10, res=300)
ggplot(tall_data_filt_period, aes(Date, Correlation)) + 
  geom_line(aes(group = `Asset Pair`, color = `Asset Pair`)) + 
  scale_color_brewer(palette = "Set2") + #Specifcy a custom color palette
  labs(title = 'Asset Pair Correlations (with Equities) - Rolling 180 Day Period', y = 'Correlation', caption = "Source: FRED") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.005)) + 
  geom_hline(yintercept=0.0, linetype="dashed", color = "black")
dev.off()