library(gcookbook)
library(grid)
library(tidyverse)

## Dummy data
test <- isabel
islice <- filter(isabel, z == min(z))
ggplot(islice, aes(x = x, y = y)) +
  geom_segment(aes(xend = x + vx/50, yend = y + vy/50),
               linewidth = 0.25)   # Make the line segments 0.25 mm thick


## My data
mydata <- read.csv("data_secondary/alt2_dredged_dirvary.csv")

tidydata <- mydata %>%
  select(time, x, y, veloc.x, veloc.y, depth, dir)

## Lil subset
# x = 1242638
# y = 601552.6
tinysubset <- tidydata %>%
  filter(x < 12415000 & x > 1241500) %>%
  filter(y < 603000 & y > 601000) %>%
  mutate(dir = round(dir, 0)) %>%
  filter(str_detect(time, "09:00:00"))
tinysubset$newdir <- ((-tinysubset$dir+90)/360)*2*pi


ggplot(data = tinysubset, aes(x=x, y=y)) + 
  geom_text(aes(angle=dir), label="→") +
  geom_point(data=data.frame(x=c(1242638, 1242650),
                             y=c(601552, 601560)),
             color="red") 

