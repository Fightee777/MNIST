library(tidyverse)

View(hobbs)
summarise(hobbs)

print(mean(hobbs$r, na.rm = TRUE))
print(mean(hobbs$t, na.rm = TRUE))
print(
  hobbs %>% 
    group_by(nms) %>% 
    reframe(r)
    #reframe(mean(r))
)

?reframe

hobbs %>% 
ggplot(aes(x=t)) +
  geom_histogram(color="red")

hobbs %>% 
  ggplot(aes(x=r, y = t)) +
  geom_density2d_filled()