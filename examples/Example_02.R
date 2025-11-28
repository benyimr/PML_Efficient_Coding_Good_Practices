# How to use Styler?

library(dplyr)
library(ggplot2)
mtcars %>%
  group_by(cyl, gear) %>%
  summarise(
    avg_mpg = mean(mpg, na.rm = T),
    total_cars = n(),
    avg_disp = mean(disp)
  ) %>%
  ggplot(aes(x = cyl, y = avg_mpg, fill = factor(gear))) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Average MPG by Cylinders and Gears", x = "Cylinders", y = "Average MPG")


# Install the package
# install.packages("styler")
