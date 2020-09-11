library(tidyverse)
view(diamonds)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) +
        geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

smaller <- diamonds %>%
        filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) +
        geom_histogram(binwidth = 0.1)


ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
        geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
        geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + geom_histogram(binwidth =  0.25)

ggplot(diamonds) +
        geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) +
        geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
        coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>%
        filter(y < 3 | y > 20) %>%
        select(price, x, y, z) %>%
        arrange(y)
unusual

diamonds2 <- diamonds %>%
        filter(between(y, 3, 20))

diamonds2 <- diamonds %>%
        mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
        geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
        geom_point(na.rm = TRUE)

ggplot(data = smaller, mapping = aes(x = price, colour = cut)) +
        geom_freqpoly(binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
        geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
        geom_boxplot()

ggplot(data = mpg) +
        geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y
                                   = hwy))

ggplot(data = mpg) +
        geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y
                                   = hwy)) +
        coord_flip()

ggplot(data = diamonds) +
        geom_count(mapping = aes(x = cut, y = color))

ggplot(data = diamonds) +
        geom_point(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
        geom_boxplot(mapping = aes(group = cut_width(carat, 0.5)))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
        geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>%
        add_residuals(mod) %>%
        mutate(resid = exp(resid))
ggplot(data = diamonds2) +
        geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) +
        geom_boxplot(mapping = aes(x = cut, y = resid))
