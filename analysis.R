#set working directory
setwd("../Desktop/Practice/Fuhball")

#load in cleaned datasets
source("data_wrangle.R")

#load libraries 
library(ggplot2) #for visualizations 
library(ggrepel) #for plot labeling
library(dplyr) #for df manipulation
library(reshape2) #for wide to long transformation

#show hall of famers by draft class
ggplot(Draft_Overview_Data, aes(x = Year, y = HOF, fill = HOF)) + 
        geom_bar(stat = "identity") +
        ggtitle("Hall of Famers by Draft Class") +
        scale_fill_gradient(low = "lightgrey", high = "black") +
        xlab("Draft Year") +
        ylab("Hall of Fame Members") +
        #plain black/white plot with no hashes
        theme_classic() +
        theme(axis.text.x = element_text(angle = 270, vjust = .3, size = 8)) + 
        #show every other year below plot
        scale_x_continuous(breaks = seq(1936, 2001, 2)) + 
        theme(legend.position = "none")

#leaderboards - highs (bench_press, vertical_inches, broad_jump_inches, wonderlic)
leaders_high <- function(combine_category) {
all_combine_data %>%
        select(year, name, position, combine_category) %>%
        group_by(year) %>%
        #select only the best performance
        top_n(n = 1) %>% 
        #remove ties
        slice(row_number(1))
}

#leaderboards - lows (forty_yard, shuttle, three_cone)
leaders_low <- function(combine_category) {
        all_combine_data %>%
                select(year, name, position, combine_category) %>%
                group_by(year) %>%
                #select only the best performance
                top_n(n = -1) %>% 
                #remove ties
                slice(row_number(1))
}

#create dataframes for top performer in each category by year
bench_press_leaders <- leaders_high("bench_press")
vertical_leaders <- leaders_high("vertical_inches")
broad_jump_leaders <- leaders_high("broad_jump_inches")
wonderlic_leaders <- leaders_high("wonderlic")
forty_leaders <- leaders_low("forty_yard")
shuttle_leaders <- leaders_low("shuttle")
three_cone_leaders <- leaders_low("three_cone")

#combine the group to show the collection of leaders where high # is better
peak_performances_high <- rbindlist(l = list(bench_press_leaders, vertical_leaders, 
                                             broad_jump_leaders, wonderlic_leaders), 
                                    fill = TRUE)

#create new target factor variable to show the better score direction
peak_performances_high$target <- "high"

#combine the group to show the collection of leaders where low # is better
peak_performances_low <- rbindlist(l = list(forty_leaders, shuttle_leaders, 
                                            three_cone_leaders), fill = TRUE)

#create new target factor variable to show the better score direction
peak_performances_low$target <- "low"

#shorten names for cleaner plotting
names(peak_performances_high)[4:7] <- c("bench", "vert", "broad", "wonderlic")
names(peak_performances_low)[4:6] <- c("forty", "shuttle", "3_cone")

#merge high & low dataframes
peak_performances <- rbind.data.frame(peak_performances_high, peak_performances_low, 
                                      fill = TRUE)

#create the long version of the dataset
peak_performances_long <- melt(peak_performances, id.vars = c("year", "target", 
                                                              "name", "position"), 
                               measure.vars = c("bench", "vert", "broad", 
                                                "wonderlic", "forty", "shuttle", 
                                                "3_cone"), 
                               variable.name = "combine_event", value.name = "result")

#convert to df
peak_performances_long <- as.data.frame(peak_performances_long)

#create variable for potential labelling use
peak_performances_long$label <- paste(peak_performances_long$name, 
                                      peak_performances_long$result, sep=": ")

#leader over time using facet wrap
#plot best result per year for each category
ggplot(peak_performances_long, aes(x = year, y = result, color = target)) +
        #data point at each yr
        geom_point() + 
        #trend line for each category
        geom_smooth(method = "lm", se = FALSE) +
        #align categories vertically with flexible y axis
        facet_grid(combine_event ~ target, scales = "free") + 
        #remove legend and set facets as horizontal text
        theme(legend.position = "none", strip.text.y = element_text(angle = 0, 
                                                                    hjust = 0)) +
        ggtitle("Top Combine Performances by Event over Time")

#plot forty yard dashes winners over time with most impressive winners acknowledged
peak_performances_long %>% 
        filter(combine_event == "forty", !is.na(result)) %>% 
        ggplot(aes(x = year, y = result, label = label)) +
        #data point at each yr
        geom_point(stat = "identity", aes(color = position)) + 
        #add callout labels to all years with top performers below 4.3
        geom_text_repel(
                data          = subset(peak_performances_long,
                                       combine_event == "forty" & result < 4.3),
                        nudge_y      = 1,
                        direction    = "x",
                        angle        = 90,
                        vjust        = 1,
                        segment.size = 0.2
                ) +
        #trend line for each category
        geom_smooth(method = "lm", se = FALSE) +
        ggtitle("Top Forty Performances Over the Years", subtitle = "Colored by Position & Years with Sub-4.3 Times Called Out")
        
#plot forty yard dashes winners over time with most impressive winners acknowledged
peak_performances_long %>% 
        filter(combine_event == "vert", !is.na(result)) %>% 
        ggplot(aes(x = year, y = result, label = label)) +
        #data point at each yr
        geom_point(stat = "identity", aes(color = position)) + 
        #add callout labels to all years with top performers below 4.3
        geom_text_repel(
                data          = subset(peak_performances_long,
                                       combine_event == "vert" & result > 42),
                nudge_y      = 0.1,
                direction    = "x",
                angle        = 90,
                vjust        = 2,
                segment.size = 0.2
        ) +
        #trend line for each category
        geom_smooth(method = "lm", se = FALSE) +
        ggtitle("Top Vertical Jumps Over the Years", subtitle = "Colored by Position & Years with 42-inch+ Jumps Called Out")

