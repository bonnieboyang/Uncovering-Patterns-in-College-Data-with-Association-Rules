library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(arules)

set.seed(100)

rm(list = ls()) # empty the environment

# prepare the data
colleges = read.delim('colleges.tsv', header = TRUE, sep = '\t')
colleges_dict = read.delim('colleges_data_dictionary.tsv', header = TRUE, sep = '\t')
str(colleges)
str(colleges_dict)

# discretize the continuous variable 'cost'
colleges$cost_quartiles = discretize(colleges$cost, 
                                     method = 'frequency',
                                     categories = 4,
                                     labels = c('cost_Q1', 'cost_Q2', 'cost_Q3', 'cost_Q4'))

table(colleges$cost_quartiles)


# discretze the continuous variable 'median_earnings'
colleges$earnings_quartiles = discretize(colleges$median_earnings,
                                         method = 'frequency',
                                         categories = 4,
                                         labels =c('earnings_Q1', 'earnings_Q2', 
                                                   'earnings_Q3', 'earnings_Q4'))

# discretze the continuous variable 'median_debt'
colleges$debt_quartiles = discretize(colleges$median_debt,
                                         method = 'frequency',
                                         categories = 4,
                                         labels =c('debt_Q1', 'debt_Q2', 
                                                   'debt_Q3', 'debt_Q4'))

# discretze the continuous variable 'family_income_median'
colleges$family_income_quartiles = discretize(colleges$family_income_median,
                                     method = 'frequency',
                                     categories = 4,
                                     labels =c('family_income_Q1', 'family_income_Q2', 
                                               'family_income_Q3', 'family_income_Q4'))

# feature engineering: high point university
#ref: http://www.prepscholar.com/sat/s/colleges/High-Point-University-admission-requirements
ind = which(colleges$sat_verbal_quartile_2 > 552 &
              colleges$sat_math_quartile_2 > 556 &
              colleges$sat_writing_quartile_2 > 545)

high_point_schools = colleges[ind,]
str(high_point_schools)
head(high_point_schools)

new_colleges = colleges %>% mutate(high_point = 
                                     ifelse(sat_verbal_quartile_2 > 552 &
                                            sat_math_quartile_2 > 556 &
                                            sat_writing_quartile_2 > 545,
                                            TRUE,
                                            FALSE))
str(new_colleges)

# plot the relationship between high SAT score and median earnings
avg = new_colleges %>% na.omit() %>% 
        group_by(high_point) %>% 
        summarise(avg_earnings = mean(median_earnings, na.rm = TRUE))

new_colleges %>% na.omit() %>% 
  ggplot(mapping = aes(x = median_earnings, fill = high_point)) +
  geom_histogram(alpha = 0.6) +
  geom_vline(data = avg, aes(xintercept = avg_earnings, 
                             color = high_point))

# feature engineering: high STEM university
new_colleges = new_colleges %>% mutate(stem_perc = 
                                         architecture_major_perc + comm_tech_major_perc +
                                         computer_science_major_perc + engineering_major_perc +
                                         eng_tech_major_perc + bio_science_major_perc +
                                         math_stats_major_perc,
                                       high_stem = ifelse(stem_perc >= 0.3, TRUE, FALSE))

# generate rules
# select out the columns to mine
college_features = new_colleges %>% 
                    select(locale, control, pred_deg, historically_black,
                           men_only, women_only, religious, online_only,
                           earnings_quartiles, debt_quartiles, cost_quartiles,
                           family_income_quartiles, high_stem,
                           high_point, top_ten)

# plot with ggpairs
ggpairs(na.omit(college_features),
        lower = list(combo = 'dot'),
        diag = list(discrete = 'barDiag'))

# load the data into a transaction object
college_trans = as(college_features, 'transactions')

# view the itemsets
inspect(college_trans[1:3])
summary(college_trans)

# plot the most frequent items
itemFrequencyPlot(college_trans, topN = 10, cex = 0.7)

# run the apriori algorithm
# with a support of 0.01 and a confidence of 0.60
rules_1 = apriori(college_trans, parameter = list(sup = 0.01,
                                                conf = 0.6,
                                                target = 'rules'))

# print distribution information
summary(rules_1)

# with a support of 0.1 and a confidence of 0.60
rules_2 = apriori(college_trans, parameter = list(sup = 0.1,
                                                  conf = 0.6,
                                                  target = 'rules'))

# print distribution information
summary(rules_2)

# with a support of 0.01 and a confidence of 0.10
rules_3 = apriori(college_trans, parameter = list(sup = 0.01,
                                                  conf = 0.1,
                                                  target = 'rules'))

# print distribution information
summary(rules_3)

rules = rules_1
# print distribution information
summary(rules)

items(rules)

# view the rules
inspect(head(rules))

# sort the rules by lift
inspect(head(sort(rules, by = 'lift')))

# investigate interesting patterns with subset
# explore the bottom 25% of earners
bottom_earners = subset(rules, subset = rhs %in% 
                          "earnings_quartiles=earnings_Q1" &
                          lift > 1)

summary(bottom_earners)
inspect(head(bottom_earners, n=5, by = 'lift'))

# explore the high point universities
# create new rules with a support of 0.01 and a confidence of 0.10
high_point_rules = apriori(college_trans, parameter = list(sup = 0.01,
                                                  conf = 0.1,
                                                  target = 'rules'))
high_point_universities = subset(high_point_rules, subset = rhs %in%
                                   'high_point')

summary(high_point_universities)
inspect(head(high_point_universities, n = 10, by = c('support', 'lift')))

# visualize with arulesViz
library(arulesViz)

# scatter plot
plot(rules, shading = 'order')

# filter to narrow in on rules with high confidence
subrules = rules[quality(rules)$confidence > 0.7]
plot(subrules, method = 'grouped', control = list(k = 50))
