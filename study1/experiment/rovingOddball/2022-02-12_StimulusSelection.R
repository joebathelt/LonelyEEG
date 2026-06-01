library(ggplot2)
library(MatchIt)
library(psych)

stimuli <- read.csv('/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/FACES_selected_stimuli.csv')
stimuli <- stimuli[, c('Stimulus', 'expression', 'sex', 'total_rating', 'male_rating', 'female_rating')]
stimuli <- stimuli[(stimuli['total_rating'] > 90) & (stimuli['male_rating'] > 90) & (stimuli['female_rating'] > 90), ]
matching_group <- stimuli[(stimuli['expression'] == 'angry') & (stimuli['sex'] == 'male'), ]

describeBy(stimuli, group=c('expression', 'sex'))
matching_group['matching_group'] <- 1
all_matches <- c()

# Propensity matching across all ratings with the smallest group as the reference
for (expression in c('angry', 'happy', 'neutral')){
  for (sex in c('male', 'female')){
    if ((expression == 'angry') & (sex == 'male')){
      all_matches <- rbind(all_matches, matching_group['Stimulus'])
    }
    else {
      other_group <- stimuli[(stimuli['expression'] == expression) & (stimuli['sex'] == sex), ]
      other_group['matching_group'] <- 0
      combined_groups <- rbind(matching_group, other_group)
      matches <- matchit(matching_group ~ total_rating + male_rating + female_rating,
                         data=combined_groups,
                         ratio=1,
                         replace=F)
      matches <- match.data(matches)
      matches <- matches[matches[, 'matching_group'] == 0, ]
      all_matches <- rbind(all_matches, matches['Stimulus'])
    }
  }
}

stimuli <- stimuli[stimuli$Stimulus %in% all_matches$Stimulus, ]

ggplot(stimuli, aes(x=expression, y=total_rating, hue=sex)) + 
  geom_boxplot()

group1 <- stimuli[(stimuli$expression == 'happy') & (stimuli$sex == 'female'), 'total_rating']
group2 <- stimuli[(stimuli$expression == 'angry') & (stimuli$sex == 'female'), 'total_rating']
t.test(group1, group2)

write.csv(all_matches$Stimulus, '/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/matched_FACE_stimuli.csv', row.names=F)
