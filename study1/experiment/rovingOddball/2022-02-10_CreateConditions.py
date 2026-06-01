# %%
import numpy as np
import os
import pandas as pd

# Setting the randomisation seed
np.random.seed(0)

# %% Selecting stimuli
stimuli = [stimulus for stimulus in os.listdir('./processed_stimuli/') if stimulus.endswith('.jpg')]
angry_stimuli = [stimulus for stimulus in stimuli if stimulus.split('_')[3] == 'a']
happy_stimuli = [stimulus for stimulus in stimuli if stimulus.split('_')[3] == 'h']
neutral_stimuli = [stimulus for stimulus in stimuli if stimulus.split('_')[3] == 'n']

# %%
n_trials = 1260
counter = 0
trials = []
stimulus = np.random.permutation(happy_stimuli)[0]
stimulus_category = 20

for trial in np.arange(0, n_trials):
    if counter > (4 + np.random.randint(0, 4)):
        stimulus_type = np.random.permutation(['angry', 'happy', 'neutral'])[0]
        if stimulus_type == 'angry':
            stimulus = np.random.permutation(angry_stimuli)[0]
            stimulus_category = 10
            counter = 0
        if stimulus_type == 'happy':
            stimulus = np.random.permutation(happy_stimuli)[0]
            stimulus_category = 20
            counter = 0
        if stimulus_type == 'neutral':
            stimulus = np.random.permutation(neutral_stimuli)[0]
            stimulus_category = 30
            counter = 0
    stimulus_repetition = stimulus_category + (counter + 1)
    counter += 1

    # Setting the colour of the fixation cross
    if np.random.randint(20) == 1:
        colour = 'red'
        trial_type = 'response'
        stimulus_code = 100 + stimulus_repetition
    else:
        colour = 'black'
        trial_type = 'no_response'
        stimulus_code = 0 + stimulus_repetition

    trials.append({
        'stimulus': 'processed_stimuli/' + stimulus,
        'stimulus_code': stimulus_code,
        'colour': colour,
        'trial_type': trial_type
    })

pd.DataFrame(trials).to_csv('./conditions.csv', index=False)

# %%
