# %%
import numpy as np
import os
import pandas as pd

# Setting the randomisation seed
np.random.seed(0)

# %% Selecting stimuli
stimuli = [stimulus for stimulus in os.listdir('./SHINE_processed_stimuli/') if stimulus.endswith('.jpg')]
angry_stimuli = [stimulus for stimulus in stimuli if stimulus.split('_')[4] == 'a']
happy_stimuli = [stimulus for stimulus in stimuli if stimulus.split('_')[4] == 'h']
neutral_stimuli = [stimulus for stimulus in stimuli if stimulus.split('_')[4] == 'n']

# Read the trigger codes
triggers_df = pd.read_csv('./trigger_codes.csv')
# %%
n_trials = 1260
repetition = 0
trials = []
stimulus = np.random.permutation(happy_stimuli)[0]
stimulus_type = 'happy_face'

for trial in np.arange(0, n_trials):
    if repetition > (4 + np.random.randint(0, 4)):
        stimulus_type = np.random.permutation(['angry_face', 'happy_face', 'neutral_face'])[0]
        if stimulus_type == 'angry_face':
            stimulus = np.random.permutation(angry_stimuli)[0]
            repetition = 0
        if stimulus_type == 'happy_face':
            stimulus = np.random.permutation(happy_stimuli)[0]
            repetition = 0
        if stimulus_type == 'neutral_face':
            stimulus = np.random.permutation(neutral_stimuli)[0]
            repetition = 0
    repetition += 1

    # Setting the colour of the fixation cross
    if np.random.randint(20) == 1:
        cross_colour = 'red'
    else:
        cross_colour = 'black'

    trigger_code = triggers_df.loc[(triggers_df['face_type'] == stimulus_type) & \
        (triggers_df['cross_colour'] == cross_colour) & \
            (triggers_df['rep_number'] == repetition), 'trigger_code'].values[0]

    trials.append({
        'stimulus': 'SHINE_processed_stimuli/' + stimulus,
        'colour': cross_colour,
        'repetition': repetition,
        'trigger_code': trigger_code
    })

pd.DataFrame(trials).to_csv('./conditions.csv', index=False)

# %%
