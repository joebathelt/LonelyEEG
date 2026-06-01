# %%
import mne
import numpy as np
import os
import pandas as pd

# Loading the raw data
biosemi_montage = mne.channels.make_standard_montage('biosemi64')
channel_dict = {
    'A1': 'Fp1', 
    'A2': 'AF7', 
    'A3': 'AF3', 
    'A4': 'F1', 
    'A5': 'F3', 
    'A6': 'F5', 
    'A7': 'F7', 
    'A8': 'FT7', 
    'A9': 'FC5', 
    'A10': 'FC3', 
    'A11': 'FC1', 
    'A12': 'C1', 
    'A13': 'C3', 
    'A14': 'C5', 
    'A15': 'T7', 
    'A16': 'TP7', 
    'A17': 'CP5', 
    'A18': 'CP3', 
    'A19': 'CP1', 
    'A20': 'P1', 
    'A21': 'P3', 
    'A22': 'P5', 
    'A23': 'P7', 
    'A24': 'P9', 
    'A25': 'PO7', 
    'A26': 'PO3', 
    'A27': 'O1', 
    'A28': 'Iz', 
    'A29': 'Oz', 
    'A30': 'POz', 
    'A31': 'Pz', 
    'A32': 'CPz', 
    'B1': 'Fpz', 
    'B2': 'Fp2', 
    'B3': 'AF8', 
    'B4': 'AF4', 
    'B5': 'AFz', 
    'B6': 'Fx', 
    'B7': 'F2', 
    'B8': 'F4', 
    'B9': 'F6', 
    'B10': 'F8', 
    'B11': 'FT8', 
    'B12': 'FC6', 
    'B13': 'FC4', 
    'B14': 'FC2', 
    'B15': 'FCz', 
    'B16': 'Cz', 
    'B17': 'C2', 
    'B18': 'C4', 
    'B19': 'C6', 
    'B20': 'T8', 
    'B21': 'TP8', 
    'B22': 'CP6', 
    'B23': 'CP4', 
    'B24': 'CP2', 
    'B25': 'P2', 
    'B26': 'P4', 
    'B27': 'P6', 
    'B28': 'P8', 
    'B29': 'P10', 
    'B30': 'PO8', 
    'B31': 'PO4', 
    'B32': 'O2'
}
raw = mne.io.read_raw_bdf('../data/2022-02-25_Pilot.bdf', preload=True)

# Rename channels to 10-20 system
mne.rename_channels(raw.info, channel_dict, allow_duplicates=False, verbose=True)

# Set channel types
channel_type_dict = {
    'Fp1': 'eeg',
    'AF7': 'eeg',
    'AF3': 'eeg',
    'F1': 'eeg',
    'F3': 'eeg',
    'F5': 'eeg',
    'F7': 'eeg',
    'FT7': 'eeg',
    'FC5': 'eeg',
    'FC3': 'eeg',
    'FC1': 'eeg',
    'C1': 'eeg',
    'C3': 'eeg',
    'C5': 'eeg',
    'T7': 'eeg',
    'TP7': 'eeg',
    'CP5': 'eeg',
    'CP3': 'eeg',
    'CP1': 'eeg',
    'P1': 'eeg',
    'P3': 'eeg',
    'P5': 'eeg',
    'P7': 'eeg',
    'P9': 'eeg',
    'PO7': 'eeg',
    'PO3': 'eeg',
    'O1': 'eeg',
    'Iz': 'eeg',
    'Oz': 'eeg',
    'POz': 'eeg',
    'Pz': 'eeg',
    'CPz': 'eeg',
    'Fpz': 'eeg',
    'Fp2': 'eeg',
    'AF8': 'eeg',
    'AF4': 'eeg',
    'AFz': 'eeg',
    'Fx': 'eeg',
    'F2': 'eeg',
    'F4': 'eeg',
    'F6': 'eeg',
    'F8': 'eeg',
    'FT8': 'eeg',
    'FC6': 'eeg',
    'FC4': 'eeg',
    'FC2': 'eeg',
    'FCz': 'eeg',
    'Cz': 'eeg',
    'C2': 'eeg',
    'C4': 'eeg',
    'C6': 'eeg',
    'T8': 'eeg',
    'TP8': 'eeg',
    'CP6': 'eeg',
    'CP4': 'eeg',
    'CP2': 'eeg',
    'P2': 'eeg',
    'P4': 'eeg',
    'P6': 'eeg',
    'P8': 'eeg',
    'P10': 'eeg',
    'PO8': 'eeg',
    'PO4': 'eeg',
    'O2':  'eeg',
    'Fx': 'misc',
    'EXG1': 'misc',
    'EXG2':  'misc',
    'EXG3':  'misc',
    'EXG4':  'misc',
    'EXG5': 'misc',
    'EXG6': 'misc',
    'EXG7': 'misc',
    'EXG8': 'misc',
    'GSR1': 'misc',
    'GSR2': 'misc',
    'Erg1': 'misc',
    'Erg2': 'misc',
    'Resp':  'misc',
    'Plet': 'misc',
    'Temp':  'misc'
}

raw.set_channel_types(channel_type_dict, verbose=True)

# Set the refernce (for now)
raw.set_eeg_reference(ref_channels=['Cz'])

# Mark bad channels
raw.info['bads'].extend(['F7', 'POz', 'FC1', 'P3'])

# %%
# Get the channel locations
raw = raw.copy().set_montage(biosemi_montage)

# %%
filtered = raw.copy().filter(l_freq=0.2, h_freq=30)
filtered = filtered.copy().interpolate_bads(reset_bads=True)
filtered = filtered.copy().set_eeg_reference(ref_channels='average')
events = mne.find_events(filtered)

# %%
events_df = pd.read_csv('../experiment/rovingOddball/conditions.csv')
events_df
event_dict = dict()
for stimulus in events_df['stimulus'].unique():
    parts = stimulus.split('/')[-1].split('_')
    model = int(parts[0])
    if model in np.unique(events[:, 2]):
        stim_name = parts[3] + '/' + parts[2] + '/' + parts[0]
        event_dict[stim_name] = model
    
# %%
# Identifying the events
rejection_criteria = dict(eeg=100e-6)
# %%
epochs = mne.Epochs(filtered, events, event_id=event_dict, tmin=-0.2, tmax=0.5, preload=True)
# %%
happy = epochs['h'].average()
angry = epochs['a'].average()
neutral = epochs['n'].average()


mne.viz.plot_compare_evokeds([happy, angry, neutral], picks='P9', colors=['blue', 'red', 'grey'])