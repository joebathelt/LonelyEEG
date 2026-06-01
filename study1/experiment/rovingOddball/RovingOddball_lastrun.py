#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2022.1.1),
    on Wed Mar 16 15:02:58 2022
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

import psychopy
psychopy.useVersion('latest')


from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, layout
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

import psychopy.iohub as io
from psychopy.hardware import keyboard

n_practice = 30


# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
# Store info about the experiment session
psychopyVersion = '2022.1.1'
expName = 'RovingOddball'  # from the Builder filename that created this script
expInfo = {'participant': ''}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/rovingOddball/RovingOddball_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

# Setup the Window
win = visual.Window(
    size=[800, 800], fullscr=False, screen=0, 
    winType='pyglet', allowGUI=True, allowStencil=False,
    monitor='iMac', color='#bfbfbf', colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='deg')
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess
# Setup ioHub
ioConfig = {}

# Setup iohub keyboard
ioConfig['Keyboard'] = dict(use_keymap='psychopy')

ioSession = '1'
if 'session' in expInfo:
    ioSession = str(expInfo['session'])
ioServer = io.launchHubServer(window=win, **ioConfig)
eyetracker = None

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard(backend='iohub')

# Initialize components for Routine "intro"
introClock = core.Clock()
textbox = visual.TextBox2(
     win, text='Welcome to the Oddball Task! \n\nYou will see a cross in the centre of the screen. \n\nPress <b>SPACE</b> whenever the cross changes colour. \n\nRespond as fast as possible without making any mistakes.\n\n\nPress SPACE to continue.', font='Open Sans',
     pos=(0, 0),units='height',     letterHeight=0.03,
     size=(1.0, 0.5), borderWidth=2.0,
     color='black', colorSpace='rgb',
     opacity=None,
     bold=False, italic=False,
     lineSpacing=1.0,
     padding=0.0, alignment='center',
     anchor='center',
     fillColor=None, borderColor=None,
     flipHoriz=False, flipVert=False, languageStyle='LTR',
     editable=False,
     name='textbox',
     autoLog=True,
)
intro_key = keyboard.Keyboard()

# Initialize components for Routine "practice"
practiceClock = core.Clock()
polygon = visual.ShapeStim(
    win=win, name='polygon', vertices='cross',
    size=(0.4, 0.4),
    ori=0.0, pos=(0, 0), anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-1.0, interpolate=True)
polygon_3 = visual.ShapeStim(
    win=win, name='polygon_3', vertices='cross',
    size=(0.4, 0.4),
    ori=0.0, pos=(0, 0), anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='black',
    opacity=None, depth=-2.0, interpolate=True)
key_response = keyboard.Keyboard()

# Initialize components for Routine "feedback"
feedbackClock = core.Clock()
practice_hits = 0
practice_misses = 0
practice_correctRejects = 0
practice_falseAlarms = 0
trial_counter = 0
msg = ''
feedback_onset = 0.0
feedback_offset = 0.00
feedback_message = visual.TextStim(win=win, name='feedback_message',
    text='',
    font='Open Sans',
    pos=(0, 0), height=1.0, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "trial"
trialClock = core.Clock()
face = visual.ImageStim(
    win=win,
    name='face', 
    image='sin', mask=None, anchor='center',
    ori=0.0, pos=(0, 0.0), size=(5.7, 8.1),
    color=[1,1,1], colorSpace='rgb', opacity=1.0,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-1.0)
polygon_4 = visual.ShapeStim(
    win=win, name='polygon_4', vertices='cross',
    size=(0.4, 0.4),
    ori=0.0, pos=(0, 0), anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-2.0, interpolate=True)
polygon_2 = visual.ShapeStim(
    win=win, name='polygon_2', vertices='cross',
    size=(0.4, 0.4),
    ori=0.0, pos=(0, 0), anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='black',
    opacity=None, depth=-3.0, interpolate=True)
main_key_response = keyboard.Keyboard()

# Initialize components for Routine "main_feedback"
main_feedbackClock = core.Clock()
main_hits = 0
main_correctRejects = 0
main_misses = 0
main_falseAlarms = 0
break_onset = 0.0
break_duration = 0.0
main_feedback_duration = 0.0
feedback_response_duration = 0.0
break_feedback = visual.TextStim(win=win, name='break_feedback',
    text='',
    font='Open Sans',
    pos=(0, 0), height=1.0, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
main_feedback_response = keyboard.Keyboard()

# Initialize components for Routine "end"
endClock = core.Clock()
end_message = visual.TextBox2(
     win, text='That’s it. Thank you for completing the task!', font='Open Sans',
     pos=(0, 0),units='height',     letterHeight=0.05,
     size=(1.0, 0.5), borderWidth=2.0,
     color='white', colorSpace='rgb',
     opacity=None,
     bold=False, italic=False,
     lineSpacing=1.0,
     padding=0.0, alignment='center',
     anchor='center',
     fillColor=None, borderColor=None,
     flipHoriz=False, flipVert=False, languageStyle='LTR',
     editable=False,
     name='end_message',
     autoLog=True,
)
end_task = keyboard.Keyboard()

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "intro"-------
continueRoutine = True
# update component parameters for each repeat
textbox.reset()
intro_key.keys = []
intro_key.rt = []
_intro_key_allKeys = []
# keep track of which components have finished
introComponents = [textbox, intro_key]
for thisComponent in introComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
introClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "intro"-------
while continueRoutine:
    # get current time
    t = introClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=introClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    if intro_key.keys == 'space':
        print('press')
        #intro_port.set_data(0)
    
    # *textbox* updates
    if textbox.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        textbox.frameNStart = frameN  # exact frame index
        textbox.tStart = t  # local t and not account for scr refresh
        textbox.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(textbox, 'tStartRefresh')  # time at next scr refresh
        textbox.setAutoDraw(True)
    
    # *intro_key* updates
    waitOnFlip = False
    if intro_key.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        intro_key.frameNStart = frameN  # exact frame index
        intro_key.tStart = t  # local t and not account for scr refresh
        intro_key.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(intro_key, 'tStartRefresh')  # time at next scr refresh
        intro_key.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(intro_key.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(intro_key.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if intro_key.status == STARTED and not waitOnFlip:
        theseKeys = intro_key.getKeys(keyList=['space'], waitRelease=False)
        _intro_key_allKeys.extend(theseKeys)
        if len(_intro_key_allKeys):
            intro_key.keys = _intro_key_allKeys[-1].name  # just the last key pressed
            intro_key.rt = _intro_key_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in introComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "intro"-------
for thisComponent in introComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('textbox.started', textbox.tStartRefresh)
thisExp.addData('textbox.stopped', textbox.tStopRefresh)
# check responses
if intro_key.keys in ['', [], None]:  # No response was made
    intro_key.keys = None
thisExp.addData('intro_key.keys',intro_key.keys)
if intro_key.keys != None:  # we had a response
    thisExp.addData('intro_key.rt', intro_key.rt)
thisExp.addData('intro_key.started', intro_key.tStartRefresh)
thisExp.addData('intro_key.stopped', intro_key.tStopRefresh)
thisExp.nextEntry()
# the Routine "intro" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
practice_trials = data.TrialHandler(nReps=0.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('practice_conditions.csv'),
    seed=None, name='practice_trials')
thisExp.addLoop(practice_trials)  # add the loop to the experiment
thisPractice_trial = practice_trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisPractice_trial.rgb)
if thisPractice_trial != None:
    for paramName in thisPractice_trial:
        exec('{} = thisPractice_trial[paramName]'.format(paramName))

for thisPractice_trial in practice_trials:
    currentLoop = practice_trials
    # abbreviate parameter names if possible (e.g. rgb = thisPractice_trial.rgb)
    if thisPractice_trial != None:
        for paramName in thisPractice_trial:
            exec('{} = thisPractice_trial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "practice"-------
    continueRoutine = True
    routineTimer.add(0.700000)
    # update component parameters for each repeat
    print(trigger_code)
    polygon.setFillColor(colour)
    key_response.keys = []
    key_response.rt = []
    _key_response_allKeys = []
    # keep track of which components have finished
    practiceComponents = [polygon, polygon_3, key_response]
    for thisComponent in practiceComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    practiceClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "practice"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = practiceClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=practiceClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        if key_response.keys == 'space':
            print('press')
        
        # *polygon* updates
        if polygon.status == NOT_STARTED and tThisFlip >= 0.00-frameTolerance:
            # keep track of start time/frame for later
            polygon.frameNStart = frameN  # exact frame index
            polygon.tStart = t  # local t and not account for scr refresh
            polygon.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon, 'tStartRefresh')  # time at next scr refresh
            polygon.setAutoDraw(True)
        if polygon.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon.tStartRefresh + 0.15-frameTolerance:
                # keep track of stop time/frame for later
                polygon.tStop = t  # not accounting for scr refresh
                polygon.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon, 'tStopRefresh')  # time at next scr refresh
                polygon.setAutoDraw(False)
        
        # *polygon_3* updates
        if polygon_3.status == NOT_STARTED and tThisFlip >= 0.15-frameTolerance:
            # keep track of start time/frame for later
            polygon_3.frameNStart = frameN  # exact frame index
            polygon_3.tStart = t  # local t and not account for scr refresh
            polygon_3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_3, 'tStartRefresh')  # time at next scr refresh
            polygon_3.setAutoDraw(True)
        if polygon_3.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_3.tStartRefresh + 0.55-frameTolerance:
                # keep track of stop time/frame for later
                polygon_3.tStop = t  # not accounting for scr refresh
                polygon_3.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_3, 'tStopRefresh')  # time at next scr refresh
                polygon_3.setAutoDraw(False)
        
        # *key_response* updates
        waitOnFlip = False
        if key_response.status == NOT_STARTED and tThisFlip >= 0.00-frameTolerance:
            # keep track of start time/frame for later
            key_response.frameNStart = frameN  # exact frame index
            key_response.tStart = t  # local t and not account for scr refresh
            key_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_response, 'tStartRefresh')  # time at next scr refresh
            key_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_response.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > key_response.tStartRefresh + 0.70-frameTolerance:
                # keep track of stop time/frame for later
                key_response.tStop = t  # not accounting for scr refresh
                key_response.frameNStop = frameN  # exact frame index
                win.timeOnFlip(key_response, 'tStopRefresh')  # time at next scr refresh
                key_response.status = FINISHED
        if key_response.status == STARTED and not waitOnFlip:
            theseKeys = key_response.getKeys(keyList=['space'], waitRelease=False)
            _key_response_allKeys.extend(theseKeys)
            if len(_key_response_allKeys):
                key_response.keys = _key_response_allKeys[-1].name  # just the last key pressed
                key_response.rt = _key_response_allKeys[-1].rt
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in practiceComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "practice"-------
    for thisComponent in practiceComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    practice_trials.addData('polygon.started', polygon.tStartRefresh)
    practice_trials.addData('polygon.stopped', polygon.tStopRefresh)
    practice_trials.addData('polygon_3.started', polygon_3.tStartRefresh)
    practice_trials.addData('polygon_3.stopped', polygon_3.tStopRefresh)
    # check responses
    if key_response.keys in ['', [], None]:  # No response was made
        key_response.keys = None
    practice_trials.addData('key_response.keys',key_response.keys)
    if key_response.keys != None:  # we had a response
        practice_trials.addData('key_response.rt', key_response.rt)
    practice_trials.addData('key_response.started', key_response.tStartRefresh)
    practice_trials.addData('key_response.stopped', key_response.tStopRefresh)
    
    # ------Prepare to start Routine "feedback"-------
    continueRoutine = True
    # update component parameters for each repeat
    # Count number of correct responses
    if (trial_type == 'response') & (key_response.keys == 'space'):
        practice_hits += 1
        trial_counter += 1
    if (trial_type == 'no_response') & (key_response.keys == 'space'):
        practice_falseAlarms += 1
        trial_counter += 1
    if (trial_type == 'response') & (key_response.keys != 'space'):
        practice_misses += 1
        trial_counter += 1
    if (trial_type == 'no_response') & (key_response.keys != 'space'):
        practice_correctRejects += 1
        trial_counter += 1
    
    if (trial_counter % 10) == 0: # provide feedback every 10 trials
        percent_correct = round(100*(practice_hits + practice_correctRejects)/(trial_counter), 0)
        if percent_correct > 80:
            msg = 'Well done! You got ' + str(int(percent_correct)) + '% correct'
        if percent_correct < 80:
            msg = 'You can do better! You got '+  str(int(percent_correct)) + '% correct'
            n_practice += 10
        feedback_onset = 0.0
        feedback_offset = 0.75
    else: 
        msg = ''
        feedback_onset = 0.0
        feedback_offset = 0.00
    feedback_message.setText(msg)
    # keep track of which components have finished
    feedbackComponents = [feedback_message]
    for thisComponent in feedbackComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    feedbackClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "feedback"-------
    while continueRoutine:
        # get current time
        t = feedbackClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=feedbackClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *feedback_message* updates
        if feedback_message.status == NOT_STARTED and tThisFlip >= feedback_onset-frameTolerance:
            # keep track of start time/frame for later
            feedback_message.frameNStart = frameN  # exact frame index
            feedback_message.tStart = t  # local t and not account for scr refresh
            feedback_message.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(feedback_message, 'tStartRefresh')  # time at next scr refresh
            feedback_message.setAutoDraw(True)
        if feedback_message.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > feedback_message.tStartRefresh + feedback_offset-frameTolerance:
                # keep track of stop time/frame for later
                feedback_message.tStop = t  # not accounting for scr refresh
                feedback_message.frameNStop = frameN  # exact frame index
                win.timeOnFlip(feedback_message, 'tStopRefresh')  # time at next scr refresh
                feedback_message.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in feedbackComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "feedback"-------
    for thisComponent in feedbackComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    practice_trials.addData('feedback_message.started', feedback_message.tStartRefresh)
    practice_trials.addData('feedback_message.stopped', feedback_message.tStopRefresh)
    # the Routine "feedback" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 0.0 repeats of 'practice_trials'


# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1.0, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('conditions.csv'),
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial:
        exec('{} = thisTrial[paramName]'.format(paramName))

for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "trial"-------
    continueRoutine = True
    routineTimer.add(0.700000)
    # update component parameters for each repeat
    print(trigger_code)
    face.setImage(stimulus)
    polygon_4.setFillColor(colour)
    main_key_response.keys = []
    main_key_response.rt = []
    _main_key_response_allKeys = []
    # keep track of which components have finished
    trialComponents = [face, polygon_4, polygon_2, main_key_response]
    for thisComponent in trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trialClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        if main_key_response.keys == 'space':
            print('press')
        
        # *face* updates
        if face.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            face.frameNStart = frameN  # exact frame index
            face.tStart = t  # local t and not account for scr refresh
            face.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(face, 'tStartRefresh')  # time at next scr refresh
            face.setAutoDraw(True)
        if face.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > face.tStartRefresh + 0.15-frameTolerance:
                # keep track of stop time/frame for later
                face.tStop = t  # not accounting for scr refresh
                face.frameNStop = frameN  # exact frame index
                win.timeOnFlip(face, 'tStopRefresh')  # time at next scr refresh
                face.setAutoDraw(False)
        
        # *polygon_4* updates
        if polygon_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_4.frameNStart = frameN  # exact frame index
            polygon_4.tStart = t  # local t and not account for scr refresh
            polygon_4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_4, 'tStartRefresh')  # time at next scr refresh
            polygon_4.setAutoDraw(True)
        if polygon_4.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_4.tStartRefresh + 0.15-frameTolerance:
                # keep track of stop time/frame for later
                polygon_4.tStop = t  # not accounting for scr refresh
                polygon_4.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_4, 'tStopRefresh')  # time at next scr refresh
                polygon_4.setAutoDraw(False)
        
        # *polygon_2* updates
        if polygon_2.status == NOT_STARTED and tThisFlip >= 0.15-frameTolerance:
            # keep track of start time/frame for later
            polygon_2.frameNStart = frameN  # exact frame index
            polygon_2.tStart = t  # local t and not account for scr refresh
            polygon_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_2, 'tStartRefresh')  # time at next scr refresh
            polygon_2.setAutoDraw(True)
        if polygon_2.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_2.tStartRefresh + 0.55-frameTolerance:
                # keep track of stop time/frame for later
                polygon_2.tStop = t  # not accounting for scr refresh
                polygon_2.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_2, 'tStopRefresh')  # time at next scr refresh
                polygon_2.setAutoDraw(False)
        
        # *main_key_response* updates
        waitOnFlip = False
        if main_key_response.status == NOT_STARTED and tThisFlip >= 0.00-frameTolerance:
            # keep track of start time/frame for later
            main_key_response.frameNStart = frameN  # exact frame index
            main_key_response.tStart = t  # local t and not account for scr refresh
            main_key_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(main_key_response, 'tStartRefresh')  # time at next scr refresh
            main_key_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(main_key_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(main_key_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if main_key_response.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > main_key_response.tStartRefresh + 0.70-frameTolerance:
                # keep track of stop time/frame for later
                main_key_response.tStop = t  # not accounting for scr refresh
                main_key_response.frameNStop = frameN  # exact frame index
                win.timeOnFlip(main_key_response, 'tStopRefresh')  # time at next scr refresh
                main_key_response.status = FINISHED
        if main_key_response.status == STARTED and not waitOnFlip:
            theseKeys = main_key_response.getKeys(keyList=['space'], waitRelease=False)
            _main_key_response_allKeys.extend(theseKeys)
            if len(_main_key_response_allKeys):
                main_key_response.keys = _main_key_response_allKeys[-1].name  # just the last key pressed
                main_key_response.rt = _main_key_response_allKeys[-1].rt
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial"-------
    for thisComponent in trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('face.started', face.tStartRefresh)
    trials.addData('face.stopped', face.tStopRefresh)
    trials.addData('polygon_4.started', polygon_4.tStartRefresh)
    trials.addData('polygon_4.stopped', polygon_4.tStopRefresh)
    trials.addData('polygon_2.started', polygon_2.tStartRefresh)
    trials.addData('polygon_2.stopped', polygon_2.tStopRefresh)
    # check responses
    if main_key_response.keys in ['', [], None]:  # No response was made
        main_key_response.keys = None
    trials.addData('main_key_response.keys',main_key_response.keys)
    if main_key_response.keys != None:  # we had a response
        trials.addData('main_key_response.rt', main_key_response.rt)
    trials.addData('main_key_response.started', main_key_response.tStartRefresh)
    trials.addData('main_key_response.stopped', main_key_response.tStopRefresh)
    
    # ------Prepare to start Routine "main_feedback"-------
    continueRoutine = True
    # update component parameters for each repeat
    # Count number of correct responses
    # Hit
    if (trial_type == 'response') & (key_response.keys == 'space'):
        main_hits += 1
        trial_counter += 1
    # False Alarm
    if (trial_type == 'no_response') & (key_response.keys == 'space'):
        main_falseAlarms += 1
        trial_counter += 1
    # Miss
    if (trial_type == 'response') & (key_response.keys != 'space'):
        main_misses += 1
        trial_counter += 1
    # Correct Reject
    if (trial_type == 'no_response') & (key_response.keys != 'space'):
        main_correctRejects += 1
        trial_counter += 1
        
    if (trial_counter % 315) == 0: # provide feedback every 315 trials
        percent_correct = round(100*(main_hits + main_correctRejects)/(trial_counter), 0)
        msg = 'Time for a break! In this block, you got ' + str(int(percent_correct)) + '% correct \n Press SPACE to continue'
        main_feedback_duration = 10*60 # button press ends this earlier
        feedback_response_duration = 10*60
        
        # Reset the counters
        main_hits = 0
        main_correctRejects = 0
        main_misses = 0
        main_falseAlarms = 0
        trial_counter = 0
    else: 
        msg = ''
        main_feedback_duration = 0.00
        feedback_response_duration = 0.00
    break_feedback.setText(msg)
    main_feedback_response.keys = []
    main_feedback_response.rt = []
    _main_feedback_response_allKeys = []
    # keep track of which components have finished
    main_feedbackComponents = [break_feedback, main_feedback_response]
    for thisComponent in main_feedbackComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    main_feedbackClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "main_feedback"-------
    while continueRoutine:
        # get current time
        t = main_feedbackClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=main_feedbackClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *break_feedback* updates
        if break_feedback.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            break_feedback.frameNStart = frameN  # exact frame index
            break_feedback.tStart = t  # local t and not account for scr refresh
            break_feedback.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(break_feedback, 'tStartRefresh')  # time at next scr refresh
            break_feedback.setAutoDraw(True)
        if break_feedback.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > break_feedback.tStartRefresh + main_feedback_duration-frameTolerance:
                # keep track of stop time/frame for later
                break_feedback.tStop = t  # not accounting for scr refresh
                break_feedback.frameNStop = frameN  # exact frame index
                win.timeOnFlip(break_feedback, 'tStopRefresh')  # time at next scr refresh
                break_feedback.setAutoDraw(False)
        
        # *main_feedback_response* updates
        waitOnFlip = False
        if main_feedback_response.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            main_feedback_response.frameNStart = frameN  # exact frame index
            main_feedback_response.tStart = t  # local t and not account for scr refresh
            main_feedback_response.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(main_feedback_response, 'tStartRefresh')  # time at next scr refresh
            main_feedback_response.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(main_feedback_response.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(main_feedback_response.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if main_feedback_response.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > main_feedback_response.tStartRefresh + feedback_response_duration-frameTolerance:
                # keep track of stop time/frame for later
                main_feedback_response.tStop = t  # not accounting for scr refresh
                main_feedback_response.frameNStop = frameN  # exact frame index
                win.timeOnFlip(main_feedback_response, 'tStopRefresh')  # time at next scr refresh
                main_feedback_response.status = FINISHED
        if main_feedback_response.status == STARTED and not waitOnFlip:
            theseKeys = main_feedback_response.getKeys(keyList=['space'], waitRelease=False)
            _main_feedback_response_allKeys.extend(theseKeys)
            if len(_main_feedback_response_allKeys):
                main_feedback_response.keys = _main_feedback_response_allKeys[-1].name  # just the last key pressed
                main_feedback_response.rt = _main_feedback_response_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in main_feedbackComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "main_feedback"-------
    for thisComponent in main_feedbackComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('break_feedback.started', break_feedback.tStartRefresh)
    trials.addData('break_feedback.stopped', break_feedback.tStopRefresh)
    # check responses
    if main_feedback_response.keys in ['', [], None]:  # No response was made
        main_feedback_response.keys = None
    trials.addData('main_feedback_response.keys',main_feedback_response.keys)
    if main_feedback_response.keys != None:  # we had a response
        trials.addData('main_feedback_response.rt', main_feedback_response.rt)
    trials.addData('main_feedback_response.started', main_feedback_response.tStartRefresh)
    trials.addData('main_feedback_response.stopped', main_feedback_response.tStopRefresh)
    # the Routine "main_feedback" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials'


# ------Prepare to start Routine "end"-------
continueRoutine = True
# update component parameters for each repeat
end_message.reset()
end_task.keys = []
end_task.rt = []
_end_task_allKeys = []
# keep track of which components have finished
endComponents = [end_message, end_task]
for thisComponent in endComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
endClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "end"-------
while continueRoutine:
    # get current time
    t = endClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=endClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *end_message* updates
    if end_message.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        end_message.frameNStart = frameN  # exact frame index
        end_message.tStart = t  # local t and not account for scr refresh
        end_message.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(end_message, 'tStartRefresh')  # time at next scr refresh
        end_message.setAutoDraw(True)
    
    # *end_task* updates
    waitOnFlip = False
    if end_task.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        end_task.frameNStart = frameN  # exact frame index
        end_task.tStart = t  # local t and not account for scr refresh
        end_task.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(end_task, 'tStartRefresh')  # time at next scr refresh
        end_task.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(end_task.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(end_task.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if end_task.status == STARTED and not waitOnFlip:
        theseKeys = end_task.getKeys(keyList=['space'], waitRelease=False)
        _end_task_allKeys.extend(theseKeys)
        if len(_end_task_allKeys):
            end_task.keys = _end_task_allKeys[-1].name  # just the last key pressed
            end_task.rt = _end_task_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in endComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "end"-------
for thisComponent in endComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('end_message.started', end_message.tStartRefresh)
thisExp.addData('end_message.stopped', end_message.tStopRefresh)
# check responses
if end_task.keys in ['', [], None]:  # No response was made
    end_task.keys = None
thisExp.addData('end_task.keys',end_task.keys)
if end_task.keys != None:  # we had a response
    thisExp.addData('end_task.rt', end_task.rt)
thisExp.addData('end_task.started', end_task.tStartRefresh)
thisExp.addData('end_task.stopped', end_task.tStopRefresh)
thisExp.nextEntry()
# the Routine "end" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='auto')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
if eyetracker:
    eyetracker.setConnectionState(False)
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
