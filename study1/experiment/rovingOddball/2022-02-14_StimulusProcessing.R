library(webmorphR)
library(magick)

wm_opts(plot.maxwidth = 850*2) # Set maximum plot width

stimuli <- read_stim("/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/selected_stimuli/")
stimuli <- auto_delin(stimuli, replace = TRUE)

# Procrustes alignment of the images
algined_stimuli <- align(stimuli, pt1 = 0, pt2 = 1, procrustes = TRUE, fill = "#d3d3d3")

# Create an oval mask
bounds <- list(t = 500, r = 600, b = 800, l = 600)
masked_stimuli <- mask_oval(algined_stimuli, bounds=bounds, fill='#BFBFBF')

# Create a blank mask
blank <- read_stim("/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/rovingOddball/blank_image.jpg")
masked_blank <- mask_oval(blank, bounds=bounds, fill='#BFBFBF')
write_stim(masked_blank, "/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/rovingOddball/masked_blank", format = "jpg")

# Convert images to greyscale
greyscale_stimuli <- grayscale(masked_stimuli)

# Saving the processed stimuli
write_stim(greyscale_stimuli, dir = "/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/processed_stimuli/", format = "jpg")

# Creating an average face to determine the colour of the background
avg_stim1 <- avg(greyscale_stimuli[0:54])
avg_stim2 <- avg(greyscale_stimuli[54:108])
avg_stim1[2] <- avg_stim2[1]
avg_stim <- avg(avg_stim1)

write_stim(avg_stim1, "/Users/joebathelt/Documents/1_Projects/Loneliness_EEG/experiment/average_face/", format = "jpg")