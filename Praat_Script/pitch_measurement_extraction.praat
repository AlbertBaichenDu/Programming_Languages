# Measurement of pitch
# Albert Baichen Du

# Initial Settings of the Audio Files
form Settings
	sentence stimuli_name multiple_generalisation
	positive target_tier 1
	sentence gender F
	comment Number of Points to measure during the course of each interval:
	natural measurement_points 20 (= Every 5%)
endform

# Adjust Pitch Floor Based on Gender
if gender$ == "F"
	pitch_floor = 100
elif gender$ == "M"
	pitch_floor = 75
endif

output_file$ = "result_" + "pitch_" + stimuli_name$ + ".csv"

wav = Open long sound file: stimuli_name$ + ".wav"
tgfile$ = stimuli_name$ + ".TextGrid"

selectObject: wav
	tg = Read from file: tgfile$
	nInt = Get number of intervals: target_tier

	for i to nInt
		selectObject: tg
		target_interval$ = Get label of interval: target_tier, i

		interval_start = Get start time of interval: target_tier, i
		interval_end = Get end time of interval: target_tier, i

		duration = interval_end - interval_start
		mid_time = interval_start + 0.5 * duration

		appendInfoLine: target_interval$

		tg_chunck = Extract part: interval_start, interval_end, "yes"

		selectObject: wav

			wav_chunck = Extract part: interval_start, interval_end, "yes"

			selectObject: wav_chunck

			pitch_chunck = To Pitch: 0, pitch_floor, 600

		selectObject: pitch_chunck

			pitches$ = ""

				for t from 0 to measurement_points

						percent = t * (1/measurement_points)
						time_percent = interval_start + (percent * duration)

						pitch_stimuli = Get value at time: time_percent, "Hertz", "Linear"

						pitches$ = pitches$ + string$(pitch_stimuli) + ","
				endfor


				appendFile: output_file$, stimuli_name$, ",", target_interval$, "," 
				appendFile: output_file$, pitches$
				appendFile: output_file$, newline$


		removeObject: wav_chunck
		removeObject: tg_chunck
		removeObject: pitch_chunck

	endfor


appendInfoLine: "Finished! Press Command+W to quit."









