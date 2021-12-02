####################
#
# script débit & f0
# Solange & mahault
# 27 janv 2021
# completé 8 avril 2021 
# 
########################
clearinfo

# DJ&WP : shorten variables
silencedb = -25
mindip = 2
minpause = 0.3
nomFich$ = "resultats.csv"

# print a single header line with column names and units
writeFileLine: nomFich$, "idLoc",tab$, "idLivre",tab$, "nomFichier",tab$, "nsyll",tab$, "npause",tab$, "dur (s)",tab$, "durParole(s)",tab$, "debit(nsyll/dur)",tab$, "debitArt (nsyll / durParole)", tab$,"durSyll (durParole/nsyll)"

# Part du répertoire dans lequel est le script. 
repertoire$ = "."

liste_loc = Create Strings as directory list: "list", repertoire$
selectObject: liste_loc



# pour chaque loc

for i_loc from 1 to 1;nb_loc
selectObject: liste_loc
	id_loc$ = Get string: i_loc

	#affichage
	appendInfoLine: id_loc$

	liste_livre = Create Strings as directory list: "list", repertoire$  + "/"+ id_loc$
	
	selectObject: liste_livre
	nb_livre = Get number of strings

	for i_livre from 1 to 1;nb_livre

		selectObject: liste_livre
		id_livre$ = Get string: i_livre

		#affichage
		appendInfoLine: id_loc$ + "/" + id_livre$

		liste_fich = Create Strings as file list: "fichiers", repertoire$  + "/"+ id_loc$ + "/" + id_livre$ + "/*.flac"
	
		selectObject: liste_fich
		nb_fich = Get number of strings

		for i_fich from 1 to nb_fich
			selectObject: liste_fich
			nom_fich$ = Get string: i_fich

			# Traitement
			son = Read from file: repertoire$ + "/"+ id_loc$ + "/" + id_livre$ + "/" + nom_fich$
			
			selectObject: son
			# duree
			dur = Get total duration
			
			#DJ&WP *************
			# Use intensity to get threshold
   			intid = To Intensity... 50 0 yes
   			start = Get time from frame number... 1
   			nframes = Get number of frames
   			end = Get time from frame number... 'nframes'

   			# estimate noise floor
   			minint = Get minimum... 0 0 Parabolic
   			# estimate noise max
   			maxint = Get maximum... 0 0 Parabolic
   			#get .99 quantile to get maximum (without influence of non-speech sound bursts)
   			max99int = Get quantile... 0 0 0.99

   			# estimate Intensity threshold
   			threshold = max99int + silencedb
  			threshold2 = maxint - max99int
  			threshold3 = silencedb - threshold2
  			if threshold < minint
       				threshold = minint
   			endif

  			# get pauses (silences) and speakingtime
   			textgridid = To TextGrid (silences): threshold3, minpause, 0.1, "silent", "sounding"
   			silencetierid = Extract tier... 1
   			silencetableid = Down to TableOfReal... sounding
   			nsounding = Get number of rows
   			npauses = nsounding
   			speakingtot = 0
   			for ipause from 1 to npauses
      				beginsound = Get value: ipause, 1
      				endsound = Get value: ipause, 2
      				speakingdur = endsound - beginsound
      				speakingtot = speakingdur + speakingtot
   			endfor

			selectObject: intid
   			matid = Down to Matrix
   			# Convert intensity to sound
   			sndintid = To Sound (slice): 1
   
			# use total duration, not end time, to find out duration of intdur
 			# in order to allow nonzero starting times.
 			intdur = Get total duration
 			intmax = Get maximum: 0, 0, "Parabolic"

			# estimate peak positions (all peaks)
			ppid = To PointProcess (extrema): "Left", "yes", "no", "Sinc70"

			numpeaks = Get number of points

   			# fill array with time points
   			for i from 1 to numpeaks
       				t'i' = Get time from index: 'i'
   			endfor
	 		# fill array with intensity values
   			selectObject: sndintid
   			peakcount = 0
   			for i from 1 to numpeaks
       				value = Get value at time: t'i', "Cubic"
       				if value > threshold
             				peakcount += 1
             				int'peakcount' = value
             				timepeaks'peakcount' = t'i'
       				endif
   			endfor

 			# fill array with valid peaks: only intensity values if preceding
   			# dip in intensity is greater than mindip
   			selectObject: intid
   			validpeakcount = 0
   			currenttime = timepeaks1
   			currentint = int1

   			for p to peakcount-1
      				following = p + 1
      				followingtime = timepeaks'following'
      				dip = Get minimum: currenttime, followingtime, "None"
      				diffint = abs(currentint - dip)

      				if diffint > mindip
         				validpeakcount += 1
         				validtime'validpeakcount' = timepeaks'p'
      				endif
         			currenttime = timepeaks'following'
         			currentint = Get value at time: timepeaks'following', "Cubic"
   			endfor

		# Look for only voiced parts
   		selectObject: son
   		pitchid = To Pitch (ac): 0.02, 30, 4, "no", 0.03, 0.25, 0.01, 0.35, 0.25, 450
  
   		voicedcount = 0
   		for i from 1 to validpeakcount
      			querytime = validtime'i'

			selectObject: textgridid
      			whichinterval = Get interval at time: 1, 'querytime'
      			whichlabel$ = Get label of interval: 1, 'whichinterval'

      			selectObject: pitchid
      			value = Get value at time: 'querytime', "Hertz", "Linear"

      			if value <> undefined
         			if whichlabel$ = "sounding"
             				voicedcount = voicedcount + 1
             				voicedpeak'voicedcount' = validtime'i'
         			endif
      			endif
   		endfor
   
   		# calculate time correction due to shift in time for Sound object versus
   		# intensity object
   		timecorrection = dur/intdur

  
   		# clean up before next sound file is opened
    		selectObject: intid
    		plusObject: matid
    		plusObject: sndintid
    		plusObject: ppid
    		plusObject: pitchid
    		plusObject: silencetierid
    		plusObject: silencetableid

    		Remove
	
		# write results in output file
   		debit = voicedcount/dur
   		debitArt = voicedcount/speakingtot
   		npauses = npauses-1
   		durSyll = speakingtot/voicedcount
  
   		appendFileLine: nomFich$, id_loc$, tab$,id_livre$,tab$, nom_fich$,tab$, voicedcount,tab$, npauses,tab$, fixed$ (dur,2),tab$, fixed$ (speakingtot,2),tab$, fixed$ (debit,2),tab$, fixed$ (debitArt,2),tab$, fixed$ (durSyll,3) 
		endfor
  	
	selectObject: liste_fich
	Remove
	endfor

	selectObject: liste_livre 
	Remove

endfor

appendInfoLine: "fin"
selectObject: liste_loc
Remove


