% Programmed with Psychtoolbox 3.0.12 on Matlab 2015b


% UPDATE: the food is now administered after each run instead than after each session. 




% input variables:


1) real or testing: 1 = real, it opens a full screen; 2 = test it opens a 	smaller screen for debugging and tests

2) response device ID = insert the ID of the response device, a list with all the devices connected to your machine will appear on the command window, enter the number of the response device you are using (e.g., internal keyboard).

3) subject number: is the subject numeric id

4) session number: number of the experimental session for the participant that is being tested (e.g., 1 = first day; 2 = second day; 3 = third day). 

5) training schedule: insert 1 if participant is part of the 1-day group or 3 if participant is part of the the 3 days group

6) Sweet reward: insert the favourite sweet reward of the participant that is being tested

7) Salty reward: insert the favourite salty reward of the participant that is being tested.

8) Country: 1= USA; 2= AUSTRALIA loads different lists according to the local snacks


(! input 2 3 and 4 will be verified to check for possible inconsistencies, if inputs are inconsistent you will be asked to re-enter these variables)



	
% Experimenter interventions	

1) Show to the participant how to position their fingers on the keyboard: d ???> left middle finger, f ???> left index, j ???> right index, k ???> right middle finger.

1) When the slide with the winning appear it???s time to give the participant the real food to be eaten.

2) when the "bonus all you can eat slide" appears it's devaluation time press space to continue with the script after devaluation