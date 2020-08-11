
function [RT, pressed_correct, pressed_all, ACC, RT_all, Button, got_reward,avail_cmpt, available_time, duration] = drawnActiveScreen (var, ii)

% Last modifed on May 2017

startT = GetSecs(); % time the begining of the trial

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INITILIZE OUTPUTS
RT = NaN;
RT_all = NaN;
Button = {};
ACC = NaN;
pressed_correct = 0;
pressed_all = 0;
got_reward = 0;
avail_cmpt = 0; % counter to compute how many time reward becomes available
available_time = [];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINE POSITIONS

CUEbaseRect     = [0 0 var.CUEdim var.CUEdim]; % Make a base Rect
FRACTALbaseRect = [0 0 var.FRACTALdim var.FRACTALdim];
REWARDbaseRect  = [0 0 var.REWARDdim var.REWARDdim]; 
ACTIONbaseRect  = [0 0 var.ACTIONdim var.ACTIONdim];

numSqaures = length(var.squareXpos);
allRectsV = nan(4, 4);% Make our rectangle coordinates
for i = 1:numSqaures
    allRectsV(:, i) = CenterRectOnPointd(CUEbaseRect, var.squareXpos(i), var.yUpper);
end

positionFractal = CenterRectOnPointd(FRACTALbaseRect, var.xCenter, var.yCenter);
positionReward  = CenterRectOnPointd(REWARDbaseRect, var.xCenter, var.yLower);    
positionAction  = CenterRectOnPointd(ACTIONbaseRect, var.xCenter, var.yLower);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GET THE RIGHT ELEMENTS GIVEN EXPERIMENTAL CONDITION

switch var.condition(ii)

    case 1 % sweet
        position_response_cue = CenterRectOnPointd(CUEbaseRect,var.sweet_square_pos, var.yUpper);
        fractal               = var.sweet_fractal;
        response              = var.sweet_action;
        reward                = var.sweetImage;
        
    case 2 % salty
        position_response_cue = CenterRectOnPointd(CUEbaseRect,var.salty_square_pos, var.yUpper);
        fractal               = var.salty_fractal;
        response              = var.salty_action;
        reward                = var.saltyImage;
        
    case 0 % baseline (just show rest block)
        fractal               = var.rest_fractal;
        timer = GetSecs()-var.time_MRI;
        while timer < var.ref_end
            timer = GetSecs()-var.time_MRI;
            % baseline screen
            fractal_text  = Screen('MakeTexture',var.w, fractal);
            Screen('DrawTexture', var.w, fractal_text ,[], positionFractal);
            Screen('Close', fractal_text);
            Screen('FrameRect', var.w, var.allColors, allRectsV, 15); % reponse grid
            Screen('Flip',var.w);
        end
        duration   = GetSecs()-startT;
        return 
end

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% START RECORDING RESPONSES AND REINFORCE THEM WITH A VI 10s

cmpt = 0; % counter
seconds_cmpt = 1; % second counter to compute reward availability
availability = 0; % set availability to 0
slideOnT = GetSecs();
timer = GetSecs()-var.time_MRI;
KbQueueCreate(var.deviceIndex);
KbQueueStart(var.deviceIndex);

%FlushEvents(); % clean the keyboard memory
while timer < var.ref_end
       
    timer = GetSecs()-var.time_MRI;
    timer_seconds = GetSecs() - slideOnT;
    
    % compute reward availablitly
    if timer_seconds  > (seconds_cmpt - 0.110) && timer_seconds < (seconds_cmpt + 0.110)% if one sec has elapse compute probability to get reward (account for the +- 100 ms) variablity if the botton is pressed or not
        disp ('spinning ...')
        seconds_cmpt = seconds_cmpt+1; % add one second
        
        if rand < 0.1 
            availability = 1;
            avail_cmpt = avail_cmpt+1;
            available_time(avail_cmpt) = GetSecs() - slideOnT;
            disp ('available !')
        end
    end
    
    
    % baseline screen
    fractal_text  = Screen('MakeTexture',var.w, fractal);
    Screen('DrawTexture', var.w, fractal_text ,[], positionFractal );
    Screen('Close', fractal_text);
    Screen('FrameRect', var.w, var.allColors, allRectsV, 15);         % reponse grid
    Screen('FillRect', var.w, [250; 250; 0], position_response_cue);  % reponse cue
    Screen('Flip',var.w);
    
    [ keyPressed, keyCode]=KbQueueCheck(var.deviceIndex); 
    
    if keyPressed  
        
        cmpt = cmpt+1;
        pressed_all = pressed_all+1;
        
        Button {cmpt} = KbName(min(find(keyCode)));
        respKey = min(find(keyCode));
        RT_all (cmpt) = GetSecs - slideOnT;
        
        
        if (keyPressed == 1) && ismember(respKey, response) % Register the accuracy of the response
            pressed_correct = pressed_correct+1;
            ACC(cmpt) = 1;
            if pressed_correct == 1 % we log the RT of the first correct response
                RT = GetSecs - slideOnT;
            end
            
            % display feedback according the reward availability
            fractal_text   = Screen('MakeTexture',var.w, fractal); 
            Screen('DrawTexture', var.w, fractal_text ,[], positionFractal );
            Screen('Close', fractal_text);
            Screen('FrameRect', var.w, var.allColors, allRectsV,15);          % reponse grid
            Screen('FillRect', var.w, [250; 250; 0], position_response_cue);  % response cue
            
            if availability == 1
                reward_text   = Screen('MakeTexture',var.w, reward); 
                Screen('DrawTexture', var.w, reward_text ,[], positionReward );
                Screen('Close', reward_text);
                got_reward = got_reward + 1;
                Screen('Flip',var.w);
                WaitSecs(1);
                seconds_cmpt = seconds_cmpt+1; % update seconds counter to include the 1 sec pause to display reward
            else
                Screen('FillOval', var.w, [70; 70; 70], positionAction); % response feedback
                Screen('Flip',var.w);
                WaitSecs(0.100);
            end
         
        else
            ACC(cmpt) = 0;
        end
       
        availability = 0; % reset availability to 0
    
    end

end

KbQueueRelease(var.deviceIndex);
duration = GetSecs()-startT;
