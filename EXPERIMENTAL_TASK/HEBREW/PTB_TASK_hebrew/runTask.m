% runTasK
%__________________________________________________________________________
%--------------------------------------------------------------------------
%
% Free operant task with outcome devaluation procedure Tricomi et al., (2009)
% PTB 3.0.12 on matlab 2015b
%__________________________________________________________________________
%-------------------------------------------------------------------------
% last modified on AUGUST 2017 by Eva

% added country variable to load the snacks according to local preferences
% main code changed, 1 function changed (uploadImages), new images added

% session = different sessions collected on different days
% run = different runs run on the same days
% button to answer: -d- -f- -j- -k- -left arrow- -right arrow- and -space-
% Press space to start runs



%**************************************************************************
%       PTB INITIALIZATION/PARAMETER SETUP
%**************************************************************************
try
    
    clear all
    
    % add the function folder to the path just for this session
    path(path, 'functions');
    
    % define variables
    %---Rani
    %var.country = input('***input*** USA (=1) or AUSTRALIA (=2): ' ); %
    var.country = 3; % ISRAEL   
    
    var.real = 1;
    %var.real    = input('***input*** real experiment (=1) or testing (=0)?: '); % 1 = real experiment; 0 testing
    
    % get the response device index
    [id, names] = GetKeyboardIndices();
    %disp ('list of devices:' )
    %for i = 1:length(id)
    %    
    %    nameX = cellstr(names(i));
    %    idX   = id(i);
    %    disp (['ID number: ' idX  nameX])
    %    
    %end    
    var.deviceIndex = min(id);
    %var.deviceIndex = input('***input*** response device ID number (check list of devices above): ');
    %---
    
    % enter the task variables
    var.sub_ID = input('***input*** SUBJECT NUMBER: ');
    %---Rani (editted)
    if var.sub_ID > 100 && var.sub_ID < 200
        var.session = 1;
        var.training = 1;
    elseif var.sub_ID > 200 && var.sub_ID < 300
        var.session = input('***input*** SESSION NUMBER (1,2 or 3 session day): '); % 1,2,or 3 session
        var.training = 3;
    else % TESTING MODE
        var.real = 0; %testing mode
        fprintf('\n\n-- Subject number is not a valid subject number --\n--- This will run on DEBUGGING MODE ---\n\n\n')
        var.session = input('***input*** SESSION NUMBER (1,2 or 3 session day): '); % 1,2,or 3 session
        var.training = input('***input*** TRAINING SCHEDULE (1= 1-day group, 3= 3-day group): '); % 1 day or 3 days
    end
    %var.training = input('***input*** TRAINING SCHEDULE (1= 1-day group, 3= 3-day group): '); % 1 day or 3 days
    
    % If it's the first day - open the questionnaires
    if var.session == 1 && var.real == 1
        web('https://goo.gl/forms/AZMA5j09M4fQl7Al1', '-browser')
    end
    %---
    
    
    
    % check that task variable make sense
    var = inputCheck (var,1); % first check
    var = inputCheck (var,2); % second check (after variable have been adjusted)
    
    % enter the snack the participant prefers
    switch var.country
        
        case 1 % California
            var.salty= input('***input*** SWEET REWARD (1=CHOCOLATE, 2=chocorice, 3=skittles): ');
            var.sweet = input('***input*** SALTY REWARD (4=BEYGALE, 5=goldfish, 6=cheez-it): ');        
        case 2 % Australia
            var.salty= input('***input*** SWEET REWARD (1=M&M, 2=maltesers; 3=skittles): ');
            var.sweet = input('***input*** SALTY REWARD (4=cashew, 5=doritos; 6=chips): ');
        %---Rani
        case 3 % ISRAEL
            var.salty= input('***input*** SWEET REWARD (1=M&M, 2=click, 3=skittles): ');
            var.sweet = input('***input*** SALTY REWARD (4=cashew, 5=doritos, 6=tapuchips): ');
        %---
    end
    
    % initialize task parameters
    [var, data] = initTask(var);
    %-- Rani added the condition if it crashes...
    if exist(var.resultFile, 'file')
        save (var.resultFile, 'data', '-append');
    else
        save(var.resultFile,'data');
    end
    
    %**************************************************************************
    %        INITIAL HUNGER AND PLEASANTNESS RATINGS
    %**************************************************************************
    showInstruction(var,'instructions/ratings.txt')
    WaitSecs(0.4);
    while 1
        [~, ~, keycode] = KbCheck(-3,2);
        keyresp = find(keycode);
        if ismember (keyresp, [var.centerLeftKey, var.centerRightKey])
            break
        end
        
    end
    
    images           = {var.sweetImage, var.saltyImage};
    names            = {'sweet', 'savory'};
    questionX        = {var.sweetLabelHebrew; var.saltyLabelHebrew};
    
    % Randomize the image list
    randomIndex     = randperm(length(images));
    images          = images(randomIndex);
    names           = names (randomIndex);
    questionX       = questionX(randomIndex);
    
    for i = 1:length(images)
        
        if var.session == 1     %pleasantness ratings for snacks after tasting each snack (only in session 1)
            % --- by Rani
            %question = ['Please rate how pleasant the piece of ' char(questionX(i)) ' you just ate was'];
            question = fliplr([1491 1512 1490 47 1497 32 1489 1489 1511 1513 1492 32 1506 1491 32 1499 1502 1492 32 1492 1497 1514 1492 32 1502 1492 1504 1492 32 1506 1489 1493 1512 1498 32 1495 1514 1497 1499 1514 32 1492 questionX{i} 32 1513 1488 1499 1500 1514 32 1499 1512 1490 1506]);
        else                    %pleasantness ratings for snacks they haven't tasted before (session 2&3)
            %question = ['Please rate how pleasant you would find a piece of ' char(questionX(i)) ' right now'];
            question = fliplr([1491 1512 1490 47 1497 32 1489 1489 1511 1513 1492 32 1506 1491 32 1499 1502 1492 32 1492 1497 1514 1492 32 1502 1492 1504 1492 32 1506 1489 1493 1512 1498 32 1495 1514 1497 1499 1514 32 questionX{i} 32 1506 1499 1513 1497 1493]);
        end
        
        %data.initialRatings.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, 'very unpleasant', 'very pleasant');
        data.initialRatings.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, fliplr([1502 1488 1491 32 1500 1488 32 1502 1492 1504 1492]), fliplr([1502 1488 1491 32 1502 1492 1504 1492]));
            % ----

        Screen('TextStyle', var.w, 1);
        Screen('TextSize', var.w, 36);
        DrawFormattedText(var.w, '+', 'center', 'center', [0 0 0]);
        Screen('Flip', var.w);
        WaitSecs(1+rand(1,1));
        
    end
    
    % Rate Hunger level
    % --- Rani
    %question = 'Please rate your current hunger level';
    question = fliplr([1491 1512 1490 47 1497 32 1489 1489 1511 1513 1492 32 1488 1514 32 1512 1502 1514 32 1492 1512 1506 1489 32 1492 1504 1493 1499 1495 1497 1514 32 1513 1500 1498]);
    %data.initialRatings.hunger = likertScale(0, question, [1 2 3 4 5 6 7 8 9 10], var, 'very full', 'very hungry');
    data.initialRatings.hunger = likertScale(0, question, [1 2 3 4 5 6 7 8 9 10], var, fliplr([1513 1489 1506 47 1492 32 1502 1488 1491]), fliplr([1512 1506 1489 47 1492 32 1502 1488 1491]));
    % ----
    
    Screen('TextStyle', var.w, 1);
    Screen('TextSize', var.w, 36);
    DrawFormattedText(var.w, '+', 'center', 'center', [0 0 0]);
    Screen('Flip', var.w);
    WaitSecs(1+rand(1,1));
    
    save (var.resultFile, 'data', '-append');
    
    
    %**************************************************************************
    %                ACTIVE TASK INSTRUCTIONS AND PRACTICE                    %
    %**************************************************************************
    if var.real == 1 %
        
        if var.session == 1 % we want this only for the first session
            
            %**********************************************************************
            % active instructions
            
            var.instructionSpeed = 2; % time in s for the slides that do not wait for a participant response
            activeInstruction(var);
            
            %**********************************************************************
            % practice blocK
            
            showInstruction(var, 'instructions/practice.txt')
            WaitSecs(0.4);
            KbWait(-3,2);
            
            % initialize just as time references for practice
            var.time_MRI = GetSecs();
            var.ref_end = 0;
            
            condition = [  1  2  0];% 1 = sweet 2 = salty; 0 = rest
            duration  = [ 20 20 20];
            [var.condition, var.duration] = loadRandList(condition, duration);
            
            for ii = 1:3 % 3 blocks for practice
                
                % show block
                var.ref_end = var.ref_end + var.duration(ii); % 20 or 40 s
                drawnActiveScreen (var,ii);
                % we do not save data of the practice blocks
                
            end
            
            showInstruction(var, 'instructions/question.txt')
            
            %-- Rani
            WaitSecs(0.4);
            while 1
                [~, ~, keycode] = KbCheck(-3,2);
                keyresp = find(keycode);
                if ismember (keyresp, var.mySafetyControl)
                    break
                end
                
            end
            %--
            
            WaitSecs(0.4);
            KbWait(-3,2);
            
            
        end
        
    end
    %**************************************************************************
    %                      FREE OPERANT TRAINING                               %
    %**************************************************************************
    trial = 0;
    %---Rani
    CumulatedWonSweet = 0;
    CumulatedWonSalty = 0;
    ThisBlockSweetWinnings = zeros(1,var.runs);
    ThisBlockSaltyWinnings = zeros(1,var.runs);
    %---

    for i = 1:var.runs
        
        
        %%%%%%%%%%%%%%%% sync procedure and time initialization %%%%%%%%%%%%%%%%%%%
        
        RestrictKeysForKbCheck([]); % allow all butttons as inputs
        if i ==1
            showInstruction(var,'instructions/wait.txt'); % experiment about to start
        else
            showInstruction(var,'instructions/newRun.txt'); % next run is about to start
        end
        
        triggerscanner = 0;
        WaitSecs(0.4);
        while ~triggerscanner
            [down, secs, keycode, d] = KbCheck(-3,2);
            keyresp = find(keycode);
            if (down == 1)
                if ismember (keyresp, var.pulseKeyCode)
                    triggerscanner = 1;
                end
            end
            WaitSecs(.01);
        end
        
        % once the task is on we just check the task relevant button to avoid any interference
        RestrictKeysForKbCheck([var.leftKey, var.rightKey, var.centerLeftKey, var.centerRightKey, var.mycontrol, var.mySafetyControl]);
        
        var.time_MRI = GetSecs(); % absolute reference of the experiment beginning
        var.ref_end = 0;
        
        %%%%%%%%%%%%%%%% lead in screen for 2 s (is 2 s) %%%%%%%%%%%%%%%%%%%%%%
        var.ref_end = var.ref_end + 2;
        data.training.onsets.leadIn = GetSecs -var.time_MRI;
        data.training.durations.leadIn = displayITI(var);
        
        
        % randomize list for the run
        % each learning run has 12 task blocks and eight rest blocks.
        condition = [1  1  1  1  1  1  2  2  2  2  2  2  0  0  0  0  0  0  0  0 ]; % 1 = sweet 2 = salty; 0 = rest
        duration  = [40 40 20 20 20 20 40 40 20 20 20 20 20 20 20 20 20 20 20 20];% the duration of each block is 20s for rest and 20 or 40 for active blocks
        [var.condition, var.duration] = loadRandList(condition, duration);
        
        for ii = 1:length(var.condition)
            
            trial = trial+1;
            
            % show block
            var.ref_end = var.ref_end + var.duration(ii); % 20 or 40 s
            data.training.onsets.block(trial) = GetSecs - var.time_MRI; % get onset
            
            [RT, pressed_correct, pressed_all,...
                ACC, RT_all, Button,...
                reward, potential_rewards, potential_rewards_time,...
                duration] = drawnActiveScreen (var,ii);
            
            % log data
            data.training.stPressRT(trial)           = RT;
            data.training.raw_press(trial)           = pressed_correct;
            data.training.pressFreq(trial)           = pressed_correct/duration; % press per second
            data.training.raw_all_press(trial)       = pressed_all;
            data.training.all_pressFreq(trial)       = pressed_correct/duration; % press per second
            data.training.reward(trial)              = reward;
            data.training.blockDetails(trial).ACC    = ACC;
            data.training.blockDetails(trial).RT     = RT_all;
            data.training.blockDetails(trial).button = Button;
            data.training.blockDetails(trial).potential_rewards = potential_rewards;
            data.training.blockDetails(trial).potential_rewards_time = potential_rewards_time;
            data.training.durations.blocks(trial)    = duration;
            
            data.training.condition(trial)           = var.condition(ii);
            data.training.block    (trial)           = ii;
            data.training.run      (trial)           = i;
            data.training.session  (trial)           = var.session;
            data.training.subID    (trial)           = var.sub_ID;
            
            if var.condition(ii) == var.devalued
                data.training.value {trial}         = 'devalued';
            elseif var.condition(ii) == 0;
                data.training.value {trial}         = 'baseline';
            else
                data.training.value {trial}         = 'valued';
            end
            
            % save at the end of each active block
            save(var.resultFile, 'data', '-append');
            
        end
        %**************************************************************************
        %             PERFORMANCE FEEDBACK AND REWARD DELIVERY                    %
        %**************************************************************************
        
        %---Rani
        ThisBlockSweetWinnings(i) = sum(data.training.reward(data.training.condition == 1))/2 - CumulatedWonSweet;
        ThisBlockSaltyWinnings(i) = sum(data.training.reward(data.training.condition == 2))/2 - CumulatedWonSalty;
        
        
        % --- Rani
        %message = ['This session you won: ' num2str(won_sweet) ' ' var.sweetLabel ' and ' num2str(won_salty) ' ' var.saltyLabel ' ' '(Please inform the investigator).' ];
        message = fliplr([41 1489 1489 1511 1513 1492 32 1506 1491 1499 1504 47 1497 32 1488 1514 32 1492 1504 1505 1497 1497 1503 40 10 1489 1495 1500 1511 32 1494 1492 32 1494 1499 1497 1514 32 1489 58 32 fliplr(double(num2str(ThisBlockSweetWinnings(i)))) 32 var.sweetLabelHebrew 32 1493 45 fliplr(double(num2str(ThisBlockSaltyWinnings(i)))) 32 var.saltyLabelHebrew 46]);
        %
        
        % Screen settings
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, var.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % Print the instruction on the screen
        DrawFormattedText(var.w, message, 'center', 'center', [0 0 0], 60);
        Screen(var.w, 'Flip');
        WaitSecs(0.4);
        while 1
            [~, ~, keycode] = KbCheck(-3,2);
            keyresp = find(keycode);
            if ismember (keyresp, var.mySafetyControl)
                break
            end
            
        end
        
        %----Rani
        CumulatedWonSweet = sum(data.training.reward(data.training.condition == 1))/2;
        CumulatedWonSalty = sum(data.training.reward(data.training.condition == 2))/2;
        
    end
    
    won_sweet     = sum(data.training.reward(data.training.condition == 1))/2;
    won_salty     = sum(data.training.reward(data.training.condition == 2))/2;
    
    %**************************************************************************
    %                    STIM_OUTCOME CONTIGENCY TEST                         %
    %**************************************************************************
    
    if var.session == 1 % first day only
        
        % test of knowledge of stimulus-outcome associations
        showInstruction(var,'instructions/ratings.txt')
        WaitSecs(0.4);
        while 1
            [~, ~, keycode] = KbCheck(-3,2);
            keyresp = find(keycode);
            if ismember (keyresp, [var.centerLeftKey, var.centerRightKey])
                break
            end
            
        end
        
        % prepare the images
        images           = {var.sweet_fractal, var.salty_fractal, var.rest_fractal};
        
        % define names by condition
        names            = {'sweet'; 'salty'; 'baseline'};
        condition        = [      1;       2;         0 ];
        
        % Randomize the image list
        randomIndex      = randperm(length(images));
        images           = images(randomIndex);
        names            = names (randomIndex);
        condition        = condition(randomIndex);
        
        for i = 1:length(images)
            
            %--- Rani
            %question = ['When the fractal below was shown, was it more likely that a button press would result in a ' var.sweetLabel ' or a ' var.saltyLabel ' reward?'];
            question = fliplr([1500 1490 1502 1493 1500 32 1513 1500 32 var.sweetLabelHebrew 32 1488 1493 32 1513 1500 32 var.saltyLabelHebrew 63 10 1499 1488 1513 1512 32 1492 1514 1502 1493 1504 1492 32 1500 1502 1496 1492 32 1492 1493 1508 1497 1506 1492 44 32 1492 1488 1501 32 1492 1497 1492 32 1505 1489 1497 1512 32 1497 1493 1514 1512 32 1513 1500 1495 1497 1510 1514 32 1499 1508 1514 1493 1512 32 1514 1493 1489 1497 1500]);
            
                %data.contingencyTest.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, [var.sweetLabel ' more likely'], [var.saltyLabel  ' more likely']);
                data.contingencyTest.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, fliplr([var.sweetLabelHebrew 32 1505 1489 1497 1512 32 1497 1493 1514 1512]), fliplr([var.saltyLabelHebrew 32 1505 1489 1497 1512 32 1497 1493 1514 1512]));
            %---
                
            % recode the rating so that the higher = the more accurate for both
            % food related-fractals
            if condition(i) == 1
                data.contingencyTest.(names{i}) = data.contingencyTest.(names{i}) * -1;
            end
            
            Screen('TextStyle', var.w, 1);
            Screen('TextSize', var.w, 36);
            DrawFormattedText(var.w, '+', 'center', 'center', [0 0 0]);
            Screen('Flip', var.w);
            WaitSecs(1+rand(1,1));
            
        end
        
        
    end
    
    
    %**************************************************************************
    %                        DEVALULATION PROCEDURE                           %
    %**************************************************************************
    if var.training == 1 || (var.training ==3 && var.session == 3)% only if it's the last training session for the experimental group
        
        disp ('Devaluation procedure is about to start...')
        beep; % beeping because it requires the experimenter to be active
        
        
        % show instruction for the devaluation procedure
        showBonus(var);
        WaitSecs(0.4);
        while 1
            [~, ~, keycode] = KbCheck(-3,2);
            keyresp = find(keycode);
            if ismember (keyresp, var.mySafetyControl)
                break
            end
            
        end
        
        % ratings after devaluation
        showInstruction(var,'instructions/ratings.txt')
        WaitSecs(0.4);
        while 1
            [~, ~, keycode] = KbCheck(-3,2);
            keyresp = find(keycode);
            if ismember (keyresp, [var.centerLeftKey, var.centerRightKey])
                break
            end
            
        end
        
        images           = {var.sweetImage, var.saltyImage};
        names            = {'sweet', 'salty'};
        questionX        = {var.sweetLabelHebrew; var.saltyLabelHebrew};
        
        % Randomize the image list
        randomIndex      = randperm(length(images));
        images           = images(randomIndex);
        names            = names (randomIndex);
        questionX        = questionX(randomIndex);
        
        for i = 1:length(images)
            
            %--- Rani
            %question = ['Please rate how pleasant you would find a piece of ' char(questionX(i)) ' right now'];
            question = fliplr([1491 1512 1490 47 1497 32 1489 1489 1511 1513 1492 32 1506 1491 32 1499 1502 1492 32 1514 1492 1497 1492 32 1502 1492 1504 1492 32 1506 1489 1493 1512 1498 32 1495 1514 1497 1499 1514 32 questionX{i} 32 1506 1499 1513 1497 1493]);

            %data.finalRatings.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, 'very unpleasant', 'very pleasant');
            data.finalRatings.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, fliplr([1502 1488 1491 32 1500 1488 32 1502 1492 1504 1492]), fliplr([1502 1488 1491 32 1502 1492 1504 1492]));
            %---
            
            Screen('TextStyle', var.w, 1);
            Screen('TextSize', var.w, 36);
            DrawFormattedText(var.w, '+', 'center', 'center', [0 0 0]);
            Screen('Flip', var.w);
            WaitSecs(1+rand(1,1));
            
        end
        
        % Rate Hunger level
       
        % --- Rani
        %question = 'Please rate your current hunger level';
        question = fliplr([1491 1512 1490 47 1497 32 1489 1489 1511 1513 1492 32 1488 1514 32 1512 1502 1514 32 1492 1512 1506 1489 32 1492 1504 1493 1499 1495 1497 1514 32 1513 1500 1498]);  
        %data.finalRatings.hunger = likertScale(0, question, [1 2 3 4 5 6 7 8 9 10], var, 'very full', 'very hungry');
        data.finalRatings.hunger = likertScale(0, question, [1 2 3 4 5 6 7 8 9 10], var, fliplr([1513 1489 1506 47 1492 32 1502 1488 1491]), fliplr([1512 1506 1489 47 1492 32 1502 1488 1491]));
        % ----

        
        Screen('TextStyle', var.w, 1);
        Screen('TextSize', var.w, 36);
        DrawFormattedText(var.w, '+', 'center', 'center', [0 0 0]);
        Screen('Flip', var.w);
        WaitSecs(1+rand(1,1));
        
        save(var.resultFile, 'data', '-append');
        
    end
    
    %**************************************************************************
    %                      RATING OF FRACTAL IMAGES                           %
    %**************************************************************************
    
    % pleasantness ratings of the fractal
    
    if var.training == 1 || (var.training ==3 && var.session == 3)% only if it's the last training session for the experimental group
        
        showInstruction(var,'instructions/ratings.txt')
        WaitSecs(0.4);
        while 1
            [~, ~, keycode] = KbCheck(-3,2);
            keyresp = find(keycode);
            if ismember (keyresp, [var.centerLeftKey, var.centerRightKey])
                break
            end
            
        end
        
        % prepare the images
        images           = {var.sweet_fractal, var.salty_fractal, var.rest_fractal};
        
        % define names by condition
        names            = {'sweet'; 'salty'; 'baseline'};
        %condition        = [    1;         2;         0 ];
        
        % Randomize the image list
        randomIndex     = randperm(length(images));
        images          = images(randomIndex);
        names           = names (randomIndex);
        
        for i = 1:length(images)
            
            %--- Rani
            %question = 'Please rate the pleasantness of the fractal below';
            question = fliplr([1491 1512 1490 47 1497 32 1489 1489 1511 1513 1492 32 1499 1502 1492 32 1504 1506 1497 1502 1492 32 1506 1489 1493 1512 1498 32 1492 1514 1502 1493 1504 1492 32 1500 1502 1496 1492]);
            
            %data.fractalLiking.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, 'very unpleasant', 'very pleasant');
            data.fractalLiking.(names{i}) = likertScale(images{i}, question, [-5 -4 -3 -2 -1 0 1 2 3 4 5], var, fliplr([1502 1488 1491 32 1500 1488 32 1504 1506 1497 1502 1492]), fliplr([1502 1488 1491 32 1504 1506 1497 1502 1492]));
            %---
            
            Screen('TextStyle', var.w, 1);
            Screen('TextSize', var.w, 36);
            DrawFormattedText(var.w, '+', 'center', 'center', [0 0 0]);
            Screen('Flip', var.w);
            WaitSecs(1+rand(1,1));
            
        end
        
        
    end
    
    
    %**************************************************************************
    %                           EXTINCTIION TEST                              %
    %**************************************************************************
    
    if var.training == 1 || (var.training ==3 && var.session == 3) % only if it's the last training session for the experimental group
        disp ('Extinction procedure is about to start...')
        
        %%%%%%%%%%%%%%%% sync procedure and time initialization %%%%%%%%%%%%%%%%%%%
        RestrictKeysForKbCheck([]); % re-allow all keys to be read as inputs
        
        showInstruction(var,'instructions/newRun.txt');
        triggerscanner = 0;
        WaitSecs(0.4);
        while ~triggerscanner
            [down, secs, keycode, d] = KbCheck(-3,2);
            keyresp = find(keycode);
            if (down == 1)
                if ismember (keyresp, var.pulseKeyCode)
                    triggerscanner = 1;
                end
            end
            WaitSecs(.01);
        end
        
        % once the task is on we just check the task relevant button to avoid any interference
        RestrictKeysForKbCheck([var.leftKey, var.rightKey, var.centerLeftKey, var.centerRightKey, var.mycontrol, var.mySafetyControl]);
        
        var.time_MRI = GetSecs(); % absolute reference of the experiment beginning
        var.ref_end = 0;
        
        
        % randomize list for the run
        % the extinction run has 9 task blocks and 3 rest blocks.
        condition = [1  1  1  2  2  2  0  0  0 ]; % 1 = sweet 2 = salty; 0 = rest
        duration  = [20 20 20 20 20 20 20 20 20];% the duration of each block is 20s
        [var.condition, var.duration] = loadRandList(condition, duration);
        
        for trial = 1:length(var.condition)
            
            % show block
            var.ref_end = var.ref_end + var.duration(trial); % 20 or 40 s
            data.extinction.onsets.block(trial) = GetSecs - var.time_MRI; % get onset
            
            [RT, pressed_correct, pressed_all,...
                ACC, RT_all, Button, duration] = drawnExtinctionScreen (var,trial);
            
            % log data
            data.extinction.stPressRT(trial)           = RT;
            data.extinction.raw_press(trial)           = pressed_correct;
            data.extinction.pressFreq(trial)           = pressed_correct/duration; % press per second
            data.extinction.raw_all_press(trial)       = pressed_all;
            data.extinction.all_pressFreq(trial)       = pressed_correct/duration; % press per second
            data.extinction.blockDetails(trial).ACC    = ACC;
            data.extinction.blockDetails(trial).RT     = RT_all;
            data.extinction.blockDetails(trial).button = Button;
            data.extinction.durations.blocks(trial)    = duration;
            
            data.extinction.condition(trial)           = var.condition(trial);
            data.extinction.block    (trial)           = trial;
            data.extinction.run      (trial)           = i;
            data.extinction.session  (trial)           = var.session;
            data.extinction.subID    (trial)           = var.sub_ID;
            
            if var.condition(trial) == var.devalued
                data.extinction.value {trial}          = 'devalued';
            elseif var.condition(trial) == 0;
                data.extinction.value {trial}          = 'baseline';
            else
                data.extinction.value {trial}          = 'valued';
            end
            
            % save at the end of each extiction block
            save(var.resultFile, 'data', '-append');
            
        end
        
    end
        
    %**************************************************************************
    %                           END EXPERIMENT                                %
    %**************************************************************************
    
    data = endTask(var, data);
    
    %---Rani
    CopyOutputToDropbox2(var.resultFile, pwd)
    % copy outputfile
    
catch %#ok<*CTCH>
    % This "catch" section executes in case of an error in the "try"
    % section []
    % above.  Importantly, it closes the onscreen window if it's open.
    Screen('CloseAll');
    fclose('all');
    psychrethrow(psychlasterror);
end
