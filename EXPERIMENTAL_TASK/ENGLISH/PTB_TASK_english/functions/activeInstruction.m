function [] =  activeInstruction (var)


[instrDisp,text, position] = initInstructions(var);
Screen('TextFont', var.w, 'Arial');
Screen('TextSize', var.w, instrDisp.scaledSize);
Screen('TextStyle', var.w, 1);

% list of instructions to show
instructions = 1:23;
% init the current instruction
currentInst = 1;

% loop until done signal
doneInst = false;
while ~doneInst
    
    % run the instruction function
    RestrictKeysForKbCheck([]);
    feval(['Instructions_MP_' num2str(currentInst)], instrDisp, text, position, var);
    
    % wait for navigation input
    RestrictKeysForKbCheck( [instrDisp.instKeyPrev, instrDisp.instKeyNext, instrDisp.instKeyDone] );
    [~, keyCode] = KbWait(-3, 2);
    
    % update the current instructin according to key press
    respKey = find(keyCode);
    if ismember(respKey, instrDisp.instKeyPrev)
        % move back a screen
        currentInst = max(1, currentInst-1);
    elseif ismember(respKey, instrDisp.instKeyNext)
        % move forward
        currentInst = min(length(instructions), currentInst+1);
    elseif ismember(respKey,instrDisp.instKeyDone) & currentInst == length(instructions)
        doneInst = true;
    end
end

RestrictKeysForKbCheck([]);

end
%**************************************************************************
%                        AUXILIARY FUNCTIONS                              %
%**************************************************************************

    function [] = Instructions_MP_1  (instrDisp, text, position, var)
        % Welcome Screen
        showInstruction(var, 'instructions/activeInstruction1.txt')
        
    end

    function [] = Instructions_MP_2  (instrDisp, text, position, var)
        % Explain baseline screen
        
        % format text
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % baseline screen
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal);% draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        DrawFormattedText(var.w, text.basicScreen , 'center', var.yLowerLow, [0 0 0], 60);% explain text
        Screen('Flip',var.w);
        
    end

    function [] = Instructions_MP_3  (instrDisp, text, position, var)
        % point at fractal
        
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal );% draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        head   = [var.xCenter, var.yLower];        % coordinates of head
        points =  [ head+[instrDisp.triangle,0]    % left corner
            head-[instrDisp.triangle,0]                      % right corner
            head-[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        DrawFormattedText(var.w, text.aFractal, 'center', var.yLowerLow, [0 0 0], 60);% explain text
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_4  (instrDisp, text, position, var)
        
        % point at response grid
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal );% draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        head   = [var.xCenter, var.yUpperHigh]; % coordinates of head
        points =  [ head-[instrDisp.triangle,0]              % left corner
            head+[instrDisp.triangle,0]                      % right corner
            head+[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        
        DrawFormattedText(var.w, text.yellowBoxes, 'center', var.yLowerLow, [0 0 0], 60);% explain text
        Screen('Flip',var.w);
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_5  (instrDisp, text, position, var)
        % point at yellow buttons
        
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % point at response grid
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal); % make fractal texture
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        head   = [var.xCenter, var.yUpperHigh]; % coordinates of head
        points =  [ head-[instrDisp.triangle,0]              % left corner
            head+[instrDisp.triangle,0]                      % right corner
            head+[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        DrawFormattedText(var.w, text.buttonWorks , 'center', var.yLowerLow, [0 0 0], 60);
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_6  (instrDisp, text, position, var)
        
        % Focus on the yellow box only
        
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        DrawFormattedText(var.w, text.eachBox, 'center', 'center', [0 0 0], 60);
        Screen('Flip',var.w);
        
    end

    function [] = Instructions_MP_7  (instrDisp, text, position, var)
        
        % animation of yellow buttons
        
        squares   = [var.square1, var.square2, var.square3, var.square4];
        responses = [var.leftKey; var.centerLeftKey; var.centerRightKey; var.rightKey];
        
        for ii = 1: length(squares)
            
            squareX = squares(ii);
            buttonX = KbName(responses(ii,1));
            
            position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],squareX, var.yUpper);
            Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
            Screen('FillRect', var.w, [250; 250; 0], position_response_cue);  % response cue
            message = ['This box corresponds to the button: \n' '  ' buttonX '   ']; % this needs to be modified to have it txt file with buttonX as wildcard
            DrawFormattedText(var.w, message, 'center', 'center', [0 0 0], 60,false, false, 1.2);
            Screen('Flip',var.w);
            WaitSecs (var.instructionSpeed);
            
        end
        
       DrawFormattedText(var.w, 'right arrow next, left arrow repeat one more time ', 'center', 'center', [0 0 0], 60); % this need to be put in a txt file with button X as wildcard
       Screen('Flip',var.w);
        
    end

    function [] = Instructions_MP_8  (instrDisp, text, position, var)
        % practice the buttons
        
        % re-allow the button
        RestrictKeysForKbCheck([]);
        
        % format text
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % General
        DrawFormattedText(var.w, text.letsPractice, 'center', 'center', [0 0 0], 60);
        Screen('Flip',var.w);
        WaitSecs (2);
        
        % Button Specific
        squares   = [var.square1, var.square2, var.square3, var.square4];
        responses = [var.leftKey; var.centerLeftKey; var.centerRightKey; var.rightKey];
        
        % prepare queque
        KbQueueCreate(var.deviceIndex);
        KbQueueStart(var.deviceIndex);
        
        for ii = 1: length(squares)
            
            squareX = squares(ii);
            buttonX = KbName(responses(ii,1));
            codeX   = responses(ii,1);
            correctPressed = 0;
            
            while ~correctPressed
                position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],squareX, var.yUpper);
                Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
                Screen('FillRect', var.w, [250; 250; 0], position_response_cue);   % response cue
                DrawFormattedText(var.w, text.pleasePress, 'center', 'center', [0 0 0], 60);
                Screen('Flip',var.w);
                
                [keyPressed, Code]=KbQueueCheck(var.deviceIndex);
                respKey = min(find(Code));
                
                if (keyPressed == 1) && ismember(respKey, codeX)
                    
                    position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],squareX, var.yUpper);
                    Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
                    Screen('FillRect', var.w, [250; 250; 0], position_response_cue);   % response cue
                    
                    DrawFormattedText(var.w, 'Correct ', 'center', 'center', [0 120 0], 60);
                    Screen('Flip',var.w);
                    WaitSecs(2);
                    correctPressed = 1;
                    
                elseif (keyPressed == 1) && ~ismember(respKey, codeX)
                    
                    position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],squareX, var.yUpper);
                    Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
                    Screen('FillRect', var.w, [250; 250; 0], position_response_cue);   % response cue
                    
                    DrawFormattedText(var.w, ['Incorrect. The right response was \n' '   ' num2str(buttonX) '   ' '\n' 'let us retry'], 'center', 'center', [200 0 0], 60, false, false, 1.2); % this need to be put in a txt file with button X as wildcard
                    Screen('Flip',var.w);
                    WaitSecs(2);
                    correctPressed = 0;
                    
                end
            end
            
            
        end
        
        DrawFormattedText(var.w, 'Good job! \n (right arrow next) ', 'center', 'center', [0 0 0], 60); % this need to be put in a txt file with button X as wildcard
        Screen('Flip',var.w);
        
        KbQueueRelease(var.deviceIndex); % relase queque
        RestrictKeysForKbCheck( [instrDisp.instKeyPrev, instrDisp.instKeyNext, instrDisp.instKeyDone] );

        
    end

    function [] = Instructions_MP_9  (instrDisp, text, position, var)
        % say there are to kind of blocks
        
        showInstruction(var, 'instructions/general2.txt')
        
    end

    function [] = Instructions_MP_10 (instrDisp, text, position, var)
        % explain respond: light is on
        
        % format test
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % define lit box position
        position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],var.square1, var.yUpper);
        
        % make images texture
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        
        %point at lit square
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        Screen('FillRect', var.w, var.allColors(:,1),   position_response_cue, instrDisp.gridThick); % reponse grid
        head   = [position_response_cue(1), var.yUpperHigh];    % coordinates of head
        points = [ head-[instrDisp.triangle,0]              % left corner
            head+[instrDisp.triangle,0]                      % right corner
            head+[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        
        DrawFormattedText(var.w, text.duringRespond, 'center', var.yLowerLow, [0 0 0], 60);
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_11 (instrDisp, text, position, var)
        % explain respond feedback: gray circle if right press
        
        % make images texture
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],var.square1, var.yUpper);

        % gray circle
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        Screen('FillRect', var.w, var.allColors(:,1), position_response_cue, instrDisp.gridThick); % lit
        Screen('FillOval', var.w, [70; 70; 70], position.Action);
        head   = [var.xCenter, var.yLowerLow*0.97];  % coordinates of head                        
        points =  [ head+[instrDisp.triangle,0]              % left corner
            head-[instrDisp.triangle,0]                      % right corner
            head-[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        DrawFormattedText(var.w, text.circleAppear, 'center', var.yLowerLow, [0 0 0], 60);
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_12 (instrDisp, text, position, var)
        % explain respond feedback: no gray circle if right press
        
        % make images texture
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],var.square1, var.yUpper);
        
        % no feeedback if wrong button
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        Screen('FillRect', var.w, var.allColors(:,1),   position_response_cue, instrDisp.gridThick); % lit
        DrawFormattedText(var.w, text.wrongPress, 'center', var.yLowerLow, [0 0 0], 60);
        head   = [var.xCenter, var.yLowerLow*0.97];  % coordinates of head                         % width of arrow head
        points =  [ head+[instrDisp.triangle,0]              % left corner
            head-[instrDisp.triangle,0]                      % right corner
            head-[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_13 (instrDisp, text, position, var)
        % explain respond feedback: food picture
        
        % make images texture
        fractal_text = Screen('MakeTexture',var.w, instrDisp.fractal);
        sweet_text   = Screen('MakeTexture',var.w, var.sweetImage);
        salty_text   = Screen('MakeTexture',var.w, var.saltyImage);
        position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],var.square1, var.yUpper);
        
        % reward sweet
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        Screen('FillRect', var.w, var.allColors(:,1),   position_response_cue, instrDisp.gridThick); % lit
        Screen('DrawTexture', var.w, sweet_text ,[], position.Reward ); % sweet picture
        DrawFormattedText(var.w, text.sometimesReward, 'center', var.yLowerLow, [0 0 0], 60); % explain text
        Screen('Flip',var.w);
        WaitSecs(var.instructionSpeed);
        
        % salty picture
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        Screen('FillRect', var.w, var.allColors(:,1),   position_response_cue, instrDisp.gridThick); % lit
        Screen('DrawTexture', var.w, salty_text, [], position.Reward ); % salty picture
        DrawFormattedText(var.w, text.sometimesReward , 'center', var.yLowerLow, [0 0 0], 60,false, false, 1.2); % explain text
        Screen('Flip',var.w);
        
        Screen('Close', sweet_text);
        Screen('Close', salty_text);
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_14 (instrDisp, text, position, var)
        % explain respond feedback: food picture
        
        % make images texture
        fractal_text = Screen('MakeTexture',var.w, instrDisp.fractal);
        sweet_text   = Screen('MakeTexture',var.w, var.sweetImage);
        salty_text   = Screen('MakeTexture',var.w, var.saltyImage);
        
        % what do picture means
        Screen('DrawTexture', var.w, sweet_text ,[], position.Reward ); % sweet picture
        DrawFormattedText(var.w, text.pictureMeans , 'center', 'center', [0 0 0], 60); % explain text
        Screen('Flip',var.w);
        
        Screen('Close', sweet_text);
        Screen('Close', salty_text);
        Screen('Close', fractal_text);
        
        
    end

    function [] = Instructions_MP_15 (instrDisp, text, position, var)
        % you will eat what you see
        showInstruction(var, 'instructions/general4.txt')
        
    end

    function [] = Instructions_MP_16 (instrDisp, text, position, var)
        % you can press as much as you want
        
        showInstruction(var, 'instructions/general3.txt')
        
    end

    function [] = Instructions_MP_17 (instrDisp, text, position, var)
        % explain rest : no light is on
        
        % format text
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % point at non active grid
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        head   = [var.xCenter, var.yUpperHigh]; % coordinates of head
        points =  [ head-[instrDisp.triangle,0]              % left corner
            head+[instrDisp.triangle,0]                      % right corner
            head+[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        
        DrawFormattedText(var.w, text.duringRest, 'center', var.yLowerLow, [0 0 0], 60,false, false, 1.2); % explain text
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_18 (instrDisp, text, position, var)
        % explain rest : no need to press
        
        % format text
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % rest screen
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
        
        DrawFormattedText(var.w, text.relax, 'center', var.yLowerLow, [0 0 0], 60, false, false, 1.2); % explain text
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_19 (instrDisp, text, position, var)
        % test is it a rest or a respond block?
        
        RestrictKeysForKbCheck([]);

        % format text
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % make texture
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],var.square1, var.yUpper);
        
        % message
        text.restoractive = ['if you think this is a REST period press   ' KbName(var.rightKey(1)) '  ' '\n' 'if you think this is a RESPOND period press  ' KbName(var.leftKey(1)) ' '];
        
        % prepare queque
        KbQueueCreate(var.deviceIndex);
        KbQueueStart(var.deviceIndex);
        
        % lists
        condition = [1; 2]; % 1 = ACTIVE 2 = REST
        conName   = {'RESPOND'; 'REST'};
        responses = [var.leftKey; var.rightKey];
        
        for ii = 1:length(condition)
            
            % get the specific of condition
            buttonX  = KbName(responses(ii,1));
            codeX    = responses(ii,1);
            conNameX = char(conName(ii));
            
            correctPressed = 0;
            
            while ~correctPressed
                
                % Set QUESTION SCREEN
                Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
                Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
                
                if condition(ii) == 1
                    Screen('FillRect', var.w, var.allColors(:,1),   position_response_cue, instrDisp.gridThick); % lit of of the bo
                end
                
                DrawFormattedText(var.w, text.restoractive, 'center', var.yLowerLow, [0 0 0], 60); % explain text
                Screen('Flip',var.w);
                
                [keyPressed, Code]=KbQueueCheck(var.deviceIndex);
                respKey = min(find(Code));
                
                if (keyPressed == 1) && ismember(respKey, codeX) % correct
                    
                    Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
                    Screen('FrameRect', var.w, var.allColors, position.allRectsV, instrDisp.gridThick); % reponse grid
                    
                    if condition(ii) == 1
                        Screen('FillRect', var.w, var.allColors(:,1), position_response_cue, instrDisp.gridThick) % lit of of the box
                    end
                    
                    DrawFormattedText(var.w, 'Correct ', 'center', var.yLowerLow, [0 120 0], 60); % explain text
                    Screen('Flip',var.w);
                    
                    WaitSecs(2);
                    correctPressed = 1;
                    
                elseif (keyPressed == 1) && ~ismember(respKey, codeX)
                    
                    Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal ); % draw fractal
                    Screen('FrameRect', var.w, var.allColors, position.allRectsV, 15); % reponse grid
                    
                    if condition(ii) == 1
                        Screen('FillRect', var.w, var.allColors(:,1), position_response_cue, instrDisp.gridThick) % lit of of the box
                    end
                    
                    DrawFormattedText(var.w, ['Incorrect. This is a ' '  ' conNameX ' period   ' '\n' 'Let us retry'], 'center', var.yLowerLow, [200 0 0], 60,false, false, 1.2); % explain text
                    Screen('Flip',var.w);
                    
                    WaitSecs(2);
                    correctPressed = 0;
                    
                end
                
            end
            
        end
        
        
        DrawFormattedText(var.w, ' Good job! \n (right arrow next)  ', 'center', 'center', [0 0 0], 60); % this need to be put in a txt file with button X as wildcard
        Screen('Flip',var.w);
        
        RestrictKeysForKbCheck( [instrDisp.instKeyPrev, instrDisp.instKeyNext, instrDisp.instKeyDone] );
        KbQueueRelease(var.deviceIndex); % relase queque

    end

    function [] = Instructions_MP_20 (instrDisp, text, position, var)
        % what does the fractal mean: there will be different fractals
        
        % format text
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, instrDisp.scaledSize);
        Screen('TextStyle', var.w, 1);
        
        % define box lit position
        position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],var.square1, var.yUpper);
        
        % explain there will be different fractals
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal); % draw fractal
        DrawFormattedText(var.w, text.multipleFractal, 'center', var.yUpper, [0 0 0], 60); % explain text
        head   = [var.xCenter, var.yLower];                  % coordinates of head                        
        points =  [ head+[instrDisp.triangle,0]              % left corner
            head-[instrDisp.triangle,0]                      % right corner
            head-[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_21 (instrDisp, text, position, var)
        
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        
        % each fractal an outcome and an action
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal );
        DrawFormattedText(var.w, text.Fractalmeans , 'center', var.yUpper, [0 0 0], 60,false, false, 1.2);
        Screen('Flip',var.w);
        
        Screen('Close', fractal_text);
        
    end

    function [] = Instructions_MP_22 (instrDisp, text, position, var)
        
        % get image
        reward_text  = Screen('MakeTexture',var.w, var.sweetImage);
        fractal_text  = Screen('MakeTexture',var.w, instrDisp.fractal);
        position_response_cue = CenterRectOnPointd([0 0 var.CUEdim var.CUEdim],var.square1, var.yUpper);
 
        % illustrate
        Screen('DrawTexture', var.w, fractal_text ,[], position.Fractal); % draw fractal
        Screen('FrameRect', var.w, var.allColors, position.allRectsV, 15); % reponse grid
        Screen('FillRect', var.w, var.allColors(:,1),  position_response_cue, instrDisp.gridThick); % lit
        Screen('DrawTexture', var.w, reward_text ,[], position.Reward );
        head   = [position_response_cue(1), var.yUpperHigh]; % coordinates of head
        points =  [ head-[instrDisp.triangle,0]              % left corner
            head+[instrDisp.triangle,0]                      % right corner
            head+[0,instrDisp.triangle] ];                   % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        
        head   = [var.xCenter, var.yLowerLow];  % coordinates of head
        points = [ head+[instrDisp.triangle,0]            % left corner
            head-[instrDisp.triangle,0]                   % right corner
            head-[0,instrDisp.triangle] ];                % vertex
        Screen('FillPoly', var.w,[200 0 0], points);
        Screen('Flip',var.w);
        
    end

    function [] = Instructions_MP_23 (instrDisp, text, position, var)
        % draw final instruction
        
        showInstruction(var,'instructions/payAttention.txt')
        
    end

