function rate = likertScale(imageX, question, scale, var, anchMin, anchMax)


% last modified on June 2017
% scale = vector with the steps e.g., [1 2 3 4 5 6 7 8 9 10]
% question char with the question
% image or 0 if no image will be shown

if sum(sum(max(imageX))) > 0
    imageTexture = Screen('MakeTexture', var.w, imageX);
end

% Define the image rect and the destination rect
imageRect = [0 0 RectWidth(var.rect) (RectHeight(var.rect) - 100)];
imageDestRect = CenterRect([0 0 var.FRACTALdim var.FRACTALdim], imageRect);

% scale text size to the screen used
textRefBig   = 40;
textRefSmall = 30;
windowref_y = 1560; % we want something that correponds to a size of 30 on a screen with a y of 1560

scaledSizeBig   = round((textRefBig * var.rect(4)) / windowref_y);
scaledSizeSmall = round((textRefSmall * var.rect(4)) / windowref_y);

% scale distances
twenty = round((20 * var.rect(4)) / windowref_y);
forty  = round((40 * var.rect(4)) / windowref_y);
sixty  = round((60 * var.rect(4)) / windowref_y);
dent   = round((10 * var.rect(4)) / windowref_y);
if dent > 7; dent = 7; end
if dent < 0; dent = 2; end

% Query the frame duration
ifi = Screen('GetFlipInterval', var.w);

% Sync us and get a time stamp
vbl = Screen('Flip', var.w);
waitframes = size(scale,2);

% VAS parameters
verticalPosLine = var.yLower;
distanceFromBorderLine = var.screenXpixels/9;

% Maximum priority level
topPriorityLevel = MaxPriority(var.w);
Priority(topPriorityLevel);

% Set the amount we want our square to move on each button press
steps            = size(scale,2) -1;
likertDim        = (RectWidth(var.rect) - distanceFromBorderLine) -distanceFromBorderLine;
pixelsPerPress   = likertDim/steps;
cursorPosition   = distanceFromBorderLine+pixelsPerPress*(round((size(scale,2)/2)-1));
rate             = scale(round((size(scale,2)/2))); % midpoint of the scale

% The avaliable keys to press
escapeKey = [var.mycontrol];
leftKey = var.leftKey;   % former 'LeftArrow'
rightKey = var.rightKey; % former 'RightArrow'

% This is the cue which determines whether we exit the demo
exitDemo = false;

% Loop the animation until the escape key is pressed
while exitDemo == false
    
    % Check the keyboard to see if a button has been pressed
    [~,~, keyCode] = KbCheck(-3,2);
    
    respKey = find(keyCode);
    
    % Depending on the button press, either move ths position of the square
    % or exit the demo
    if ismember(respKey, escapeKey)
        exitDemo = true;
    elseif ismember (respKey, leftKey)
        cursorPosition = cursorPosition - pixelsPerPress;
        rate           = rate -1;
    elseif ismember (respKey, rightKey)
        cursorPosition = cursorPosition + pixelsPerPress;
        rate           = rate +1;
    end
    
    % We set bounds to make sure our square doesn't go completely off of
    % the screen
    if cursorPosition < distanceFromBorderLine
        cursorPosition = distanceFromBorderLine;
    elseif cursorPosition > RectWidth(var.rect) - distanceFromBorderLine
        cursorPosition = RectWidth(var.rect) - distanceFromBorderLine;
    end
    
    % make sure rating does not exceed boundaries
    if rate > max(scale)
        rate =  max(scale);
    elseif rate < min (scale)
        rate = min (scale);
    end
    
    % Show question
    Screen('TextFont', var.w, 'Arial');
    Screen('TextSize', var.w, scaledSizeBig);
    Screen('TextStyle', var.w, 1);
    
    if sum(sum(max(imageX))) > 0
        DrawFormattedText(var.w, question, 'center',var.yUpperHigh, [0 0 0], 65, false, false, 1.2);
    else
        DrawFormattedText(var.w, question, 'center','center', [0 0 0], 65, false, false, 1.2);
    end

    % Show image
    if sum(sum(max(imageX))) > 0
        Screen('DrawTexture', var.w, imageTexture, [], imageDestRect);
    end
    
    % Draw the vertical line
    Screen('DrawLine', var.w, [0 0 0], distanceFromBorderLine, verticalPosLine, RectWidth(var.rect) - distanceFromBorderLine, verticalPosLine, dent);
    
    % Likert
    for i = 1:size(scale,2)
        
        positionX = distanceFromBorderLine+pixelsPerPress*(i-1);
        
        Screen('DrawLine', var.w, [10 10 10], positionX, verticalPosLine - twenty, positionX, verticalPosLine + twenty, dent);
        number = num2str (scale(i));
        Screen('TextFont', var.w, 'Arial');
        Screen('TextSize', var.w, scaledSizeSmall);
        Screen('TextStyle', var.w, 1);
        DrawFormattedText(var.w, number, positionX, verticalPosLine + forty);
        
        if i == 1
            % draw min anchor
            Screen('TextFont', var.w, 'Arial');
            Screen('TextSize', var.w, scaledSizeSmall);
            Screen('TextStyle', var.w, 1);
            %DrawFormattedText(var.w, anchMin, 'left', verticalPosLine + 1.5*sixty, [0 0 0], 60);
            DrawFormattedText(var.w, anchMin, positionX -2*sixty, verticalPosLine + 1.5*sixty, [0 0 0], 60);

        elseif i == size(scale,2)
            % draw max anchor
            Screen('TextFont', var.w, 'Arial');
            Screen('TextSize', var.w, scaledSizeSmall);
            Screen('TextStyle', var.w, 1);
            DrawFormattedText(var.w, anchMax, positionX -2*sixty, verticalPosLine + 1.5*sixty, [0 0 0], 60);
            
        end
        
    end
    
    % Print confirmation message
    Screen('TextFont', var.w, 'Arial');
    Screen('TextSize', var.w, scaledSizeSmall);
    Screen('TextStyle', var.w, 1);
    %--- Rani
    %DrawFormattedText(var.w, 'press the space keyboard to confirm', 'center', RectHeight(var.rect) - 3*forty);
    DrawFormattedText(var.w, fliplr([1500 1495 1510 47 1497 32 1506 1500 32 1502 1511 1513 32 1492 1512 1493 1493 1495 32 1499 1491 1497 32 1500 1488 1513 1512]), 'center', RectHeight(var.rect) - 3*forty);
    %---
    
    % Draw the cursor with the new position
    Screen('DrawLine', var.w, [210 10 10], cursorPosition, verticalPosLine - twenty, cursorPosition, verticalPosLine + twenty, dent);
    
    % Flip to the screen
    vbl  = Screen('Flip', var.w, vbl + (waitframes-05) * ifi);
    
end


% Close the texture
if sum(sum(max(imageX))) > 0
    Screen('Close', imageTexture);
end

end