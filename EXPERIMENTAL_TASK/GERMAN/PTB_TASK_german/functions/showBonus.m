function showBonus(var)

% last modified on June 2017
% depict bonus slide for the devaluation procedure

bonusText = importdata('instructions/bonus.txt');

% get the picture and text
if var.devalued == 1 % target for devaluation will be sweet
      target_pict = var.sweetImage;
      message     = [bonusText{1} var.sweetLabel bonusText{2}];
       
elseif var.devalued == 2 % target for devaluation will be savory
     target_pict   = var.saltyImage;
     message       = [bonusText{1} var.saltyLabel bonusText{2}];
     
end

% define position of the picture
baseRect           = [0 0 var.FRACTALdim var.FRACTALdim];% Make a base Rect
positionPicture    = CenterRectOnPointd(baseRect, var.xCenter, var.yCenter);

% format text
Screen('TextFont', var.w, 'Arial');
Screen('TextSize', var.w, var.scaledSize);
Screen('TextStyle', var.w, 1);

% prepare the bonus screen
target_text  = Screen('MakeTexture',var.w, target_pict);
Screen('DrawTexture', var.w, target_text ,[], positionPicture );
DrawFormattedText(var.w, message , 'center', var.yUpper, [0 0 0], 60);
   
% show the screen
Screen('Flip',var.w);

end