function showBonus(var)

% depict bonus slide for the devaluation procedure

% get the picture and text
if var.devalued == 1 % target for devaluation will be sweet
      target_pict = var.sweetImage;
      message     = ['Bonus!  All you can eat ' var.sweetLabel];
       
elseif var.devalued == 2 % target for devaluation will be savory
     target_pict   = var.saltyImage;
     message       = ['Bonus!  All you can eat ' var.saltyLabel];
     
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