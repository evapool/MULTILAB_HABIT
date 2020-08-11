function showBonus(var)

% depict bonus slide for the devaluation procedure

% get the picture and text
if var.devalued == 1 % target for devaluation will be sweet
      target_pict = var.sweetImage;
      %---Rani
      %message     = ['Bonus!  All you can eat ' var.sweetLabel];
      message     = fliplr([41 1489 1489 1511 1513 1492 32 1506 1491 1499 1504 47 1497 32 1488 1514 32 1492 1504 1505 1497 1497 1503 40 10 10 1489 1493 1504 1493 1505 33 32 1488 1499 1493 1500 47 1488 1499 1500 1497 32 var.sweetLabelHebrew 32 1499 1508 1497 32 1497 1499 1493 1500 1514 1498]);           
elseif var.devalued == 2 % target for devaluation will be savory
     target_pict   = var.saltyImage;
     %message       = ['Bonus!  All you can eat ' var.saltyLabel];
     message       = fliplr([41 1489 1489 1511 1513 1492 32 1506 1491 1499 1504 47 1497 32 1488 1514 32 1492 1504 1505 1497 1497 1503 40 10 10 1489 1493 1504 1493 1505 33 32 1488 1499 1493 1500 47 1488 1499 1500 1497 32 var.saltyLabelHebrew 32 1499 1508 1497 32 1497 1499 1493 1500 1514 1498]);
     %---
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