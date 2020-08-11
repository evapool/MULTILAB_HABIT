function [instrDisp,text, position] = initInstructions(var)

% last modified on June 2017

% format text for intructions
 
% scale textsize to window
textref = 32;
windowref_y = 1560; % we want some thing that correpond to a size of 30 on a screen with a y of 1560
instrDisp.scaledSize = round((textref * var.rect(4)) / windowref_y);


% read general messages from instructions txt files (to simplify
% translation in another language);
text.repeatorcontinue = fileread('instructions/repeatContinue.txt');
text.basicScreen      = fileread('instructions/basicScreen.txt');
text.yellowBoxes      = fileread('instructions/yellowBoxes.txt');
text.aFractal         = fileread('instructions/aFractal.txt');
text.buttonWorks      = fileread('instructions/buttonWorks.txt');
text.eachBox          = fileread('instructions/eachBox.txt');
text.letsPractice     = fileread('instructions/letsPractice.txt');
text.pleasePress      = fileread('instructions/pleasePress.txt');
text.duringRespond    = fileread('instructions/duringRespond.txt');
text.circleAppear     = fileread('instructions/circleAppear.txt');
text.wrongPress       = fileread('instructions/wrongPress.txt');
text.sometimesReward  = fileread('instructions/sometimesReward.txt');
text.pictureMeans     = fileread('instructions/pictureMeans.txt');
text.duringRest       = fileread('instructions/duringRest.txt');
text.relax            = fileread('instructions/relax.txt');
text.multipleFractal  = fileread('instructions/multipleFractals.txt');
text.Fractalmeans     = fileread('instructions/fractalMeans.txt');
text.payAttention     = fileread('instructions/payAttention.txt');
text.restRespondTest  = fileread('instructions/restRespondTest.txt');

% define dimensions of images used for illustrations
instrDisp.CUEbaseRect     = [0 0 var.CUEdim var.CUEdim];% Make a base Rect
instrDisp.FRACTALbaseRect = [0 0 var.FRACTALdim var.FRACTALdim];
instrDisp.CUEpointerRect  = [0 0 var.CUEdim*8 var.CUEdim + 40];
instrDisp.REWARDbaseRect  = [0 0 var.REWARDdim var.REWARDdim];
instrDisp.ACTIONbaseRect  = [0 0 var.ACTIONdim var.ACTIONdim];
instrDisp.triangle        = round((100 * var.rect(4)) / windowref_y);

% define position used for illustration
numSqaures = length(var.squareXpos);
allRectsV = nan(4, 4);% Make our rectangle coordinates
for i = 1:numSqaures
    position.allRectsV(:, i) = CenterRectOnPointd(instrDisp.CUEbaseRect, var.squareXpos(i), var.yUpper);
end

instrDisp.gridThick = 15;

position.Fractal    = CenterRectOnPointd(instrDisp.FRACTALbaseRect,var.xCenter, var.yCenter);
position.CUEpointer = CenterRectOnPointd(instrDisp.CUEpointerRect, var.xCenter, var.yUpper);
position.Reward     = CenterRectOnPointd(instrDisp.REWARDbaseRect, var.xCenter, var.yLower);    
position.Action     = CenterRectOnPointd(instrDisp.ACTIONbaseRect, var.xCenter, var.yLower);

% define the fractal that will be used for the illustration
instrDisp.fractal               = var.rest_fractal;

% previous and next keys
instrDisp.instKeyNext = KbName('RightArrow') ;
instrDisp.instKeyPrev = KbName('LeftArrow') ;
instrDisp.instKeyDone = KbName ('Return');