function [instrDisp,text, position] = initInstructions(var)

% format text for intructions

% scale textsize to window
textref = 40;
windowref_y = 1560; % we want some thing that correpond to a size of 30 on a screen with a y of 1560
instrDisp.scaledSize = round((textref * var.rect(4)) / windowref_y);


% read general messages from instructions txt files (to simplify
% translation in another language);
%--- Rani (added the 'str2num')
text.repeatorcontinue = str2num(fileread('instructions/repeatContinue.txt'));
text.basicScreen      = str2num(fileread('instructions/basicScreen.txt'));
text.yellowBoxes      = str2num(fileread('instructions/yellowBoxes.txt'));
text.aFractal         = str2num(fileread('instructions/aFractal.txt'));
text.buttonWorks      = str2num(fileread('instructions/buttonWorks.txt'));
text.eachBox          = str2num(fileread('instructions/eachBox.txt'));
text.letsPractice     = str2num(fileread('instructions/letsPractice.txt'));
text.pleasePress      = str2num(fileread('instructions/pleasePress.txt'));
text.duringRespond    = str2num(fileread('instructions/duringRespond.txt'));
text.circleAppear     = str2num(fileread('instructions/circleAppear.txt'));
text.wrongPress       = str2num(fileread('instructions/wrongPress.txt'));
text.sometimesReward  = str2num(fileread('instructions/sometimesReward.txt'));
text.pictureMeans     = str2num(fileread('instructions/pictureMeans.txt'));
text.duringRest       = str2num(fileread('instructions/duringRest.txt'));
text.relax            = str2num(fileread('instructions/relax.txt'));
text.multipleFractal  = str2num(fileread('instructions/multipleFractals.txt'));
text.Fractalmeans     = str2num(fileread('instructions/fractalMeans.txt'));
text.payAttention     = str2num(fileread('instructions/payAttention.txt'));
text.restRespondTest  = str2num(fileread('instructions/restRespondTest.txt'));

%---Rani (another part)
%This part is to fix the language in the experiment rooms
%direction correctlly:
StructFieldNames = fieldnames(text)

OS_Version = system_dependent('getos');
Is_El_Capitan = ~isempty(strfind(OS_Version,'10.11'));

if ~Is_El_Capitan
    % Check if reverse hebrew by checking matlab version:
    Versions = ver;
    % Find the right line to look at in the struct:
    index = find(strcmp({Versions.Name}, 'MATLAB')==1);
    MatlabVersion = Versions(index).Release;
    ChangeDirection = strfind(MatlabVersion,'2014');
    % Reverse process:
    if ~isempty(ChangeDirection)
        for i=1:numel(StructFieldNames)
            Borders = find(text.(StructFieldNames{i})==10); % ASCII 10 takes it one line below
            if isempty(Borders) % If there is no line
                text.(StructFieldNames{i}) = fliplr(text.(StructFieldNames{i}));
            else
                for j = 1:length(Borders)
                    if j == 1 % first line
                        text.(StructFieldNames{i})(1:Borders(1)-1) = fliplr(text.(StructFieldNames{i})(1:Borders(1)-1));
                        % last line:
                        text.(StructFieldNames{i})(Borders(end)+1:end) = fliplr(text.(StructFieldNames{i})(Borders(end)+1:end));
                    else % all the rows in the middle
                        text.(StructFieldNames{i})(Borders(j-1)+1:Borders(j)-1) = fliplr(text.(StructFieldNames{i})(Borders(j-1)+1:Borders(j)-1));
                    end
                end
            end
            % Change '(' in ')'
            LocationsToSwitchRightBrackets = find(text.(StructFieldNames{i})==')');
            LocationsToSwitchLeftBrackets = find(text.(StructFieldNames{i})=='(');
            text.(StructFieldNames{i})(LocationsToSwitchRightBrackets) = '(';
            text.(StructFieldNames{i})(LocationsToSwitchLeftBrackets) = ')';
            % Change '(' in ')'
            LocationsToSwitchP = find(text.(StructFieldNames{i})=='R');
            LocationsToSwitchB = find(text.(StructFieldNames{i})=='Y');
            text.(StructFieldNames{i})(LocationsToSwitchP) = 'Y';
            text.(StructFieldNames{i})(LocationsToSwitchB) = 'R';
            
            %change number with more than one figure directions:
            if any(ismember(text.(StructFieldNames{i}), 48:57))
                PlacesWithFigures = ismember(text.(StructFieldNames{i}), 48:57);%if it isa figure of 0 to 9 (in ASCII).
                DiffOfPlacesWithFigures = diff(PlacesWithFigures);
                ArrayOfWhichRelevantToFlip = PlacesWithFigures(1:end-1) == 1 & DiffOfPlacesWithFigures == 0;
                Placer = 0;
                Addition = 0;
                for j = 2 : length(ArrayOfWhichRelevantToFlip)
                    if ArrayOfWhichRelevantToFlip(j) == 0 || ArrayOfWhichRelevantToFlip(j-1) == 0
                        Placer = Placer + 1;
                    else
                        Addition = Addition+1;
                    end
                    if ArrayOfWhichRelevantToFlip(j) == 0 && ArrayOfWhichRelevantToFlip(j-1) == 1
                        text.(StructFieldNames{i})(Placer:j) = text.(StructFieldNames{i})(j:-1:Placer);
                        Placer = Placer + Addition;
                        Addition = 0;
                    end
                end
            end
        end
    end
    % Correcting apostrophes:
    text.(StructFieldNames{i})(text.(StructFieldNames{i}) == 8216 | text.(StructFieldNames{i}) == 8217) = '''';
    text.(StructFieldNames{i})(text.(StructFieldNames{i}) == 8220 | text.(StructFieldNames{i}) == 8221) = '"';
end

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