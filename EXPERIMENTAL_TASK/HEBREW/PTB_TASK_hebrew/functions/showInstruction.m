function showInstruction(var, instructionFile)

% import the instructions from a file
instructionText = fileread(instructionFile);

% -- Added by Rani:
instructionText = str2num(instructionText);


%This part is to fix the language in the experiment rooms
%dirction correctlly:
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
        Borders = find(instructionText==10); % ASCII 10 takes it one line below
        if isempty(Borders) % If there is no line
            instructionText = fliplr(instructionText);
        else
            for j = 1:length(Borders)
                if j == 1 % first line
                    instructionText(1:Borders(1)-1) = fliplr(instructionText(1:Borders(1)-1));
                    % last line:
                    instructionText(Borders(end)+1:end) = fliplr(instructionText(Borders(end)+1:end));
                else % all the rows in the middle
                    instructionText(Borders(j-1)+1:Borders(j)-1) = fliplr(instructionText(Borders(j-1)+1:Borders(j)-1));
                end
            end
        end
        % Change '(' in ')'
        LocationsToSwitchRightBrackets = find(instructionText==')');
        LocationsToSwitchLeftBrackets = find(instructionText=='(');
        instructionText(LocationsToSwitchRightBrackets) = '(';
        instructionText(LocationsToSwitchLeftBrackets) = ')';
        % Change '(' in ')'
        LocationsToSwitchP = find(instructionText=='R');
        LocationsToSwitchB = find(instructionText=='Y');
        instructionText(LocationsToSwitchP) = 'Y';
        instructionText(LocationsToSwitchB) = 'R';
        
        %change number with more than one figure directions:
        if any(ismember(instructionText, 48:57))
            PlacesWithFigures = ismember(instructionText, 48:57);%if it isa figure of 0 to 9 (in ASCII).
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
                    instructionText(Placer:j) = instructionText(j:-1:Placer);
                    Placer = Placer + Addition;
                    Addition = 0;
                end
            end
        end
        
    end
    % Correcting apostrophes:
    instructionText(instructionText == 8216 | instructionText == 8217) = '''';
    instructionText(instructionText == 8220 | instructionText == 8221) = '"';
end

% --
% scale text size to the screen used
textref = 40;
windowref_y = 1560; % we want something that correponds to a size of 30 on a screen with a y of 1560
scaledSize = round((textref * var.rect(4)) / windowref_y);

% set screen setting

Screen('TextFont', var.w, 'Arial');
Screen('TextSize', var.w, scaledSize);
Screen('TextStyle', var.w, 1);

% print the instruction on the screen
DrawFormattedText(var.w, instructionText, 'center', 'center', [0 0 0], 60, false, false, 1.2)

Screen(var.w, 'Flip');

end