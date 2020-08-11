
function time = displayITI (var)

% Last modifed on May 2017 by Eva

startT = GetSecs();

% plot fixation cross in the middle
Screen('TextFont',var.w, 'Arial');
Screen('TextSize', var.w, 36);
Screen('TextColor',var.w, [0 0 0]);
DrawFormattedText(var.w,'+','center','center');

Screen ('Flip', var.w);

timer = GetSecs()-var.time_MRI;
while timer < var.ref_end
    timer = GetSecs()-var.time_MRI;
end

time = GetSecs()-startT;

end