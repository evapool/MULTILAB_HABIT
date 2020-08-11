function var = inputCheck (var, time)

% make sure the input variables make sense and correct for possible
% mistakes
disp ('********************************************************************')
disp (['***input*** CHECKING INPUT: Check number: ' num2str(time)])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% check for empty fields

if isempty(var.sub_ID)
    disp ('***input*** SUBJECT NUMBER is empty re-enter')
    var.sub_ID = input('***input*** SUBJECT NUMBER: ');
end

if isempty(var.session)   
    disp ('***input*** SESSION NUMBER is empty re-enter')
    var.session = input('***input*** SESSION NUMBER (1,2 or 3 session day): '); % 1,2,or 3 session
end

if isempty(var.training)
    disp ('***input*** TRAINING SCHEDULE is empty re-enter')
    var.training = input('***input*** TRAINING SCHEDULE (1 = 1day group, 3 = 3daygroup): '); % 1 day or 3 days
    
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% if training program is 1 day there cannot be more than 1 session training

if var.training ==1 && var.session > 1
    beep
    disp ('***input*** the input variables are not consistent please re-enter')
    var.sub_ID = input('***input*** SUBJECT NUMBER: ');
    var.session = input('***input*** SESSION NUMBER (1,2 or 3 session day): '); % 1,2,or 3 session
    var.training = input('***input*** TRAINING SCHEDULE (1 = 1day group, 3 = 3daygroup): '); % 1 day or 3 days
    
end

while var.training ==1 && var.session > 1
    
    beep
    disp ('*** the input variables are still not consistent please re-enter')
    var.sub_ID = input('***input*** SUBJECT NUMBER: ');
    var.session = input('***input*** SESSION NUMBER (1,2 or 3 session day): '); % 1,2,or 3 session
    var.training = input('***input*** TRAINING SCHEDULE (1 = 1day group, 3 = 3daygroup): '); % 1 day or 3 days
    
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% if session is not equal to one then check that the previous file is in 
% the data folder

%FlushEvents();
KbQueueCreate(var.deviceIndex);
KbQueueStart(var.deviceIndex);
reenter = 0;

if var.session == 2 || var.session == 3 % for multiple sessions
    
    % name of the results file of the
    Name  = (strcat('data/', 'sub-', num2str(var.sub_ID, '%02.0f'), '_task-HAB', num2str(var.training), 'day_session-',num2str(var.session-1,'%02.0f'),'.mat'));
    
    if ~exist(Name, 'file')
        beep;
        disp ('***input*** the previous session of this participant is not in the data folder');
        disp ('***input*** press space to continue or enter to re-enter the experiment inputs');
        decide = 0;
        
        while decide == 0
            
            [down, keyCode]=KbQueueCheck(var.deviceIndex);
            
            keyresp = find(keyCode);
            
            if (down == 1)
                
                if ismember (keyresp,KbName('space'))
                    
                    disp ('***input*** continuing even if previous session is not in the data folder');
                    decide = 1;
                    
                elseif ismember (keyresp, KbName('Return'))
   
                    reenter = 1;
                    decide = 1;
                    
                end
                
            end
            
        end
        
        KbQueueRelease(var.deviceIndex);
        
    end
      
end


if reenter
    
    disp ('***input*** wait preparing the input variables...')
    var.sub_ID = input('***input*** SUBJECT NUMBER: ', 's'); %ID
    if isempty(var.sub_ID)
        disp ('***input*** SUBJECT NUMBER is empty re-enter')
        var.sub_ID = input('***input*** SUBJECT NUMBER: ');
    end
    var.session  = input('***input*** SESSION NUMBER (1,2 or 3 session day): '); % 1,2,or 3 session
    var.training = input('***input*** TRAINING SCHEDULE (1 = 1-day-group, 3 = 3-day-group): '); % 1 day or 3 days

end
