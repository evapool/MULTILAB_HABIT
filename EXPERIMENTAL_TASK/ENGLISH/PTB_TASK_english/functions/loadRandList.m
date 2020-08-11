function [condition, duration] = loadRandList(condition, duration)

% last modified June 2017 

% Restict randomization to no block type occuring twice in a row
% data

randomIndex = randperm(length(condition)); % initial random index (there is no check of consecutive repetition here)
seq = condition(randomIndex);              % intitial shuffle of the CS condition

% engine repetitions
i = find(diff(seq));
n = [i numel(seq)] - [0 i];
c = arrayfun(@(X) X-1:-1:0, n , 'un',0);
y = cat(2,c{:}); % here you get the number of reptition of each element of the sequence of interest

% here we shuffle until there will be no more than 2 consecutives repetitions
while  ~isempty (find(y > 0, 1))
    
    randomIndex = randperm(length(condition));
    seq = condition(randomIndex); % no more three repetitions in a row
    
    % engine repetition
    i = find(diff(seq));
    n = [i numel(seq)] - [0 i];
    c = arrayfun(@(X) X-1:-1:0, n , 'un', 0);
    y = cat(2,c{:});
        
end

condition = condition(randomIndex);
duration  = duration (randomIndex);
