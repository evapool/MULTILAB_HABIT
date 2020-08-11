function data = endTask(var,data)

% last modified on June 2017

showInstruction(var,'instructions/thanks.txt')
WaitSecs(0.4);

while 1    
    [~, ~, keycode] = KbCheck(-3,2);
    keyresp = find(keycode);
    if ismember (keyresp, var.mycontrol)
        break
    end   
end

save(var.resultFile, 'data', '-append');
Screen('CloseAll');
ShowCursor;
RestrictKeysForKbCheck([]); %re-allow all keys to be read

end