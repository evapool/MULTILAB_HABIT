function data = endTask(var,data)

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



% if var.real == 1 % I'm commenting this out because bobfunction is specfic
% for caltech adresses but I'm leaving the function and the code so that if
% you want you can adapt it. It's very practical :-)
%     try
%         bob_sendemail('neuroHabit331@gmail.com', 'decisionHabit', 'evapool@caltech.edu', ['results sub-' num2str(var.sub_ID, '%02.0f') ' session-' num2str(var.session, '%02.0f')], 'see attached', var.resultFile)
%     catch
%         disp('Could not email data... internet may not be connected.');
%     end
% end

end