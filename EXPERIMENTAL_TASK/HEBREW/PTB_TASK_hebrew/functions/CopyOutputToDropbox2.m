function CopyOutputToDropbox2(subjectFileName, mainPath)
% Copy all the output files of a subject to the dropbox "experiments
% outputs" folder in the end of the experiment\session.
% Locate it in the end of the main function of the experiment.

outputPath = [mainPath]; %[mainPath '/data'];

% Change this 3 variables according to experiment:
NumOfFoldersBack = 2; % From the experiment folder.
PathFromCommonFolder = 'Dropbox/experimentsOutput/HRS/Output';%The path from the first common folder of the origin and the deatination folder.
FilesToCopy = [outputPath '/' subjectFileName];

% Creating the destination path:
Slashes = strfind(mainPath,'/');
Destination = mainPath(1:Slashes(end-NumOfFoldersBack+1));
Destination = [Destination PathFromCommonFolder];
% Copy the relevant files:
copyfile(FilesToCopy, Destination);

fprintf('output files have been copied successfully\n')
end
