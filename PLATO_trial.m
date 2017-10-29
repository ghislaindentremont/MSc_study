function PLATO_trial(start_or_end)
%PLATO_trial(1) = start a trial
%PLATO_trial(0)= end of a trial
% This function script allows you to control PLATO goggles from any
% Windows PC (notice: tested on Windows 7 64-bit systems).
% It automatically finds the device and allows it to start or end a trial,
% in other words to start working with the goggles and to stop them. 
% In order to start working with the goggles you need to use this function at 
% both the beginning and the end of your experiment, specifying the start of a 
% trial (by placing it at the beginning of your experiment script) and the end 
% of a trial (by placing it at the end of your experiment script). 
% In order to actually trigger the lenses you will use another function called: PLATO_lens.

% Authors:
% Rafal Skiba (rafalskiba@unr.edu) and Kyle Renwick (kyle@peqo.ca)
% Version 0.1 03/30/2014

%% Specify the operation

if nargin < 1
    error('You must specify if you want to start or end a trial: PLATO_trial(1) = start a trial; PLATO_trial(0)= end of a trial ');
end





if start_or_end == 1 % ==== START TRIAL/ START WORK with THE GOGGLES
    %% Locate your 'ToTaLcontrol USB device among all devices connected to your
    % computer. Please remember that you need to connect the device before 
    % starting Matlab. Also, if your device number is not detected the first 
    % time, close Matlab completely and open it again.
    
    % Create a structure variable with all devices
    devices = PsychHID('devices');
    % Find device numbers of all USB devices
    numberOfDevices=PsychHID('NumDevices');
    % Create a string variable that will be compared to all product names of
    % your USB devices
    a = 'ToTaLcontrol';
    % Assign 0 value to a new variable deviceIndex
    deviceIndex = 0;
    %Find your device number and assign it to variable: deviceIndex
    for ind = 1:numberOfDevices
        
        if strcmp(devices(:,ind).product, a)== 1
            deviceIndex = ind;
            
        end
        
    end
    
    %% Define constant variables
    
    % This is a constant defined by PsychHID
    outputReport = 2;
    
    % This is the output report number for the ToTaLcontrol Pulse Generator
    reportNum = 0;
    
    % Create a zero-filled array of 64 8-bit bytes for the output report
    reportArray = zeros(1, 64, 'uint8');
    
    % Save the crucial variable to a mat and then you will load it with
    % your PLATO_lens function
    save outputReport reportNum deviceIndex reportArray outputReport
        
    %% START GOGGLES/TRIAL
    
    reportArray(1) = 19;
    
    % Write the report to the device to start the trial
    err = PsychHID('SetReport', deviceIndex, outputReport, reportNum , reportArray);
    
    
   
else % STOP GOGGLES/TRIAL
    %% load the crucial variables 

load outputReport.mat
    
    if exist('deviceIndex', 'var') == 0
        disp 'You have never started a trial with the PLATO_trial function; therefore, you cannot end the trial. '
        close all
        return;
    end
    
    % Set the first byte of the report to "Stop Trial"
    reportArray(1) = 20;
    
    % Write the report to the device to stop the trial
    err = PsychHID('SetReport', deviceIndex, outputReport, reportNum , reportArray); %#ok<*NASGU,*NODEF>
    
    
end

