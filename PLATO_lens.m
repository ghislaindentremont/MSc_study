function PLATO_lens(both, right, left)
%PLATO_lens Summary of this function goes here
% PLATO_lens(1, 0, 0) = trigger both lenses 
% PLATO_lens(0, 1, 0) = trigger right lens 
% PLATO_lens(0, 0, 1) = trigger left lens 


%% load the crucial variables 

load outputReport.mat
%% CHECK IF ANY ATRIBUTES ARE IN PLACE  
if exist('deviceIndex', 'var') == 0
    error('You have never started a PLATO_trial function; therefore, you cannot trigger any lens. ');
    
end

if nargin < 1
    error('You must specify your operation.');
end

if nargin < 2
    right = 0;
    left = 0;
end


clear reportArray(1)

%% TRIGGER BOTH LENSES 



if both == 1
    

    right = 0;
    left = 0;
    % Set the first byte of the report to "Trigger Both Lenses"
    reportArray(1) = 18;
    
    % Write the report to the device to trigger both lenses
    err = PsychHID('SetReport', deviceIndex, outputReport, reportNum , reportArray); %#ok<*NASGU>
    
   clear reportArray(1)
else
    both = 0;
end



%% TRIGGER RIGHT LENS 
if right == 1
    

    both = 0;
    left =0; 
    



    % Set the first byte of the report to "Trigger Right Lens"
    reportArray(1) = 17;
    
    % Write the report to the device to trigger the right lens
    err = PsychHID('SetReport', deviceIndex, outputReport, reportNum , reportArray);
    
    clear reportArray(1)
else
    right = 0;
end

%% TRIGGER LEFT LENS 

if left == 1
    

    both = 0;
    right =0; 
    % Set the first byte of the report to "Trigger Left Lens"
    reportArray(1) = 16;
    
    % Write the report to the device to trigger the left lens
    err = PsychHID('SetReport', deviceIndex, outputReport, reportNum , reportArray);
    clear reportArray(1)
else
    left = 0;
end



end

