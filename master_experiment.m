% Clear the workspace and the screen
sca;
close all;
clearvars;

try 

    % Here we call some default settings for setting up Psychtoolbox
    PsychDefaultSetup(2);  

    % Get the screen numbers
    screens = Screen('Screens');

    % Draw to the external screen if avaliable
    screen_number = max(screens);

    % Define black and white
    white = WhiteIndex(screen_number);
    black = BlackIndex(screen_number);

    % Open an on screen window
    [window, window_rect] = PsychImaging('OpenWindow', screen_number, black);
    % HideCursor(); 

    % Get the size of the on screen window
    [screenXpixels, screenYpixels] = Screen('WindowSize', window);

    % Query the frame duration
    ifi = Screen('GetFlipInterval', window);

    % Query the maximum priority level
    % (I may not use this)
    top_priority_level = MaxPriority(window); 

    % Get the centre coordinate of the window
    [xCenter, yCenter] = RectCenter(window_rect);

    % Set up alpha-blending for smooth (anti-aliased) lines
    Screen('BlendFunction', window, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');

    
    
    %----------------------------------------------------------------------
    %                        ID/Demographics
    %----------------------------------------------------------------------
    
    Screen('TextSize', window, 36); 
    DrawFormattedText(window, 'But really, enough about me, let''s talk about you...',...
    'center', 'center', white );
    Screen('Flip', window);

    prompt = {'Enter participant ID:'
        , 'Enter participant age:'
        ,'Enter participant sex (''m'' or ''f''):'
        , 'Enter participant handedness (''r'' or ''l''):'
        , 'Enter first block condition (''v'' or ''n''):'
        };
    dlg_title = 'Demographics';
    num_lines = 1;
    dems = inputdlg(prompt,dlg_title,num_lines);
    
    if strcmp(char(dems(1)), 'test') 
        
        disp('id, age, sex, and hand are overwritten in test mode')
            
        id = 99;
        age = 99;
        sex = 99;
        hand = 99; 
        blocking_str = char(dems(5));
                
        if isempty(blocking_str)~=1
            while (strcmp(blocking_str, 'v') ~= 1 && strcmp(blocking_str, 'n') ~= 1)
                dems = inputdlg(prompt,dlg_title,num_lines);
                blocking_str = char(dems(5));
            end    
        else
            blocking_str = 'v';
            blocking = 1;
        end
        
        if (strcmp(blocking_str, 'v') == 1)
            blocking = 1;
        elseif (strcmp(blocking_str, 'n') == 1)
            blocking = 2;
        else
            disp('ERROR: blocking condition');  % should also get and error when putting in response matrix
        end  
          
    else
        
        id = char(dems(1));
        age = char(dems(2));
        sex = char(dems(3));
        hand = char(dems(4));
        blocking_str = char(dems(5));

        while (strcmp(hand, 'r') ~= 1 && strcmp(hand, 'l') ~= 1) ||  (strcmp(sex, 'm') ~= 1 && strcmp(sex, 'f') ~= 1) ||  (strcmp(blocking_str, 'v') ~= 1 && strcmp(blocking_str, 'n') ~= 1)
            dems = inputdlg(prompt,dlg_title,num_lines);
            id = char(dems(1));
            age = char(dems(2));
            sex = char(dems(3));
            hand = char(dems(4));
            blocking_str = char(dems(5));
        end

        % get numbers
        id = str2num(id);
        age = str2num(age);

        if (strcmp(sex, 'm') == 1)
            sex = 1;
        elseif (strcmp(sex, 'f') == 1)
            sex = 2;
        else     
            disp('ERROR: sex');  % should also get and error when putting in response matrix
        end

        if (strcmp(hand, 'l') == 1)
            hand = 1;
        elseif (strcmp(hand, 'r') == 1)
            hand = 2;
        else
            disp('ERROR: handedness');  % should also get and error when putting in response matrix
        end  

        if (strcmp(blocking_str, 'MI') == 1)
            task = 1;
        elseif (strcmp(blocking_str, 'ME') == 1)
            task = 2;
        else
            disp('ERROR: block condition');  % should also get and error when putting in response matrix
        end  
        
    end
        
        
    % time and date 
    format shortg
    c = clock;
    year = c(1);
    month = c(2);
    day = c(3);
    hour = c(4);
    minute = c(5);
    seconds = c(6);
    
    
    % set escape
    KbName('UnifyKeyNames');
    escapeKey = KbName('ESCAPE');
    pauseKey = KbName('p');

    keysOfInterest=zeros(1,256);
    keysOfInterest(KbName({'ESCAPE', 'p'}))=1;
    KbQueueCreate(-1, keysOfInterest);
    % start 'escape' Queue
    KbQueueStart;

    

    %----------------------------------------------------------------------
    %                       Home Cross
    %----------------------------------------------------------------------

    % Here we set the size of the arms of our fixation cross
    FIX_CROSS_DIM_PIX = 10*2;

    % Now we set the coordinates (these are all relative to zero we will let
    % the drawing routine center the cross in the center of our monitor for us)
    x_fix_coords = [-FIX_CROSS_DIM_PIX FIX_CROSS_DIM_PIX 0 0];
    y_fix_coords = [0 0 -FIX_CROSS_DIM_PIX FIX_CROSS_DIM_PIX];
    all_fix_coords = [x_fix_coords; y_fix_coords];

    % Set the line width for our fixation cross
    LINE_WIDTH_PIX = 2;
 
    FIX_COLOR = [1 1 1];
    


    %----------------------------------------------------------------------
    %                         Target
    %----------------------------------------------------------------------

    TARGET_SIZE = 5;
    
    TARGET_COLOR = [1 1 1];    


    
    %----------------------------------------------------------------------
    %                       Timing Information
    %----------------------------------------------------------------------

    % Numer of frames to wait before re-drawing
    WAIT_FRAMES = 1;
    
    % Target time frames 
    TARGET_TIME = 5;
    target_time_frames = round(TARGET_TIME / ifi / WAIT_FRAMES);
    


    %----------------------------------------------------------------------
    %                       Conditions
    %----------------------------------------------------------------------
    
    NUM_EXPERIMENTAL_BLOCKS = 2;
    NUM_PRACTICE_BLOCKS = 2;
    
    % have short practice before each block
    num_blocks = NUM_EXPERIMENTAL_BLOCKS + NUM_PRACTICE_BLOCKS;
    
    NUM_PRACTICE_TRIALS = 5;

    TRIALS_PER_CONDITION = 20;
    
    num_trials = NUM_EXPERIMENTAL_BLOCKS * TRIALS_PER_CONDITION + NUM_PRACTICE_BLOCKS * NUM_PRACTICE_TRIALS;
    
    

    %----------------------------------------------------------------------
    %                       Experimental Loop
    %----------------------------------------------------------------------

    [pressed, firstPress]=KbQueueCheck; 
    
    for block = 1:num_blocks
        
        if pressed
            if firstPress(KbName('ESCAPE'))
                break
            end
        end
        
        % get ITIs   
        itis = rand(1,num_trials)+1;  % interval of 1-2 seconds

        % final condition matrix
        cond_matrix_shuffled = [itis];
        
        if strcmp(blocking_str, 'v')
            first_condition = 'VISION';
            second_condition = 'NO VISION';
        else
            first_condition = 'NO VISION';
            second_condition = 'VISION';
        end
        
        if block == 1
            
            num_trials_in_this_block = NUM_PRACTICE_TRIALS;
            
            %------------------- Block Instruction Message ------------------------
            Screen('TextSize', window, 36); 
            DrawFormattedText(window, sprintf('The following are PRACTICE trials for the the %s condition\n\n\nPress Spacebar To Begin the Block', first_condition),...
                'center', 'center', white );
            Screen('Flip', window);
            KbStrokeWait; 
            %----------------------------------------------------------------------
            
        elseif block == 2
            
            num_trials_in_this_block = TRIALS_PER_CONDITION;
            
            %------------------- Block Instruction Message ------------------------
            Screen('TextSize', window, 36); 
            DrawFormattedText(window, sprintf('The following are EXPERIMENTAL trials for the the %s condition\n\n\nPress Spacebar To Begin the Block', first_condition),...
                'center', 'center', white );
            Screen('Flip', window);
            KbStrokeWait; 
            %----------------------------------------------------------------------

        elseif block == 3
            
            num_trials_in_this_block = NUM_PRACTICE_TRIALS;
            
            %------------------- Block Instruction Message ------------------------
            Screen('TextSize', window, 36); 
            DrawFormattedText(window, sprintf('The following are PRACTICE trials for the the %s condition\n\n\nPress Spacebar To Begin the Block', second_condition),...
                'center', 'center', white );
            Screen('Flip', window);
            KbStrokeWait; 
            %----------------------------------------------------------------------
  
        elseif block == 4
            
            num_trials_in_this_block = TRIALS_PER_CONDITION;
            
            %------------------- Block Instruction Message ------------------------
            Screen('TextSize', window, 36); 
            DrawFormattedText(window, sprintf('The following are EXPERIMENTAL trials for the the %s condition\n\n\nPress Spacebar To Begin the Block', second_condition),...
                'center', 'center', white );
            Screen('Flip', window);
            KbStrokeWait; 
            %----------------------------------------------------------------------

        else
            disp('ERROR: block undifined')
        end
        
        
              
        for trial = 1:num_trials_in_this_block
            
            % check if escape key has been pressed. If so, exit experiment
            % (by breaking out of a few loops)
            % otherwise if pause key, then we wait until a response to keep
            % going
            [pressed, firstPress]=KbQueueCheck; 
            if pressed
                if firstPress(KbName('ESCAPE'))
                    break
                elseif firstPress(KbName('p'))
                    Screen('TextSize', window, 36); 
                    DrawFormattedText(window, 'You''ve requested a break. Take one.\n\n\nPress Spacebar To Continue',...
                        'center', 'center', white );
                    Screen('Flip', window);
                    KbStrokeWait; 
                end
            end
            
            % waiting for finger to touch screen
            Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [xCenter yCenter*15/8], 2);
            vbl = Screen('Flip', window);
            
            % setting touch screen cursor variables
            startbutton=1;
            this=0;
            that=0;
            buttonstatus=[0 0 0];
            
            % detecting bottom screen press
            while startbutton==1;
               [this,that,buttonstatus]=GetMouse; %gets x and y position of mouse
               if buttonstatus(1)==0; %status when button is not pressed
                   startbutton=1;
               elseif buttonstatus(1)==1 && that<900;  %status if it is pressed
                   startbutton=1;
               else
                   startbutton=0;
               end
            end
          
          
            %---------------------------- ITI -----------------------------
            iti = cond_matrix_shuffled(1,trial);
            iti_time_frames = round(iti / ifi / WAIT_FRAMES);

            Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [xCenter yCenter*15/8], 2);
            vbl = Screen('Flip', window);

            for frame = 1:iti_time_frames-1
                Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [xCenter yCenter*15/8], 2);
                vbl = Screen('Flip', window, vbl + (WAIT_FRAMES - 0.5) * ifi);
            end
            %--------------------------------------------------------------


            %--------------------- Draw Target ----------------------------
            Screen('DrawDots', window, [xCenter; yCenter/8], TARGET_SIZE, TARGET_COLOR, [], 4);
            Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [xCenter yCenter*15/8], 2);
            vbl = Screen('Flip', window);
            
            % setting touch screen cursor variables
            startbutton=1;
            this=0;
            that=0;
            buttonstatus=[0 0 0];
            
            % detecting bottom screen press
            tic;
            while startbutton==1 || toc<1;
                Screen('DrawDots', window, [xCenter; yCenter/8], TARGET_SIZE, TARGET_COLOR, [], 4);
                Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [xCenter yCenter*15/8], 2);
                vbl = Screen('Flip', window, vbl + (WAIT_FRAMES - 0.5) * ifi);
                
               [this,that,buttonstatus]=GetMouse; %gets x and y position of mouse
               if buttonstatus(1)==0; %status when button is not pressed
                   startbutton=1;
               elseif buttonstatus(1)==1 && that>900;  %status if it is pressed
                   startbutton=1;
               else
                   startbutton=0;
               end
            end
            %--------------------------------------------------------------


        end
        
        % collect raw data for an added amount of time to avoid
        % edge-effects with offline filtering
        if firstPress(KbName('ESCAPE'))
            Screen('TextSize', window, 36); 
            DrawFormattedText(window, 'The experiment is over\n\n\nThe experimenter should be with you shortly' ,...
            'center', 'center', white );
            Screen('Flip', window);
            tic;
            while toc < 10
            end
        end
        
%         % write response matrix to csv
%         csvwrite(sprintf('C:/Users/Kine Research/Documents/MATLAB/ghis_data/raw_%s_p%i_%s_block_%i.csv', feedback_str, id, task_str, block), raw_mat);
        
    end
    
    % turn off screen
    sca;
    
catch
    sca;
    psychrethrow(psychlasterror);
end


