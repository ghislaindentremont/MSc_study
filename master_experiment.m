% Clear the workspace and the screen
sca;
close all;
clearvars;

try 

    %----------------------------------------------------------------------
    %                        ID/Demographics
    %----------------------------------------------------------------------

    prompt = {'Enter participant ID:'
        , 'Enter participant age:'
        ,'Enter participant sex (''m'' or ''f''):'
        , 'Enter participant handedness (''r'' or ''l''):'
        , 'Enter first block condition (''v'' or ''n''):'
        , 'Enter participant type (''p'' or ''e''):'
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
        

        pilot_str = 'p';
        pilot = 1;
                
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
        pilot_str = char(dems(6));

        while (strcmp(hand, 'r') ~= 1 && strcmp(hand, 'l') ~= 1) ||  (strcmp(sex, 'm') ~= 1 && strcmp(sex, 'f') ~= 1) ||  (strcmp(blocking_str, 'v') ~= 1 && strcmp(blocking_str, 'n') ~= 1) || (strcmp(pilot_str, 'p') ~= 1 && strcmp(pilot_str, 'e') ~= 1)
            dems = inputdlg(prompt,dlg_title,num_lines);
            id = char(dems(1));
            age = char(dems(2));
            sex = char(dems(3));
            hand = char(dems(4));
            blocking_str = char(dems(5));
            pilot_str = char(dems(6));
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

        if (strcmp(blocking_str, 'v') == 1)
            blocking = 1;
        elseif (strcmp(blocking_str, 'n') == 1)
            blocking = 2;
        else
            disp('ERROR: blocking condition');  % should also get and error when putting in response matrix
        end  
        
        if (strcmp(pilot_str, 'p') == 1)
            pilot = 1;
        elseif (strcmp(pilot_str, 'e') == 1)
            pilot = 2;
        else
            disp('ERROR: participant type');  % should also get and error when putting in response matrix
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
    %                        Screen Setup
    %----------------------------------------------------------------------
    
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
    HideCursor(); 

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
    %                       Home Cross
    %----------------------------------------------------------------------
    
    % HAVE TO ACCOUNT FOR ROTATION

    % Here we set the length of the arms (from center) of our fixation cross
    FIX_CROSS_DIM_PIX = 20;
    
    % where do we want the fixation cross centered
    FIX_X_CENTERED = xCenter*15/8;
    FIX_Y_CENTERED = yCenter;

    % Now we set the coordinates (these are all relative to zero we will let
    % the drawing routine center the cross in the center of our monitor for us)
    x_fix_coords = [-FIX_CROSS_DIM_PIX FIX_CROSS_DIM_PIX 0 0];
    y_fix_coords = [0 0 -FIX_CROSS_DIM_PIX FIX_CROSS_DIM_PIX];
    all_fix_coords = [x_fix_coords; y_fix_coords];

    % Set the line width for our fixation cross
    LINE_WIDTH_PIX = 5;
 
    FIX_COLOR = [1 1 1];
    


    %----------------------------------------------------------------------
    %                         Target
    %----------------------------------------------------------------------

    TARGET_SIZE = 10;
    
    TARGET_COLOR = [1 1 1];  
    
    % where is the target centered?
    TARGET_X_CENTERED = xCenter*2/3;
    TARGET_Y_CENTERED = yCenter;
    
    TARGET_X_BOUND = xCenter*4/3;


    
    %----------------------------------------------------------------------
    %                       Timing Information
    %----------------------------------------------------------------------

    % Numer of frames to wait before re-drawing
    WAIT_FRAMES = 1;
    
    % target time in seconds
    TARGET_TIME = 3;
    
    % late/soon feedback time
    FEEDBACK_TIME = 3;
    
    % terminal feedback time
    TERMINAL_TIME = 3;
    


    %----------------------------------------------------------------------
    %                       Conditions
    %----------------------------------------------------------------------
    
    NUM_EXPERIMENTAL_BLOCKS = 2;
    NUM_PRACTICE_BLOCKS = 2;
    
    % have short practice before each block
    num_blocks = NUM_EXPERIMENTAL_BLOCKS + NUM_PRACTICE_BLOCKS;
    
    NUM_PRACTICE_TRIALS = 10;

    TRIALS_PER_CONDITION = 20;
    
    num_trials = NUM_EXPERIMENTAL_BLOCKS * TRIALS_PER_CONDITION + NUM_PRACTICE_BLOCKS * NUM_PRACTICE_TRIALS;
    
    
    
    %----------------------------------------------------------------------
    %                       Occlusion Goggles
    %----------------------------------------------------------------------
    
    % initiate goggles
    PLATO_trial(1);
    PLATO_lens(1);
    
    
    
    %----------------------------------------------------------------------
    %                         Text Rotation
    %----------------------------------------------------------------------
    
    % angle of rotation on screen
    ANGLE = 270;
    
    FONT_SIZE = 28;
    
    
    
    %----------------------------------------------------------------------
    %                       Experimental Loop
    %----------------------------------------------------------------------
    
    data_matrix = [];

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
        
        % define strings for onscreen messages 
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
            Screen('TextSize', window, FONT_SIZE); 
            % get text bounds
            [~, ~, textBounds] = DrawFormattedText(window, sprintf('The following are PRACTICE trials for the %s condition\n\n\nTouch the Screen to Begin the Block', first_condition),...
                'center', 'center', white );
            % reset background screen
            Screen('FillRect', window, black);
            % define texture to hold text 
            textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
            textTexture = Screen('MakeTexture', window, textureRect);
            % set text size for texture 
            Screen('TextSize', textTexture, FONT_SIZE);
            % draw text onto texture 
            DrawFormattedText(textTexture, sprintf('The following are PRACTICE trials for the %s condition\n\n\nTouch the Screen to Begin the Block', first_condition),...
                'center', 'center', white );
            Screen('DrawTextures', window, textTexture, [], [], ANGLE);
            Screen('Flip', window);
            GetClicks; 
            %----------------------------------------------------------------------
            
        elseif block == 2
            
            num_trials_in_this_block = TRIALS_PER_CONDITION;
            
            %------------------- Block Instruction Message ------------------------
            Screen('TextSize', window, FONT_SIZE); 
            % get text bounds
            [~, ~, textBounds] = DrawFormattedText(window, sprintf('The following are EXPERIMENTAL trials for the %s condition\n\n\nTouch the Screen to Begin the Block', first_condition),...
                'center', 'center', white );
            % reset background screen
            Screen('FillRect', window, black);
            % define texture to hold text 
            textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
            textTexture = Screen('MakeTexture', window, textureRect);
            % set text size for texture 
            Screen('TextSize', textTexture, FONT_SIZE);
            % draw text onto texture 
            DrawFormattedText(textTexture, sprintf('The following are EXPERIMENTAL trials for the %s condition\n\n\nTouch the Screen to Begin the Block', first_condition),...
                'center', 'center', white );
            Screen('DrawTextures', window, textTexture, [], [], ANGLE);
            Screen('Flip', window);
            GetClicks; 
            %----------------------------------------------------------------------

        elseif block == 3
            
            num_trials_in_this_block = NUM_PRACTICE_TRIALS;
            
            %------------------- Block Instruction Message ------------------------
            Screen('TextSize', window, FONT_SIZE); 
            % get text bounds
            [~, ~, textBounds] = DrawFormattedText(window, sprintf('The following are PRACTICE trials for the %s condition\n\n\nTouch the Screen to Begin the Block', second_condition),...
                'center', 'center', white );
            % reset background screen
            Screen('FillRect', window, black);
            % define texture to hold text 
            textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
            textTexture = Screen('MakeTexture', window, textureRect);
            % set text size for texture 
            Screen('TextSize', textTexture, FONT_SIZE);
            % draw text onto texture 
            DrawFormattedText(textTexture, sprintf('The following are PRACTICE trials for the %s condition\n\n\nTouch the Screen to Begin the Block', second_condition),...
                'center', 'center', white );
            Screen('DrawTextures', window, textTexture, [], [], ANGLE);
            Screen('Flip', window);
            GetClicks; 
            %----------------------------------------------------------------------
  
        elseif block == 4
            
            num_trials_in_this_block = TRIALS_PER_CONDITION;
            
            %------------------- Block Instruction Message ------------------------
            Screen('TextSize', window, FONT_SIZE); 
            % get text bounds
            [~, ~, textBounds] = DrawFormattedText(window, sprintf('The following are EXPERIMENTAL trials for the %s condition\n\n\nTouch the Screen to Begin the Block', second_condition),...
                'center', 'center', white );
            % reset background screen
            Screen('FillRect', window, black);
            % define texture to hold text 
            textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
            textTexture = Screen('MakeTexture', window, textureRect);
            % set text size for texture 
            Screen('TextSize', textTexture, FONT_SIZE);
            % draw text onto texture 
            DrawFormattedText(textTexture, sprintf('The following are EXPERIMENTAL trials for the %s condition\n\n\nTouch the Screen to Begin the Block', second_condition),...
                'center', 'center', white );
            Screen('DrawTextures', window, textTexture, [], [], ANGLE);
            Screen('Flip', window);
            GetClicks; 
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
                    Screen('TextSize', window, FONT_SIZE); 
                    % get text bounds
                    [~, ~, textBounds] = DrawFormattedText(window, 'Taking a break\n\n\nPress Spacebar To Continue',...
                        'center', 'center', white );
                    % reset background screen
                    Screen('FillRect', window, black);
                    % define texture to hold text 
                    textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                        ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
                    textTexture = Screen('MakeTexture', window, textureRect);
                    % set text size for texture 
                    Screen('TextSize', textTexture, FONT_SIZE);
                    % draw text onto texture 
                    DrawFormattedText(textTexture, 'Taking a break\n\n\nPress Spacebar To Continue',...
                        'center', 'center', white );
                    Screen('DrawTextures', window, textTexture, [], [], ANGLE);
                    Screen('Flip', window);
                    KbStrokeWait;  
                end
            end
            
            % waiting for finger to touch screen
            Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [FIX_X_CENTERED FIX_Y_CENTERED], 2);
            Screen('Flip', window);
            
            % setting touch screen cursor variables
            startbutton=1;
            xfix=0;
            yfix=0;
            buttonstatus=[0 0 0];
            
            % detecting bottom screen press
            while startbutton==1;
               [xfix,yfix,buttonstatus]=GetMouse; %gets x and y position of mouse
               if buttonstatus(1)==0; %status when button is not pressed
                   startbutton=1;
               % status if it is pressed but beyond reasonable bounds
               elseif buttonstatus(1)==1 && (yfix<FIX_Y_CENTERED-FIX_CROSS_DIM_PIX || yfix>FIX_Y_CENTERED+FIX_CROSS_DIM_PIX || xfix<FIX_X_CENTERED-FIX_CROSS_DIM_PIX || xfix>FIX_X_CENTERED+FIX_CROSS_DIM_PIX);  
                   startbutton=1;
               % if button is pressed in proper area 
               else
                   startbutton=0;
                   beep;
               end
            end
          
          
            %---------------------------- ITI -----------------------------
            iti = cond_matrix_shuffled(1,trial);
            iti_time_frames = round(iti / ifi / WAIT_FRAMES);

            Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [FIX_X_CENTERED FIX_Y_CENTERED], 2);
            vbl = Screen('Flip', window);

            for frame = 1:iti_time_frames-1
                Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [FIX_X_CENTERED FIX_Y_CENTERED], 2);
                vbl = Screen('Flip', window, vbl + (WAIT_FRAMES - 0.5) * ifi);
            end
            %--------------------------------------------------------------

            % set early/late variables to default 0
            too_soon = 0;
            too_late = 0;
            
            % make sure the finger is still on touching the screen before
            % presenting target
            [this,that,buttonstatus]=GetMouse;
            if buttonstatus(1)==0 
                
                xtarget=NaN;
                ytarget=NaN;
                too_soon=1;
                
                rt=NaN;
                response_time=NaN;

                Screen('TextSize', window, FONT_SIZE); 
                % get text bounds
                [~, ~, textBounds] = DrawFormattedText(window, 'Too Soon! Wait for the target before starting your movement.',...
                    'center', 'center', white );
                % reset background screen
                Screen('FillRect', window, black);
                % define texture to hold text 
                textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                    ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
                textTexture = Screen('MakeTexture', window, textureRect);
                % set text size for texture 
                Screen('TextSize', textTexture, FONT_SIZE);
                % draw text onto texture 
                DrawFormattedText(textTexture, 'Too Soon! Wait for the target before starting your movement.',...
                    'center', 'center', white );
                Screen('DrawTextures', window, textTexture, [], [], ANGLE);
                Screen('Flip', window);
                WaitSecs(FEEDBACK_TIME); 
                
            else
                
                %--------------------- Draw Target ----------------------------
                Screen('DrawDots', window, [TARGET_X_CENTERED; TARGET_Y_CENTERED], TARGET_SIZE, TARGET_COLOR, [], 4);
                Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [FIX_X_CENTERED FIX_Y_CENTERED], 2);
                Screen('Flip', window);

                % setting touch screen cursor variables
                startbutton=1;
                xtarget=0;
                ytarget=0;
                buttonstatus=[0 0 0];
                movementstart=0;
                
                rt=1;
                response_time=NaN;

                % detecting top screen press
                tic;
                while startbutton==1;
                   [xtarget,ytarget,buttonstatus]=GetMouse; %gets x and y position of mouse
                   
                   if toc>TARGET_TIME;
                       too_late=1;
                       startbutton=0;
                       if rt>=1
                           rt=NaN;
                       end
                       
                       xtarget=NaN;
                       ytarget=NaN;
                       
                       if strcmp(blocking_str, 'n') && (block == 1 || block == 2) 
                            PLATO_lens(1);
                       elseif strcmp(blocking_str, 'v') && (block == 3 || block == 4)  
                            PLATO_lens(1);
                       end
                       
                        Screen('TextSize', window, FONT_SIZE); 
                        % get text bounds
                        [~, ~, textBounds] = DrawFormattedText(window, 'Too Late! You need to reach the target faster.',...
                            'center', 'center', white );
                        % reset background screen
                        Screen('FillRect', window, black);
                        % define texture to hold text 
                        textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                            ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
                        textTexture = Screen('MakeTexture', window, textureRect);
                        % set text size for texture 
                        Screen('TextSize', textTexture, FONT_SIZE);
                        % draw text onto texture 
                        DrawFormattedText(textTexture, 'Too Late! You need to reach the target faster.',...
                            'center', 'center', white );
                        Screen('DrawTextures', window, textTexture, [], [], ANGLE);
                        Screen('Flip', window);
                        WaitSecs(FEEDBACK_TIME); 
                
                   else
                       if buttonstatus(1)==0 && movementstart==0; %status when button is not pressed
                           startbutton=1;
                           % get reaction time 
                           rt = toc;
                           
                           Screen('DrawDots', window, [TARGET_X_CENTERED; TARGET_Y_CENTERED], TARGET_SIZE, TARGET_COLOR, [], 4);
                           Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [FIX_X_CENTERED FIX_Y_CENTERED], 2);
                           Screen('Flip', window);
                           
                           % occlude vision 
                           if strcmp(blocking_str, 'n') && (block == 1 || block == 2)
                               PLATO_trial(0);
                               PLATO_trial(1);
                           elseif strcmp(blocking_str, 'v') && (block == 3 || block == 4)
                               PLATO_trial(0);
                               PLATO_trial(1);
                           end
                           movementstart=1;
                       % not pressed, but in movement
                       elseif buttonstatus(1)==0 && movementstart==1
                           startbutton=1;
                       % pressed but out of range   
                       % response has to be beyond half way point along
                       % midline of screen
                       elseif buttonstatus(1)==1 && (xtarget > TARGET_X_BOUND);  
                           startbutton=1;
                       % pressed and in range
                       else
                           startbutton=0;
                           beep;
                           response_time = toc;
                           % restore vision
                           if strcmp(blocking_str, 'n') && (block == 1 || block == 2)
                                PLATO_lens(1);
                           elseif strcmp(blocking_str, 'v') && (block == 3 || block == 4)
                                PLATO_lens(1);
                           end
                           
                           tic;
                            % detecting screen release
                            while toc < 3;
                               [this,that,buttonstatus]=GetMouse; 
                               if buttonstatus(1)==0 || (buttonstatus(1)==1 && (this > TARGET_X_BOUND)); 
                                   
                                   Screen('DrawDots', window, [TARGET_X_CENTERED; TARGET_Y_CENTERED], TARGET_SIZE, TARGET_COLOR, [], 4);
                                   Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [FIX_X_CENTERED FIX_Y_CENTERED], 2);
                                   Screen('TextSize', window, FONT_SIZE); 
                                    % get text bounds
                                    [~, ~, textBounds] = DrawFormattedText(window, 'Keep your finger where it landed!',...
                                        'center', 'center', white );
                                    % reset background screen
                                    Screen('FillRect', window, black);
                                    % define texture to hold text 
                                    textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
                                        ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
                                    textTexture = Screen('MakeTexture', window, textureRect);
                                    % set text size for texture 
                                    Screen('TextSize', textTexture, FONT_SIZE);
                                    % draw text onto texture 
                                    DrawFormattedText(textTexture, 'Keep your finger where it landed!',...
                                        'center', 'center', white );
                                    Screen('DrawTextures', window, textTexture, [], [], ANGLE);
                                    Screen('Flip', window);   
                                    
                               else
                                   
                                   Screen('DrawDots', window, [TARGET_X_CENTERED; TARGET_Y_CENTERED], TARGET_SIZE, TARGET_COLOR, [], 4);
                                   Screen('DrawLines', window, all_fix_coords, LINE_WIDTH_PIX, FIX_COLOR, [FIX_X_CENTERED FIX_Y_CENTERED], 2);
                                   Screen('Flip', window);
                                   
                               end
                            end
                       end
                   end

                end
                %--------------------------------------------------------------
                
            end
            
            % create long format data row for this trial
            temp = [pilot id age sex hand year month day hour minute seconds blocking block trial iti rt response_time too_soon too_late xfix yfix xtarget ytarget];
            
            % append data matrix
            if trial == 1 && block == 1
                data_matrix = temp;
            else
                data_matrix = [data_matrix; temp];
            end
            
           % write response matrix to csv
           % in trial loop so overwrites each trial
           csvwrite(sprintf('C:/Users/CMP Research/Documents/MATLAB/Ghislain/MSc_ghis_data/%s%i_%s.csv', pilot_str, id, blocking_str), data_matrix);

        end
                       
    end

    % end of experiment message
    Screen('TextSize', window, FONT_SIZE); 
    % get text bounds
    [~, ~, textBounds] = DrawFormattedText(window, 'The experiment is over\n\n\nThe experimenter should be with you shortly',...
        'center', 'center', white );
    % reset background screen
    Screen('FillRect', window, black);
    % define texture to hold text 
    textureRect = ones(ceil((textBounds(4) - textBounds(2)) * 1.1),...
        ceil((textBounds(3) - textBounds(1)) * 1.1)) .* black;
    textTexture = Screen('MakeTexture', window, textureRect);
    % set text size for texture 
    Screen('TextSize', textTexture, FONT_SIZE);
    % draw text onto texture 
    DrawFormattedText(textTexture, 'The experiment is over\n\n\nThe experimenter should be with you shortly',...
        'center', 'center', white );
    Screen('DrawTextures', window, textTexture, [], [], ANGLE);
    Screen('Flip', window); 
    WaitSecs(10);
        
    % deactivate goggles
    PLATO_trial(0)
    
    % turn off screen
    sca;
    
catch
    sca;
    psychrethrow(psychlasterror);
    
    % deactivate goggles
    PLATO_trial(0)
end


