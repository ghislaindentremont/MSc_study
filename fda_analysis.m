%% Load Data
clear all; close all;
% the columns are ["id", "condition", "trial", "y_inter", "x_inter", "z_inter"]
M = csvread('/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/FDA/mat_long_trim.csv', 1, 1);


%% Organize Data
% get trajectory values 
data_mat2 = M(:,4:6);
data_mat = data_mat2(:,[2 1 3]); % switch order to be x,y,z
group_mat = M(:,1:3);

data={};
group_1 = [];
group_2 = [];
i=0;
for id=1:33
    for condition=1:2
        for trial=1:20
            idx = M(:,1) == id & M(:,2) == condition & M(:,3) == trial;
            temp = data_mat(idx, :);
            group_temp = group_mat(idx,:);
            if isempty(temp)
                data = data;
            else
                i = i + 1;
                group_1 = [group_1, group_temp(1,2)];
                group_2 = [group_2, group_temp(1,1)];
                data{i} = temp;
            end
        end
    end
end

% get grouping values 
group={};
group{1} = group_1;
group{2} = group_2;


%% Normalize
%from the normalizeFDA function:
%
%data = cell array where each cell holds the x,y,z (as columns) position
%data for each IR (or tracked marker)

%toNormalize = a list of IRs that you want to normalize - this refers to
%the indexes of the data cell array

%normalizeFrames = number of frames you want for your normalized
%trajectories

%normalizeType
%1 = to time
%2 = to x distance
%3 = to y distance
%4 = to z distance

%frameRate = the frame rate of data collection

toNormalize = 1:length(data); 
normalizeFrames = 200;
normalizeType = 1;
frameRate = 200;

normalizedReach = normalizeFDA(data,toNormalize,normalizeFrames,normalizeType,frameRate);

%% RM FANOVA

% first we structure the data for the fanova.m function
x=[];
y=[];
z=[];

for trial=1:length(normalizedReach)
    traj = normalizedReach{1,trial};
    x = [x; traj(:,1)'];
    y = [y; traj(:,2)'];
    z = [z; traj(:,3)'];
end

% put into a single object
fdaMat.x = x;
fdaMat.y = y;
fdaMat.z = z;

% get subject means
[meansx,newGroupx] = getRMMeans(fdaMat.x,group);
[meansy,newGroupy] = getRMMeans(fdaMat.y,group);
[meansz,newGroupz] = getRMMeans(fdaMat.z,group);

% run ANOVA
[px,corrPx,tx,statsx] = fanovan(meansx, newGroupx, 'model','full', 'random',length(newGroupx),'varnames',{'condition' 'subject'});
[py,corrPy,ty,statsy] = fanovan(meansy, newGroupy, 'model','full', 'random',length(newGroupy),'varnames',{'condition' 'subject'});
[pz,corrPz,tz,statsz] = fanovan(meansz, newGroupz, 'model','full', 'random',length(newGroupz),'varnames',{'condition' 'subject'});

% get stuff for plotting
idynv = find(group{1}==1); %this will be green 
idyv = find(group{1}==2); %this will be black 

%plot in 3D
figure; hold on; 
plot3(mean(fdaMat.x(idynv,:)),mean(fdaMat.y(idynv,:)),mean(fdaMat.z(idynv,:)),'g','linewidth',2);
plot3(mean(fdaMat.x(idyv,:)),mean(fdaMat.y(idyv,:)),mean(fdaMat.z(idyv,:)),'k','linewidth',2);
% "It is worth noting that plotting the grand average of all trials of one
% condition (as shown above) is not exactly representative of a repeated
% measures ANOVA."

%get subject averages (as above when doing FANOVA) but do so for each dimension. 
% Note all the newGroup will be identical
[meansx,newGroupx] = getRMMeans(fdaMat.x,group);
[meansy,newGroupy] = getRMMeans(fdaMat.y,group);
[meansz,newGroupz] = getRMMeans(fdaMat.z,group);

%get the appropriate indexes for each condition. Recall that the %getRMMeans function renumbers your conditions (assings 1 through the %number of unique conditions, in this case 4)
idynv = find(newGroupx{1}==1); %this will be green
idyv = find(newGroupx{1}==2); %this will be black 

%plot in 3D - add in labels and titles
figure; hold on; 
plot3(mean(meansx(idynv,:)),mean(meansy(idynv,:)),mean(meansz(idynv, :)),'g','linewidth',2);
plot3(mean(meansx(idyv,:)),mean(meansy(idyv,:)),mean(meansz(idyv, :)),'k','linewidth',2);

% plot in 2D
figure; hold on; 
plot(mean(meansy(idynv,:)),'g','linewidth',2);
plot(mean(meansy(idyv,:)),'k','linewidth',2);


%% Write Results
cd '/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_results/fanovan_norm_time'

% no-vision trajectories
csvwrite('x_nv.csv', mean(meansx(idynv,:))')
csvwrite('y_nv.csv', mean(meansy(idynv,:))')
csvwrite('z_nv.csv', mean(meansz(idynv,:))')

% vision
csvwrite('x_v.csv', mean(meansx(idyv,:))')
csvwrite('y_v.csv', mean(meansy(idyv,:))')
csvwrite('z_v.csv', mean(meansz(idyv,:))')

% p-values
csvwrite('px.csv', px(1,:)')
csvwrite('py.csv', py(1,:)')
csvwrite('pz.csv', pz(1,:)')

% mean square error 
csvwrite('msx.csv', statsx.msterm(3,:)')
csvwrite('msy.csv', statsy.msterm(3,:)')
csvwrite('msz.csv', statsz.msterm(3,:)')