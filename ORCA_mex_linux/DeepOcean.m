clear
% 
system(['install_name_tool -add_rpath ...' ...
    '/opt/homebrew/anaconda3/envs/x86_env/lib']);
% svp_in
%if ~exist('WaterDepth','var'), 
%    WaterDepth = 36.33;
%end
svp_in.uphalf_cp = 343.0;
svp_in.uphalf_cs = 0.0;
svp_in.uphalf_rho = .00121;
svp_in.uphalf_ap = 0.0;
svp_in.uphalf_as = 0.0;

%svp_in.nsvp = 2;
svp_in.ctol = 0;
% svp_in.wssp = [   0 1466;  37.2 1466];  
% svp_in.wssp = [   0 1466;  38.5 1466];  
%svp_in.wssp = [   0 1466;  WaterDepth 1466];  

% Specify the filename of sound speed
filename = 'average1D_VpRhoTS.txt'; % Replace with the actual file name

% Read the first two columns from the text file, skipping the header
data = readmatrix(filename, 'NumHeaderLines', 2); % Skips first two lines

% Extract only the first two columns
data = data(:, 1:2);

% Remove rows where the second column (Speed) contains NaN
data = data(~isnan(data(:, 2)), :);

% Assign to svp_in.wssp
svp_in.wssp = data; 

% Extract WaterDepth as the last value in the first row
WaterDepth = svp_in.wssp(end, 1);

% Set the number of sound speed profile points
svp_in.nsvp = size(svp_in.wssp, 1); % Get number of columns

%svp_in.wssp = [   0 1466;  10 1466; WaterDepth 1466;];  

svp_in.wrho = 1.0;
svp_in.walphs = 0;

% svp_in.nlayb = 2;
% svp_in.btm_env = [1  50 1550 1550 0 0 1.5 1.5 -.1 -.1 0 0 0 0 0 0
%                   1 300 1550 1550 0 0 1.5 1.5 -.1 -.1 0 0 0 0 0 0 ];
% svp_in.lowhalf_cp = 1550; svp_in.lowhalf_cs = 0; svp_in.lowhalf_rho = 1.5; svp_in.lowhalf_ap = -0.1; svp_in.lowhalf_as = 0;

sspb1=1900;
sspb2=3200;

svp_in.nlayb = 1;
svp_in.btm_env = [1  200 sspb1 sspb1 500 500 1.3 1.3 -.2 -.2 0 0 0 0 0 0 ];
%                   1  30 sspb2 sspb2 0 0 1.5 1.5 -.2 -.2 0 0 0 0 0 0 ];
svp_in.lowhalf_cp = sspb2;
svp_in.lowhalf_cs = 0;
svp_in.lowhalf_rho = 1.5;
svp_in.lowhalf_ap = -0.2;
svp_in.lowhalf_as = 0;

% svp_in.nlayb = 1;
% svp_in.btm_env = [1.0000e+000  4.6199e+000  1.5850e+003  1.5850e+003            0            0  1.5000e+000  1.5000e+000 -.1 -.1 0 0 0 0 0 0 ];

% svp_in.nlayb = 0;
% svp_in.btm_env = [1.0000e+000  4.6199e+000  1.5850e+003  1.5850e+003            0            0  1.5000e+000  1.5000e+000 -.1 -.1 0 0 0 0 0 0 ];

% svp_in.lowhalf_cp = 1725; svp_in.lowhalf_cs = 0; svp_in.lowhalf_rho = 1.9; svp_in.lowhalf_ap = -0.2; svp_in.lowhalf_as = 0;

svp_in.ntop = 0;
svp_in.above_sea = [1 50  1650 1650 0 0 1.5 1.5 -.1 -.1 0 0 0 0 0 0];

% opt_in
opt_in.nmode = 10; 
opt_in.cphmax = 1600; 
opt_in.rmin = 1.;
opt_in.rmax = 5e3; 
opt_in.phfac = 4;
opt_in.dbcut = 50;
opt_in.Aih_l = -1;
opt_in.Aih_u = -1;

% opt_in.nf = 2; opt_in.fcw_n = 2; opt_in.fcw = [200 500];
opt_in.nf = -10;
opt_in.fcw_n = 2;
opt_in.fcw = [1 20];

opt_in.nzm = -250;
opt_in.zm_n = 2;
opt_in.zm = [0 1.5*WaterDepth];

iimf = 1;

help ORCA_MEX 

[nMODES, kn, freq, phi, phi_z, vg] = ORCA_MEX(svp_in, opt_in, iimf); 
vg(vg==0)=nan;


hold on; % Ensure all plots remain on the same figure
imode_plot = 1;

for ifreq = 1:length(freq)
    plot(real(phi(:, imode_plot, ifreq)), phi_z, 'LineWidth', 3);
end

% Plot ocean bottom line only once
line([min(real(phi(:, imode_plot, :)), [], 'all'), max(real(phi(:, imode_plot, :)), [], 'all')], ...
     [WaterDepth, WaterDepth], 'Color', 'k', 'LineStyle', '--', 'LineWidth', 2);

% Label ocean bottom, moving the text slightly below the line
text(max(real(phi(:, imode_plot, :)), [], 'all') / 3, WaterDepth * 1.1, ...
    'Ocean bottom', 'FontSize', 12, 'Color', 'b');

% Set title with frequency range
title(strcat("Mode ", num2str(imode_plot), " for ", ...
             num2str(freq(1)), " - ", num2str(freq(end)), " Hz"), ...
             'FontSize', 12, 'Color', 'b');

% Label and formatting
ylabel('Depth (m)');
xlabel('Mode functions');
axis tight;
grid on;
set(gca, 'YDir', 'reverse'); % Equivalent to MATLAB's 'axis ij'
hold off; % End hold to avoid affecting future plots



%phone_depth=WaterDepth-(26.05-[.48 3.25 6.02 8.74 11.5 14.22 16.89 19.13]);
 
%hold on; plot(0,phone_depth(2:end),'ro','markerfacecolor','r','markersize',10)
%xlim([0 svp_in.wssp(2,1)]); 