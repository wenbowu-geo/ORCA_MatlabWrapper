clear
% 
system(['install_name_tool -add_rpath ...' ...
    '/opt/homebrew/anaconda3/envs/x86_env/lib ']);
% svp_in
if ~exist('WaterDepth','var'), 
    WaterDepth = 36.33;
end
svp_in.uphalf_cp = 343.0;
svp_in.uphalf_cs = 0.0;
svp_in.uphalf_rho = .00121;
svp_in.uphalf_ap = 0.0;
svp_in.uphalf_as = 0.0;

svp_in.nsvp = 2;
svp_in.ctol = 0;
% svp_in.wssp = [   0 1466;  37.2 1466];  
% svp_in.wssp = [   0 1466;  38.5 1466];  
svp_in.wssp = [   0 1466;  WaterDepth 1466];  
svp_in.wrho = 1.0;
svp_in.walphs = 0;

% svp_in.nlayb = 2;
% svp_in.btm_env = [1  50 1550 1550 0 0 1.5 1.5 -.1 -.1 0 0 0 0 0 0
%                   1 300 1550 1550 0 0 1.5 1.5 -.1 -.1 0 0 0 0 0 0 ];
% svp_in.lowhalf_cp = 1550; svp_in.lowhalf_cs = 0; svp_in.lowhalf_rho = 1.5; svp_in.lowhalf_ap = -0.1; svp_in.lowhalf_as = 0;

sspb1=1460;
sspb2=1600;

svp_in.nlayb = 1;
svp_in.btm_env = [1  10 sspb1 sspb1 0 0 1.5 1.5 -.2 -.2 0 0 0 0 0 0 ];
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
opt_in.nf = -50;
opt_in.fcw_n = 2;
opt_in.fcw = [100 200];

opt_in.nzm = -250;
opt_in.zm_n = 2;
opt_in.zm = [0 100];

iimf = 1;

help ORCA_MEX 

[nMODES, kn, freq, phi, phi_z, vg] = ORCA_MEX(svp_in, opt_in, iimf); 
vg(vg==0)=nan;

figure; clf
% subplot(121); 
% plot(1:min(opt_in.nmode,nMODES(1)),2*pi*opt_in.fcw(1)./real(kn(1:min(opt_in.nmode,nMODES(1)),1)),'x-')
% xlabel('Mode #'); ylabel('Modal phase speed (m/s)')
% subplot(122);
plot(real(phi(:,1:min(opt_in.nmode,nMODES(1)))),phi_z,'linewidth',3)
% plot(real(phi(:,1:3)),phi_z)
ylabel('Depth (m)'); xlabel('Mode functions'); axis tight; grid on; axis ij

phone_depth=WaterDepth-(26.05-[.48 3.25 6.02 8.74 11.5 14.22 16.89 19.13]);
 
hold on; plot(0,phone_depth(2:end),'ro','markerfacecolor','r','markersize',10)
ylim([0 svp_in.wssp(2,1)]); 
