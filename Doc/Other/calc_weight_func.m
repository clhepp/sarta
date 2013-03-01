% Calculate the weighting function for a channel
% Note: requires the layer transmittances which are NOT normally
% output by sarta.  This example code assumes SARTA has been
% modified to write out the transmittances for one channel to
% prof.gas_5

clear

% Load RTP input file
[head, hattr, prof, pattr] = rtpread('sarta_out.rtp');

% Pull out required profile data
nlevs=prof.nlevs;
nobs=length(nlevs);

% If the bottom fractional layer is too thin, SARTA doesn't use it and
% instead uses an expanded next to last layer as the last layer.  Find
% those cases by checking where the last layer transmittance is garbage.
for ii=1:nobs
   last=prof.gas_5(nlevs(ii),ii);
   if (last > 1 | last < 0);
      nlevs(ii)=nlevs(ii)-1;
   end
end
mlevs=max(nlevs);
nlays=nlevs-1;
mlays=max(nlays);


% Load channel layer transmittance
% I put tau for channel 557 into gas_5.
tau=prof.gas_5(1:mlays,:);


plevs=prof.plevs(1:mlevs,:);

% Replace the bottom grid pressure level with the actual surface pressure
for ii=1:nobs
   plevs(nlevs(ii),ii)=prof.spres(ii);
end


tauz=ones(mlevs,nobs);
tauz(2:mlevs,:)=tau;
for ii=2:mlevs
   tauz(ii,:)=tauz(ii,:).*tauz(ii-1,:);
end
dtz=tauz(1:mlays,:)-tauz(2:mlevs,:);


lnp=log(plevs);
dlnp=lnp(1:mlays,:)-lnp(2:mlevs,:);


wf=zeros(mlays,nobs);
for ii=1:nobs
   ind=1:nlays(ii);
   wf(ind,ii) = dtz(ind,ii)./dlnp(ind,ii);
end

%%% end of program %%%
