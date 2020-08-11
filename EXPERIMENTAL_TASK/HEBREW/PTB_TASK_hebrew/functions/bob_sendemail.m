function bob_sendemail(from, pw, to, subject, message, attach)
% BOB_SENDEMAIL  Send email from a gmail or caltech account
%
% USAGE: bob_sendemail(from, pw, to, subject, message, attach)
%
% ARGUMENTS:
%   from:  the email address to send from
%   pw:  the password for the email address to send from
%   to:  the email address to send to
%   subject:  the email subject line
%   message:  the email message
%   attachment:  the file(s) to attach (string or cell array of strings) [default = '']
%
% Written by Bob Spunt, Februrary 22, 2013
% Based on code provided by Pradyumna
% 
% UPDATES:
%   2013-02-25: added arguments "from" and "pw"; added option for caltech
% ------------------------------------------------------------------
if nargin==5
    attach = '';
elseif nargin<5
    error('USAGE: bob_sendemail(from, pw, to, subject, message, attach)');
end

if regexp(from,'gmail')
    server = 'smtp.gmail.com';
elseif regexp(from,'caltech')
    server = 'smtp-server.its.caltech.edu';
end

% set up SMTP service
setpref('Internet','E_mail',from);
setpref('Internet','SMTP_Server', server);
setpref('Internet','SMTP_Username',from);
setpref('Internet','SMTP_Password',pw);

% server
props = java.lang.System.getProperties;
props.setProperty('mail.smtp.auth','true');
props.setProperty('mail.smtp.socketFactory.class', 'javax.net.ssl.SSLSocketFactory');
props.setProperty('mail.smtp.socketFactory.port', '465');

% send
if isempty(attach)
    sendmail(to,subject,message);
else
    sendmail(to,subject,message,attach)
end

end