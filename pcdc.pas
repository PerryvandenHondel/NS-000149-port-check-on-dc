//
//	PROGRAM:
//		PortCheckDomainController.exe (pcdc)
//
//	DESCRIPTION:
//		Run this on a domain controller
//		make sure that 
//			adfind.exe
//			portqry.exe
//			nslookup.exe
//		are available in the path
//
//		1) Get all trusts from AD using ADFIND.EXE > GetAllDomainTrusts
//		2) Determine the Domain Controllers of the trusted domains using a DNS nslookup > GetIpsPerDnsDomain
//		3) Check with portqry.exe the ports.
//
//	VERSION:
//		01				2015-06-03	Initial version.
//		01-issue-8		2015-06-11	Use an resolve file to resolve the FQDN of IP addresses.
//
//	
//	
//		portqry -n 10.0.0.1 -e 53 -p UDP
//
//
//	FUNCTIONS AND PROCEDURES:
//		function DoPortQuery
//		function ResolveFqdnDc(ip: string): string;
//		procedure GetAllDomainTrusts
//		procedure PortQueryAdd
//		procedure PortQueryShow
//		procedure PortQueryShowWithResult
//		procedure ExportResultToCsv
//		procedure ExportResultToSql
//		procedure PortQueryOnAll
//		procedure PortAdd
//		procedure PortShow
//		procedure GetIpsPerDnsDomain
//		procedure GetAllDcIpPerDnsDomain
//		function GetFqdn(searchIp: string): string;
//


program PortCheckDomainController;


{$MODE OBJFPC}		
{$H+}				// Use long strings


uses
  Classes, 
  Process, 
  SysUtils,
  USupportLibrary;


const
	TAB = 					#9;
	VERSION =				'01';
	DESCRIPTION =			'PortQueryDomainController';
	ID = 					'149';		
	FNAME_EXTRA =			'pqdc-extra.conf';
	FNAME_PORT = 			'pqdc-port.conf';
	FNAME_FQDN = 			'pqdc-fqdn.conf';
	

type
	RPort = record
		port: string;
		portDescription: string;
		protocol: string;
	end; // of record RPort
	TPort = array of RPort;
	
	RQuery = record				// Record of Query
		checked: string;		// The date and time when checked
		localIp: string;		// The local IP of the current server.
		localHost: string;
		localFqdn: string;		
		remoteIp: string;		// The remote IP to check the port on.
		remoteHost: string;
		remoteFqdn: string;
		port: string;			// The port number to check.
		protocol: string;		// The protocol to test: UDP or TCP.
		status: integer;		// Result of the portqry. 0=OK, 1=NOT LISTENING, 2=FILTERED or LISTENING
	end; // of record RQueryPorts.
	TQuery = array of RQuery;

	RResolve = record
		ip: string;				// IP address
		fqdn: string;			// FQDN of the IP address
	end; // of record RResolve.
	TResolve = array of RResolve;
	
	
var
	arrayQuery: TQuery;
	arrayPort: TPort;
	localIp: string;
	//fileNameOut: string;
	giTotalPortsToCheck: integer;
	//rootDse: string;
	//gbDoResolve: boolean;				// Turn on or off the resolving of IP addresses.
	//gbDoCsv: boolean;
	//gbDoSql: boolean;
	//gsLocalFqdn: string;				// Local FQDN.
	//gaResolve: TResolve;				// 
	thisFqdn: string;					// Contains the current computer FQDN
	thisHost: string;					// Contains the host name of the current computer
	setDateTime: string;				// Contains the date time of program start, used for the set date time. Combine all checks in 1 program run.
		

function DoPortQuery(remoteIp: string; port: string; protocol: string): integer;
//
//	Use PortQry.exe to check if a port is open.
//
// -n <IP or hostname>
// -e <port>
// -p <protocol (TCP or UDP)>
// -q 'quiet' operation runs with no output
//	
// returns 0 if port is listening
// returns 1 if port is not listening
// returns 2 if port is listening or filtered
//
var
	p: TProcess;
begin	
	//WriteLn('DoPortQuery(' + remoteIp + ', ' + port + ', ' + protocol + ')');

	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c portqry.exe -n ' + remoteIp + ' -e ' + port + ' -p ' + protocol + ' -q');

	// Options:
	//	poWaitOnExit:
	//	poNoConsole:
	//	poUsePipes: 
	p.Options := [poWaitOnExit, poUsePipes];

	p.Execute;
	
	DoPortQuery := p.ExitStatus; 
end; // of procedure DoPortQuery.


function ResolveIp(fqdn: string): string;
//
// Resolve a Fully Qualified Domain Name (FQDN) from a IP address.
// Using nslookup.exe.
//		
//		C:\>nslookup 10.4.68.19
//		Server:  ns00nc0102.ns.nl
//		Address:  10.4.34.11
//
//		Name:    ns00dc014.prod.ns.nl
//		Address:  10.4.68.19
//
const
	TMP_FILE = 'nslookup.tmp';
var	
	p: TProcess;
	f: TextFile;
	line: string;	// Read a line from the nslookup.tmp file.
	r: string;		// Result of the function to return.
begin
	// Result will be the ip address when no resolve can be done.
	r := '';
	
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c nslookup -timeout=5 ' + fqdn + '>' + TMP_FILE);
	p.Options := [poWaitOnExit];
	p.Execute;
	
	// Open the text file and read the lines from it.
	Assign(f, TMP_FILE);
	
	{I+}
	Reset(f);
	repeat
		ReadLn(f, line);
		//WriteLn('ResolveFqdn(): ', line);
		if Pos('Name:', line) > 0 then
		begin
			// This is the line with 'Name:' in it.
			// Get the next line that contains the IP
			ReadLn(f, line);
			// Remove 'Name:' from the line and trim the result.
			r := Trim(StringReplace(line, 'Address:', '', [rfIgnoreCase]));
		end;
	until Eof(f);
	Close(f);
	
	// Delete the temp file.
	DeleteFile(TMP_FILE);
	
	ResolveIp := r;
end; // of function ResolveIp.


procedure QueryAdd(newLocalHost: string; newRemoteHost: string; newPort: string; newProtocol: string);
//
//	Add a new record to the ArrayQuery
//
//		newLocalHost
//		newRemoteHost
//		newPort
//		newProtocol
//
var
	x: integer;
begin
	x := Length(arrayQuery);
	
	SetLength(arrayQuery, x + 1); // Increase the size with 1
	
	arrayQuery[x].localHost := newLocalHost;
	arrayQuery[x].remoteHost := newRemoteHost;
	arrayQuery[x].port := newPort;
	arrayQuery[x].protocol := newProtocol;
end; // of procedure QueryAdd


procedure QueryShow();
var
	x: integer;
begin
	WriteLn('QUERYSHOW()');
	for x := 0 to High(arrayQuery) do
	begin
		WriteLn(arrayQuery[x].checked, ' ', arrayQuery[x].localHost, ' (', arrayQuery[x].localIp, ')    ', arrayQuery[x].remoteHost, ' (', arrayQuery[x].remoteIp, ')  ', arrayQuery[x].port, ' ', arrayQuery[x].protocol, ' > ', arrayQuery[x].status);
	end; // of for
end; // of procedure QueryShow


procedure WriteQueryResultsToLog();
var
	x: integer;
	l: Ansistring;
	fname: string;
	f: TextFile;
begin
	fname := GetCurrentComputerName() + '_' + GetDateFs(true) + '.csv';
	
	Assign(f, fname);
	WriteLn(fname);
	
	if FileExists(fname) = false then
	begin
		// Create a new file and write a header to the file.
		ReWrite(f);
		WriteLn(f, 'check_dt;local_ip;local_host;remote_ip;remote_host;port;protocol;result');
	end
	else
		Append(f);
	
	for x := 0 to High(arrayQuery) do
	begin
		l := arrayQuery[x].checked + ';';
		l := l + arrayQuery[x].localIp + ';';
		l := l + arrayQuery[x].LocalHost + ';';
		l := l + arrayQuery[x].remoteIp  + ';';
		l := l + arrayQuery[x].remoteHost + ';';
		l := l + arrayQuery[x].port + ';';
		l := l + arrayQuery[x].protocol + ';';
		if arrayQuery[x].status = 0 then
			l := l + 'LISTENING'
		else
			l := l + 'FILTERED';
		//l := l + IntToStr(arrayQuery[x].status);
		
		WriteLn(f, l); 
	end; // of for
	Close(f);
end; // of procedure WriteQueryResultsToLog


procedure PortAdd(newPort: string; newProtocol: string);
//
// Add a new port to the query array.
//
var
	i: integer;
begin
	i := Length(arrayPort);
	
	SetLength(arrayPort, i + 1);
	
	arrayPort[i].port := newPort;
	arrayPort[i].protocol := newProtocol;
end;


procedure PortShow();
//
// Show all ports to query.
//
var
	i:  integer;
begin
	WriteLn;
	WriteLn('PortShow()');
	for i := 0 to High(arrayPort) do
	begin
		WriteLn(arrayPort[i].port + Chr(9) + arrayPort[i].protocol);
	end;
end;


procedure FillQueryArray();
//
//	Get a list of all domain controllers belonging to the current domain of this domain controller.
//	Use adfind.exe -sc dclist
//
var
	fname: string;
	p: TProcess;
	line: string;
	f: TextFile;
	x: integer;
	remoteHost: string;
begin
	WriteLn('Getting the domain controllers of the current AD domain, please wait...');
	
	fname := 'dc.tmp';
	
	// Create a text file ipdc-domain.tmp
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c adfind.exe -sc dclist >' + fname);
	p.Options := [poWaitOnExit];
	p.Execute;
	
	// Open the text file and read the lines from it.
	Assign(f, fname);
	{I+}
	try
		Reset(f);
		repeat
			ReadLn(f, line);
			//WriteLn(line);
			remoteHost := LeftStr(line, Pos('.', line) - 1);
			
			WriteLn(remoteHost);
			
			if thicHost <> remoteHost then
			//if gThisFqdn <> line then
			begin
				// Only process the other DC's. Do not check yourself!
				// Now add a line per port per server to check
				for x := 0 to High(arrayPort) do
				begin
					//WriteLn(arrayPort[x].port);
					
					QueryAdd(gThisFqdn, line, arrayPort[x].port, arrayPort[x].protocol);
					Inc(giTotalPortsToCheck);
				end; // of for
			end; // of if
			}
			
		
		until Eof(f);
		Close(f);
	except
		on E: EInOutError do
			WriteLn('File ', fname, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end;


procedure GetPorts();
//
//	Read the ports to be checked from the file.
//	Place the ports in a array to access again during the scan.
//
var
	fname: string;
	f: TextFile;
	line: string;
	lineArray: TStringArray;
begin
	WriteLn('Getting all ports to check from config file.');
	fname := 'pcdc-port.conf';
	// Open the text file and read the lines from it.
	Assign(f, fname);
	{I+}
	try
		Reset(f);
		repeat
			ReadLn(f, line);
			//WriteLn(line);
			lineArray := SplitString(line, ';');
			PortAdd(lineArray[0], lineArray[1]);
		until Eof(f);
		Close(f);
	except
		on E: EInOutError do
			WriteLn('File ', fname, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end; // of procedure GetPorts


procedure DoPortCheck();
var
	x: integer;
	r: integer;	// Result of query
begin
	WriteLn('DOPORTCHECK()');
	for x := 0 to High(arrayQuery) do
	begin
		WriteLn(arrayQuery[x].localHost, '   ', arrayQuery[x].remoteHost, '   ', arrayQuery[x].port, ' ', arrayQuery[x].protocol);
		r := DoPortQuery(arrayQuery[x].remoteHost, arrayQuery[x].port, arrayQuery[x].protocol);
		WriteLn('   RESULT>', r);
		
		arrayQuery[x].status := r; // Set the result of the check in the arrayQuery
		
		arrayQuery[x].checked := GetProperDateTime(Now()); // Set the date time of the check in the arrayQuery
	end; // of for
end; // of procedure DoPortCheck


procedure DoResolveIp();
var
	x: integer;
	remoteIp: string;
begin
	WriteLn('DORESOLVEIP()');
	for x := 0 to High(arrayQuery) do
	begin
		remoteIp := ResolveIp(arrayQuery[x].remoteHost);
		
		arrayQuery[x].localIp := localIp;
		arrayQuery[x].remoteIp := remoteIp;
	end; // of for
end; // of procedure DoResolveIp()


procedure ProgInit();
//
//	Program initializer procedure
//
begin
	thisHost := GetCurrentComputerName();
	thisFqdn := UpperCase(thisHost) + '.' + LowerCase(GetDnsDomain());
	localIp := ResolveIp(thisFqdn);
	giTotalPortsToCheck := 0;

	setDateTime := GetProperDateTime(Now());
	
	WriteLn('Computer host name:  ' + thisHost);
	WriteLn('Computer FQDN:       ' + thisFqdn);
	WriteLn('Computer IP address: ' + localIp);
	

	GetPorts();
	//PortShow();
	
	FillQueryArray();
	//QueryShow();
	
	WriteLn('Total number of ports to check: ', giTotalPortsToCheck);
	
end; // of procedure ProgInit


procedure ProgRun();
begin
	WriteLn(ResolveIp('VM60DC002.test.ns.nl'));

	DoPortCheck();
	DoResolveIp();
	QueryShow();
	//WriteQueryResultsToLog();
end;


procedure ProgDone();
begin
end; // of procedure ProgInit


begin
	ProgInit();
	ProgRun();
	//ProgTest();
	//ProgDone();
end. // of program PortQueryDomainController.