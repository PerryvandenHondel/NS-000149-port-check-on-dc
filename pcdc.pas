//
//	PROGRAM:
//		PortQueryDomainController.exe (pqdc)
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
	RCheck = record
		checkDateTime: string;
		localIp: string;
		localHost: string;
		remoteIp: string;
		remoteHost: string;
		port: string;
		protocol: string;
		status: string;
	end; // of Record RCheck

	RPort = record
		port: string;
		portDescription: string;
		protocol: string;
	end; // of record RPort
	TPort = array of RPort;
	
	
	RQueryPorts = record
		checked: string;		// The date and time when checked
		localIp: string;		// The local IP of the current server.
		remoteIp: string;		// The remote IP to check the port on.
		port: string;			// The port number to check.
		protocol: string;		// The protocol to test: UDP or TCP.
		status: integer;		// Result of the portqry. 0=OK, 1=NOT LISTENING, 2=FILTERED or LISTENING
	end; // of record RQueryPorts.
	TQueryPorts = array of RQueryPorts;

	

	RResolve = record
		ip: string;				// IP address
		fqdn: string;			// FQDN of the IP address
	end; // of record RResolve.
	TResolve = array of RResolve;
	
	
var
	arrayQueryPorts: TQueryPorts;
	arrayPort: TPort;
	localIp: string;
	fileNameOut: string;
	giTotalPortsToCheck: integer;
	rootDse: string;
	gbDoResolve: boolean;				// Turn on or off the resolving of IP addresses.
	gbDoCsv: boolean;
	gbDoSql: boolean;
	gsLocalFqdn: string;				// Local FQDN.
	gaResolve: TResolve;				// 
	

function DoPortQuery(remoteIp: string; port: string; protocol: string): integer;
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


procedure GetAllDomainTrusts();
//
//	Use ADFIND to make a list to get all trusts into a file trust.tmp
//
var
	p: TProcess;
	f: TextFile;
begin
	WriteLn;
	WriteLn('GetAllDomainTrusts()');

	p := TProcess.Create(nil);

	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c adfind.exe -b "CN=System,' + rootDse + '" -f "(objectClass=trustedDomain)" trustPartner -csv  -nodn -nocsvheader -nocsvq >trusts.tmp');
	p.Options := [poWaitOnExit];	// Wait until the external program is finished.
	p.Execute;
	
	// Fix for hit own domain! ISSUE-9
	WriteLn('Current DNS Domain is: ', GetDnsDomain());
	AssignFile(f, 'trusts.tmp');
	Append(f); // Open file to add!
	WriteLn(f,  GetDnsDomain());
	CloseFile(f);
end; // of procedure GetAllDomainTrusts


function GetFqdn(searchIp: string): string;
//
// Search for the searchIp, returns the FQDN.
//
// Returns 'N/A' when not found.
//
var
	r: string;
	i: integer;
begin
	r := 'N/A';
	
	for i := 0 to High(gaResolve) do
	begin
		if searchIp = gaResolve[i].ip then
		begin
			r := gaResolve[i].fqdn;
			WriteLn('FOUND FQDN ', r,  ' FOR IP ', searchIp);
			break;
		end;
	end; // of for.
	GetFqdn := r;
end; // of function GetFqdn.


procedure PortQueryAdd(newLocalIp: string; newRemoteIp: string; newPort: string; newProtocol: string);
var
	i: integer;
begin
	i := Length(arrayQueryPorts);
	
	SetLength(arrayQueryPorts, i + 1);
	arrayQueryPorts[i].localIp := newLocalIp;
	arrayQueryPorts[i].remoteIp := newRemoteIp;
	arrayQueryPorts[i].port := newPort;
	arrayQueryPorts[i].protocol := newProtocol;
	
	giTotalPortsToCheck := giTotalPortsToCheck + 1
end; // of procedure PortQueryAdd.


procedure PortQueryShow();
var
	i:  integer;
begin
	WriteLn;
	WriteLn('PortQueryShow()');
	for i := 0 to High(arrayQueryPorts) do
	begin
		WriteLn(arrayQueryPorts[i].localIp + Chr(9) + arrayQueryPorts[i].remoteIp + Chr(9) + arrayQueryPorts[i].port + Chr(9) + arrayQueryPorts[i].protocol);
	end;
end;


procedure PortQueryShowWithResult();
var
	i:  integer;
begin
	WriteLn;
	WriteLn('PortQueryShowWithResult()');
	for i := 0 to High(arrayQueryPorts) do
	begin
		WriteLn(arrayQueryPorts[i].localIp + TAB + arrayQueryPorts[i].remoteIp + TAB + arrayQueryPorts[i].port + TAB + arrayQueryPorts[i].protocol + TAB + IntToStr(arrayQueryPorts[i].status));
	end;
end;


procedure ExportResultToCsv();
//
// Export the data in the arrayQueryPorts to an Excel CSV file.
// Separator char is ; (#59)
//
const
	SEP = #59; // ;

var
	i: integer;
	f: TextFile;
	buffer: string;
begin
	AssignFile(f, fileNameOut + '.csv');

	ReWrite(f);
	
	WriteLn;
	WriteLn('ExportResultToCsv() ' + fileNameOut + '.csv');
	
	// Write the header line in the CSV.
	buffer := 'CheckedOn' + SEP; // Added for issue10
	
	buffer := buffer + 'LocalIp' + SEP;
	if gbDoResolve = true then
		buffer := buffer + 'LocalFqdn' + SEP;
	buffer := buffer + 'RemoteIp' + SEP;
	if gbDoResolve = true then
		buffer := buffer + 'RemoteFqdn' + SEP;
	buffer := buffer + 'Port' + SEP;
	buffer := buffer + 'Protocol' + SEP;
	buffer := buffer + 'Status';
	WriteLn(f, buffer);
	
	for i := 0 to High(arrayQueryPorts) do
	begin
		// Add the checked datetime to the line; issue10
		buffer := arrayQueryPorts[i].checked + SEP;
		
		// Add the local IP to the buffer.
		buffer := buffer + arrayQueryPorts[i].localIp + SEP;
		
		// Add the local FQDN to the buffer.
		if gbDoResolve = true then
			buffer := buffer + gsLocalFqdn + SEP;
		
		// Add the remote Ip to the buffer.
		buffer := buffer + arrayQueryPorts[i].remoteIp + SEP;
		
		// Add the FQDN of the remote IP to the buffer.
		if gbDoResolve = true then
			buffer := buffer + GetFqdn(arrayQueryPorts[i].remoteIp) + SEP;
			
		// Add the port to buffer.
		buffer := buffer + arrayQueryPorts[i].port + SEP;
			
		// Add the protocol to the buffer.
		buffer := buffer + arrayQueryPorts[i].protocol + SEP;
	
		// Add the status of the PortQry to the buffer.
		// Issue#3: fix. Do not export the number but letters for port query status:
		//  L = Listening.
		//	F = Failed listening port.
		if arrayQueryPorts[i].status = 0 then
			buffer := buffer + 'L'
		else
			buffer := buffer + 'F';
	
		//Write to screen.
		//WriteLn(buffer);
		
		// Write to the CSV file.
		WriteLn(f, buffer);
	end;
	CloseFile(f);
end;


procedure ExportResultToSql();
const
	SEP = #59;
var
	i: integer;
	f: TextFile;
	sql: string;
begin
	AssignFile(f, fileNameOut + '.sql');

	ReWrite(f);
	
	WriteLn;
	WriteLn('ExportResultToSql() ' + fileNameOut + '.sql');
	for i := 0 to High(arrayQueryPorts) do
	begin
		//WriteLn(arrayQueryPorts[i].localIp + TAB + arrayQueryPorts[i].remoteIp + TAB + arrayQueryPorts[i].port + TAB + arrayQueryPorts[i].protocol + TAB + IntToStr(arrayQueryPorts[i].status));
		//WriteLn(f,arrayQueryPorts[i].localIp + SEP + arrayQueryPorts[i].remoteIp + SEP + arrayQueryPorts[i].port + SEP + arrayQueryPorts[i].protocol + SEP + IntToStr(arrayQueryPorts[i].status));
		sql := 'INSERT INTO system_port_query ';
		sql := sql + 'SET ';
		//WriteLn(DateTimeToStr(Now));
		//sql := sql + 'check_datetime=''' + GetProperDateTime(Now()) + ''',';
		sql := sql + 'check_datetime=''' +arrayQueryPorts[i].checked + ''',';
		sql := sql + 'local_ip=''' + arrayQueryPorts[i].localIp + ''',';
		sql := sql + 'remote_ip=''' + arrayQueryPorts[i].remoteIp + ''',';
		sql := sql + 'port=' + arrayQueryPorts[i].port + ',';
		sql := sql + 'protocol=''' + arrayQueryPorts[i].protocol + ''',';
		sql := sql + 'status=' + IntToStr(arrayQueryPorts[i].status) + ';';
		
		WriteLn(f, sql);
	end;
	CloseFile(f);
end;


procedure PortQueryOnAll();
var
	i: integer;
	r: integer;
begin
	WriteLn;
	WriteLn('PortQueryOnAll()');
	for i := 0 to High(arrayQueryPorts) do
	begin
		//WriteLn(arrayQueryPorts[i].localIp + Chr(9) + arrayQueryPorts[i].remoteIp + Chr(9) + arrayQueryPorts[i].port + Chr(9) + arrayQueryPorts[i].protocol);
		r := DoPortQuery(arrayQueryPorts[i].remoteIp, arrayQueryPorts[i].port, arrayQueryPorts[i].protocol);
		
		arrayQueryPorts[i].status := r;
		
		// Issue-5: Write the current date time in the checked field.
		arrayQueryPorts[i].checked := GetProperDateTime(Now());
		
		// Fix issue#2: array loop is from 0 to high, display is i + 1.
		//WriteLn(TAB, i + 1, '/', giTotalPortsToCheck, ':', TAB, arrayQueryPorts[i].remoteIp, ' (', arrayQueryPorts[i].port, '/', arrayQueryPorts[i].protocol, ')', TAB, 'RESULT=', r);
		Write(TAB, i + 1, '/', giTotalPortsToCheck, ':', TAB, arrayQueryPorts[i].remoteIp, ' (', arrayQueryPorts[i].port, '/', arrayQueryPorts[i].protocol, ')', TAB, 'RESULT=', r, #13);
	end;
end;


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


procedure GetIpsPerDnsDomain(dns: string);
//
// Do a nslookup and resolve all IP addresses of Domaion Controllers an AD domain.
// Update the array to query the ports with PortQueryAdd().
//
var
	p: TProcess;
	f: TextFile;
	line: String;
	foundName: boolean;
	foundCount: integer;
	i: integer;
	remoteIp: string;
	fname: string;
begin
	WriteLn;
	WriteLn('GetIpsPerDnsDomain(' + dns + ')');

	fname := 'ipdc-' + dns + '.tmp';
	
	Sleep(1000);
	
	// Create a text file ipdc-domain.tmp
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c nslookup -timeout=5 ' + dns + '>' + fname);
	p.Options := [poWaitOnExit];
	p.Execute;
	
	foundName := false;
	
	// Open the text file and read the lines from it.
	Assign(f, fname);
	{I+}
	try
		Reset(f);
		repeat
			ReadLn(f, line);
			//WriteLn('GetIpsPerDnsDomain(): ',  line);
		
			If Pos('Name:', line) > 0 then
			begin
				// When the text 'Name:' is found. All the next lines contain 
				// IP addresses of the Domain Controllers of the domain.
				foundName := true;
				foundCount := 0;
			end;
		
			if foundName = true then
			begin
				foundCount := foundCount + 1;
				if foundCount > 1 then
				begin
					//WriteLn(Chr(9), 'ONLY THIS LINES: ', foundCount, Chr(9), line);
			
					// Remove all text 'Address:' from the line.
					remoteIp := Trim(StringReplace(line, 'Address:', '', [rfIgnoreCase]));
				
					// Remove all text 'Addresses:' from the newLine.
					remoteIp := Trim(StringReplace(remoteIp, 'Addresses:', '', [rfIgnoreCase]));
			
					// Only add a new port to query when:
					// 1) the remoteIP contains data,
					// 2) It's a IPv4 address, skip IPv6 addresses.
					if (Length(remoteIp) > 0) and (Pos(':', remoteIp) = 0) then
					begin
						for i := 0 to High(arrayPort) do
						begin
							// Increase the counter of tests to do.
							//giTotalPortsToCheck := giTotalPortsToCheck + 1;
							PortQueryAdd(localIp, remoteIp, arrayPort[i].port, arrayPort[i].protocol);
						end;
					end;
				end;
			end;
		until Eof(f);
		Close(f);
	except
		on E: EInOutError do
			WriteLn('File ', fname, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end; // of procedure GetIpsPerDnsDomain.


procedure GetAllDcIpPerDnsDomain();
//
//	Open the file trusts.tmp and obtain all IP addresses of domain controllers from a a domain.
//
var
	f: TextFile;
	line: String;
	fname: string;
begin
	WriteLn;
	WriteLn('GetAllDcsPerDnsDomain()');
	
	fname := 'trusts.tmp';
	Assign(f, fname);
	
	{I+}
	try
		Reset(f);
		repeat
			ReadLn(f, line);
			//WriteLn(s);
			GetIpsPerDnsDomain(LowerCase(line));
		until Eof(f);
		Close(f);
	except
		on E: EInOutError do
			WriteLn('File ', fname, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end; // of procedure GetAllDcsPerDnsDomain.


procedure ReadResolveConfig();
//
//	Read the fqdn resolve file and place in the array.
//
var
	f: TextFile;
	l: string;
	i: integer;
	aLine: TStringArray;
begin
	WriteLn('ReadResolveConfig(): Trying to open file: ', FNAME_FQDN);
	
	SetLength(aLine, 0);
	
	Assign(f, FNAME_FQDN);
	{I+}
	try
		Reset(f);
		WriteLn('Opened');
		repeat
			ReadLn(f, l);
			WriteLn(l);
		
			aLine := SplitString(l, ';');
		
			i := Length(gaResolve);
			SetLength(gaResolve, i + 1);
		
			gaResolve[i].ip := aLine[0];
			gaResolve[i].fqdn := aLine[1];
		until Eof(f);
		Close(f);
	
		// Show the contents of the array gaResolve.
		for i := 0 To High(gaResolve) do
		begin
			WriteLn(gaResolve[i].ip, TAB, gaResolve[i].fqdn);
		end; // of for.
	except
		on E: EInOutError do
			WriteLn('File ', FNAME_FQDN, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end;


procedure ReadConfigPort();
var
	f: TextFile;
	l: string;
	lineArray: TStringArray;
begin
	// Issue-4: Read the ports to query per DC from a config file.
	// Set the lienArray on 0 (Clear it)
	SetLength(lineArray, 0);
	AssignFile(f, FNAME_PORT);
	WriteLn('ReadConfigPort(): Trying to open file ', FNAME_PORT);
	try
		Reset(f);
		WriteLn('Openend');
		repeat
			ReadLn(f, l);
			// Split the line into the lineArray
			lineArray := SplitString(l, ';');
		
			PortAdd(lineArray[0], lineArray[1]);
		until Eof(f);
		CloseFile(f);
	except
		on E: EInOutError do
			WriteLn('File ', FNAME_PORT, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end;  // of procedure ReadConfigPort()


procedure ReadConfigExtra();
var
	f: TextFile;
	l: string;
	lineArray: TStringArray;
	fname: string;
begin
	// Issue-4: Read extra systems and ports from config file.
	// Set the lienArray on 0 (Clear it)
	SetLength(lineArray, 0);
		
	fname := 'pqdc-extra.conf';
	AssignFile(f, fname);
	{I+}
	try 
		Reset(f);
		repeat
			ReadLn(f, l);
			// Split the line into the lineArray
			lineArray := SplitString(l, ';');
		
			PortQueryAdd(localIp, lineArray[0], lineArray[1], lineArray[2]);
		until Eof(f);
		CloseFile(f);
	except
		on E: EInOutError do
			WriteLn('File ', fname, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end; // of procedure ReadConfigExtra


procedure ProgramTitle();
begin
	WriteLn();
	WriteLn(StringOfChar('-', 80));
	WriteLn(UpperCase(GetProgramName()) + ' -- Version: ' + VERSION + ' -- Unique ID: ' + ID);
	WriteLn(DESCRIPTION);
	WriteLn(StringOfChar('-', 80));	
end; // of procedure ProgramTitle()


procedure ProgramUsage();
var
	p:	string;
begin
	p := ParamStr(0);

	WriteLn();
	WriteLn('Switches:');
	WriteLn(TAB + '--output-csv             Output in CSV format, separator char is '';''');
	WriteLn(TAB + '--output-sql             Output in SQL format');
	WriteLn(TAB + '--resolve-fqdn          	Also resolve the IP address to FQDN''s');
	WriteLn(TAB + '--help, -h, -?           Show the help');
	WriteLn();
	WriteLn('Usage:');
	WriteLn(TAB + p + ' [switche(s)]');
	WriteLn();
	WriteLn('Examples:');
	WriteLn(TAB + p + ' --output-csv               Run the program an output in a CSV file.');
	WriteLn(TAB + p + ' --output-csv --resolve     Run the program an output in a CSV file and resolve IP addresses to a FQDN''s.');
	WriteLn(TAB + p + ' --output-sql               Run the program an output in a SQL import file.');
	WriteLn;
end; // of procedure ProgramUsage()


procedure GetDomainControllers();
//
//	Get a list of all domain controllers belonging to the current domain of this domain controller.
//	Use adfind.exe -sc dclist
//
var
	fname: string;
	p: TProcess;
	line: string;
	f: TextFile;
begin
	WriteLn('Getting the domain controllers of the current AD domain, please wait...');
	fname := 'dc.tmp';
	
	Sleep(1000);
	
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
			WriteLn(line);
		
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
//
var
	fname: string;
	f: TextFile;
	line: string;
begin
	fname := 'pcdc-port.conf';
	// Open the text file and read the lines from it.
	Assign(f, fname);
	{I+}
	try
		Reset(f);
		repeat
			ReadLn(f, line);
			WriteLn(line);
		
		until Eof(f);
		Close(f);
	except
		on E: EInOutError do
			WriteLn('File ', fname, ' handeling error occurred, Details: ', E.ClassName, '/', E.Message);
	end;
end; // of procedure GetPorts



procedure ProgInit();
//
//	Program initializer procedure
//
begin
	GetPorts();
	GetDomainControllers();
end; // of procedure ProgInit


procedure ProgRun();
begin
	// Query all domain trusts
	GetAllDomainTrusts();

	// Query all DC's per domain trust using NSLOOKUP.
	GetAllDcIpPerDnsDomain();
	
	//PortQueryShow();
	WriteLn('There are ', giTotalPortsToCheck, ' ports found to be tested.');
	
	// Perform a port query per port.
	PortQueryOnAll();
	
	// Make all the output as requested by the command line options.
	if gbDoCsv = true then
		// Output to CSV when selected on the command line.
		ExportResultToCsv();
		
	if gbDoSql = true then
		// Output to SQL when selected on the command line.
		ExportResultToSql();
end; // of procedure ProgInit


procedure ProgTest();
begin
	//WriteLn(GetBaseDn());
	//WriteLn(ResolveFqdn('10.145.193.15'));
	
	WriteLn('PROGTEST()');
	ReadConfigPort();
	ReadResolveConfig();
	//localIp := GetLocalIp();
	//WriteLn('LOCALIP=', localIp);
	
	//gsLocalFqdn := ResolveFqdnDc(localIp);
	//WriteLn('gsLocalFqdn=', gsLocalFqdn);
	
	
	//WriteLn(GetFqdn('10.4.34.12'));
	
	{
	ip := '10.4.68.17';
	WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));

	ip := '10.4.68.20';
	WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));

	ip := '10.4.68.14';
	WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));
	}
	//ip := '10.4.68.16';
	//WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));
end; // of procedure ProgTest


procedure ProgDone();
begin
end; // of procedure ProgInit


begin
	ProgInit();
	//ProgRun();
	//ProgTest();
	//ProgDone();
end. // of program PortQueryDomainController.