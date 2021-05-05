unit logoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Console, GlobalTypes, threads;

var
  ConsoleActivated : boolean = false;
  writelock : tspinhandle;

procedure Log(str : string);

implementation

uses
  Platform;

procedure Log(str : string);
//var
//  s : string;
begin
    try
      SpinLockIRQ(writelock);
      LoggingOutput(str);
    finally
      SpinUnlockIRQ(writelock)
    end;
end;

initialization
  writelock := spincreate;


end.

