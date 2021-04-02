unit logoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Console, GlobalTypes, threads;

var
  ConsoleActivated : boolean = false;
  WindowHandle : TWindowHandle = 0;
  writelock : tspinhandle;

procedure Log(str : string; isinterrupt : boolean = false);

implementation

procedure Log(str : string; isinterrupt : boolean = false);
var
  s : string;
begin
  s := DateTimeToStr(Now) +': ' + str;

  if (WindowHandle <> 0) then
  begin
    try
      // I'm not even sure if I've interpreted the descriptions in the threads unit
      // regarding how to make spinlocks interrupt-safe here.
      if not isinterrupt then
        SpinLockFIQ(writelock)
      else
        spinlock(writelock);

      ConsoleWindowWriteLn(WindowHandle, s);
    finally
      if not isinterrupt then
        SpinUnlockFIQ(writelock)
      else
        spinunlock(writelock);
    end;
  end;
end;

initialization
  writelock := spincreate;


end.

