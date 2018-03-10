{%RunCommand $MakeExe($(EdFile)) ./example/test.s}
program cmpl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, chardfa, nslex;

function ReadTextFile(path : AnsiString) : AnsiString;
var
    f : TextFile;
    s : AnsiString;
begin

    Result := '';
    if FileExists(path) then
    begin
        AssignFile(f, path);

        (* Open for reading at top *)
        Reset(f);

        while not eof(f) do
        begin
            readln(f, s);
            Result := Result + s + #10;
        end;
    end;
end;

var
    lexer : TLfnwLexer;
    src : AnsiString;

begin

  lexer := TLfnwLexer.Create();

  src := ReadTextFile(ParamStr(1));
  WriteLn('File: ', ParamStr(1));
  WriteLn(src);

  lexer.Lex(src);

  FreeAndNil(lexer);
  src := '';

end.

