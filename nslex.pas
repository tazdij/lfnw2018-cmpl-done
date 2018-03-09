unit nslex;

{$mode objfpc}{$H+}

interface

uses chardfa;

type
    ELexTokenType = (
        (* None, is used when there is no Token needed *)
        ELfnwLexNone,

        ELfnwLexOp, ELfnwLexComment, ELfnwLexLabel,

        (* Addresses can be used differently depending on the Op *)
        ELfnwLexHexAddr, ELfnwLexHexLit,
        ELfnwLexDecAddr, ELfnwLexDecLit,


        ELfnwLexReg);
    
    PLnfwLexToken = ^TLnfwLexToken;
    TLnfwLexToken = record
        Name : String[4];
        LexValue : AnsiString;
        LineNum : Cardinal;
        CharNum : Cardinal;
    end;
    
    TLnfwLexTokenArray = Array of TLnfwLexToken;
    
    TLnfwLexer = class(TObject)
        private
            FDfa : TCharDFA;
        public
            constructor Create();
            destructor Destroy(); Override;
            
            function Lex(s : AnsiString) : TLnfwLexTokenArray;
    end;

implementation

uses sysutils, lazutf8;

var
    // CharList : Array[0..1] of AnsiString = ('a', '四');
    DigitCL : Array[0..9] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
    HexCL : Array[0..15] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
    LowerAlphaCL : Array[0..25] of AnsiString = (
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 
        'u', 'v', 'w', 'x', 'y', 'z');
        
    UpperAlphaCL : Array[0..25] of AnsiString = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
        'U', 'V', 'W', 'X', 'Y', 'Z');

    WhitespaceCL : Array[0..2] of AnsiString = (#13, #10, #7);


constructor TLnfwLexer.Create();
var
    tmpComp : TDFAComparator;
    
    CompList : TDFAComp_IsIn;
    CompArray : Array of TDFAComparator;
    OrComp : TDFAComparator;

    StartState, OpState, LabelState,
    CommentState, WhitespaceState : TDFAState;

    WhitespaceDelta : TDFADelta;
begin
    FDfa := TCharDFA.Create();
    
    (* configure DFA to Lex LnfwSource *)
    StartState := TDFAState.Create('START', 'START', Integer(ELfnwLexNone));
    WhitespaceState := TDFAState.Create('WHITESPACE', 'WS', Integer(ELfnwLexNone));
    CommentState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELfnwLexComment));

    OpState := TDFAState.Create('OP', 'OP', Integer(ELfnwLexOp));
    LabelState := TDFAState.Create('LABEL', 'LABEL', Integer(ELfnwLexLabel));


    FDfa.addState(StartState);
    FDfa.addState(WhitespaceState);
    FDfa.addState(CommentState);
    FDfa.addState(OpState);
    FDfa.addState(LabelState);


    (* Loop whitespace back to start, we don't care about it *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), StartState, False));


    
    
    
    
    (* Create test Comparator *)
    SetLength(CompArray, 2);
    
    CompList := TDFAComp_IsIn.Create(LowerAlphaCL);
    CompArray[0] := CompList;
    
    CompList := nil;
    CompList := TDFAComp_IsIn.Create(DigitCL);
    CompArray[1] := CompList;
    
    OrComp := TDFAComp_Or.Create(CompArray);

    
    (* if not CompList.Compare('a') then
        WriteLn('a (', ord('a'), ') is not in CompList')
    else
        WriteLn('a is in CompList');
    
    if not CompList.Compare('A') then
        WriteLn('A is not in CompList');

    if not CompList.Compare('四') then
        WriteLn('Chinese is not in CompList')
    else
        WriteLn('Chinese is in CompList');
    WriteLn('Size Of TDFAToken: ', SizeOf(TDFAToken.TokenId)); *)

    //FreeAndNil(StartState);
    
    OrComp.Free();
    FreeAndNil(OrComp);
    OrComp := nil;
    SetLength(CompArray, 0);
    CompList := nil;
    
end;


destructor TLnfwLexer.Destroy();
begin
    FreeAndNil(Fdfa);
    
    inherited Destroy();
end;

function TLnfwLexer.Lex(s : AnsiString) : TLnfwLexTokenArray;
var
    len : Integer;
    curCodePoint : AnsiString;
    curP, endP : PChar;
    
    curCharNum : Cardinal = 0;
    curLineNum : Cardinal = 1;
begin
    curP := PChar(s);
    endP := curP + Length(s);
    
    while curP < endP do
    begin
        len := UTF8CodePointSize(CurP);
        SetLength(curCodePoint, len);
        Move(curP^, curCodePoint[1], len);
        
        if curCodePoint = #10 then
        begin
            curCharNum := 0;
            curLineNum := curLineNum + 1;
        end
        else if curCodePoint = #13 then
        begin
            (* Ignore cariage return *)
        end
        else
        begin
            Inc(curCharNum);
            WriteLn('Line: ', curLineNum, ', Char: ', curCharNum, ', => ', curCodePoint);
        end;
        //Write(curCodePoint);
        
        (* Pass char into dfa state *)
        self.FDfa.nextChar(curCodePoint);
        
        Inc(curP, len);
    end;
    SetLength(Result, 0);

end;

end.
