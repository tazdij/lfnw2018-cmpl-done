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
    
    PLfnwLexToken = ^TLfnwLexToken;
    TLfnwLexToken = record
        TokenType : ELexTokenType;
        Name : AnsiString;
        LexValue : AnsiString;
        LineNum : Cardinal;
        CharNum : Cardinal;
    end;
    
    TLfnwLexTokenArray = Array of TLfnwLexToken;
    
    TLfnwLexer = class(TObject)
        private
            FDfa : TCharDFA;
            FCurChar, FCurLine: Cardinal;
        public
            constructor Create();
            destructor Destroy(); Override;

            procedure HandleDFAToken(token : PDFAToken);
            
            function Lex(s : AnsiString) : TLfnwLexTokenArray;
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

    WhitespaceCL : Array[0..3] of AnsiString = (#13, #10, #7, #32);


constructor TLfnwLexer.Create();
var
    tmpComp : TDFAComparator;
    
    CompList : TDFAComp_IsIn;
    CompArray : Array of TDFAComparator;
    OrComp : TDFAComparator;

    StartState, WhitespaceState,
    OpState, OpEndState,
    LabelState, LabelEndState,
    CommentState, CommentEndState : TDFAState;

begin
    FDfa := TCharDFA.Create();

    (* Assign the LexToken Generator *)
    FDfa.SetTokenHandler(@Self.HandleDFAToken);
    
    (* configure DFA to Lex LnfwSource *)
    StartState := TDFAState.Create('START', 'START', Integer(ELfnwLexNone));
    WhitespaceState := TDFAState.Create('WHITESPACE', 'WS', Integer(ELfnwLexNone));
    CommentState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELfnwLexComment));
    CommentEndState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELfnwLexComment));

    OpState := TDFAState.Create('OP', 'OP', Integer(ELfnwLexOp));
    OpEndState := TDFAState.Create('OP', 'OP', Integer(ELfnwLexOp));
    LabelState := TDFAState.Create('LABEL', 'LABEL', Integer(ELfnwLexLabel));


    FDfa.addState(StartState); (* Must add the First "Start" State, before all others *)
    FDfa.addState(WhitespaceState);
    FDfa.addState(CommentState);
    Fdfa.AddState(CommentEndState);
    FDfa.addState(OpState);
    FDfa.AddState(OpEndState);
    FDfa.addState(LabelState);


    (* Loop whitespace back to start, we don't care about it *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), StartState, False));

    (* Handle comments *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(';'), CommentState, False));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_IsNot.Create(#10), CommentState));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(#10), CommentEndState, False));

    (* Handle Ops *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    OpState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    OpState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(UpperAlphaCL), OpEndState, False, True));




    
    
    
    
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


destructor TLfnwLexer.Destroy();
begin
    FreeAndNil(Fdfa);
    
    inherited Destroy();
end;

procedure TLfnwLexer.HandleDFAToken(token : PDFAToken);
begin
  WriteLn('#TOKEN: ', token^.TokenName, ' -> ', token^.TokenVal);

end;

function TLfnwLexer.Lex(s : AnsiString) : TLfnwLexTokenArray;
var
    len : Integer;
    curCodePoint : AnsiString;
    curP, endP : PChar;
    reprocessCodePoint : Boolean;
    
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

        reprocessCodePoint := False;
        
        (* Pass char into dfa state *)
        if not self.FDfa.nextChar(curCodePoint, reprocessCodePoint) then
        begin
            WriteLn('Error: no debugging info yet.');
        end;

        if not reprocessCodePoint then
            Inc(curP, len)
        else
        begin

        end;
    end;
    SetLength(Result, 0);

end;

end.
