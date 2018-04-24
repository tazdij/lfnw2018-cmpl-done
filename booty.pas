unit booty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nslex, lexdfa, bingen;

type

  PLfnwLabel = ^TLfnwLabel;
  TLfnwLabel = record
    LabelName : AnsiString;
    LabelPos : Cardinal;
  end;

  PLfnwLabelDefer = ^TLfnwLabelDefer;
  TLfnwLabelDefer = record
    SourcePos : Cardinal;
    LabelName : AnsiString;
  end;


  TLfnwParseGen = class(TObject)
    private
      FLabels : Array of TLfnwLabel;
      FLabelDefers : Array of TLfnwLabelDefer;

      FBinGen : TBinGen;
      FDfa : TLexDFA;

      procedure AddLabelDefer(ALabelName : AnsiString; ASourcePos : Cardinal);
      procedure

      procedure VM_OpNone(ADfaTokens : TDFATokenArray);
      procedure VM_OpHALT(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVRIl(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVHIl(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVHHBx(ADfaTokens : TDFATokenArray);

      procedure VM_OpCALL_A(ADfaTokens : TDFATokenArray);

      procedure VM_OpPRINTHOI(ADfaTokens : TDFATokenArray);
      procedure VM_OpPRINTHOC(ADfaTokens : TDFATokenArray);


      //procedure FreeLabels();
      //procedure NewLabel(id : AnsiString);

    public

      procedure Run(ATokens : TLfnwLexTokenArray);

      constructor Create();
      destructor Destroy(); override;
  end;

implementation

procedure AddLabelDefer(ALabelName : AnsiString; ASourcePos : Cardinal);
begin

end;

procedure TLfnwParseGen.VM_OpNone(ADfaTokens : TDFATokenArray);
begin
  WriteLn('None');
end;

procedure TLfnwParseGen.VM_OpHALT(ADfaTokens : TDFATokenArray);
begin
  WriteLn('HALT.');
  self.FBinGen.WriteByte(0); // Write 00, OpCode for HALT
end;

procedure TLfnwParseGen.VM_OpMOVRIl(ADfaTokens : TDFATokenArray);
begin
  self.FBinGen.WriteByte(3); // Write 01, OpCode for MOV (RIl)
end;

procedure TLfnwParseGen.VM_OpMOVHIl(ADfaTokens : TDFATokenArray);
begin
  WriteLn('MOVHIl.');
  self.FBinGen.WriteByte(1); // Write 01, OpCode for MOV (HIl)
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[2].TokenVal);
end;

procedure TLfnwParseGen.VM_OpMOVHHBx(ADfaTokens : TDFATokenArray);
begin
  WriteLn('MOVHHBx.');
  self.FBinGen.WriteByte(2); // Write 01, OpCode for MOV (HHBx)
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[2].TokenVal);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[3].TokenVal);
end;

procedure TLfnwParseGen.VM_OpCALL_A(ADfaTokens : TDFATokenArray);
begin
  WriteLn('CALL');

  //

end;

(* Handle PRINT *)
procedure TLfnwParseGen.VM_OpPRINTHOI(ADfaTokens : TDFATokenArray);
begin
  WriteLn('PRINTHOI.');
  self.FBinGen.WriteByte(4);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
end;

procedure TLfnwParseGen.VM_OpPRINTHOC(ADfaTokens : TDFATokenArray);
begin
  WriteLn('PRINTHOC.');
  self.FBinGen.WriteByte(5);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
end;

procedure TLfnwParseGen.Run(ATokens : TLfnwLexTokenArray);
var numTokens : Integer = 0;
    lastToken, curToken : PLfnwLexToken;
    reprocessToken : Boolean;
    tmpDFAToken : TDFAToken;
begin
  curToken := @ATokens[0];
  lastToken:= curToken + Length(ATokens);

  while curToken < lastToken do
  begin

    reprocessToken := False;

    tmpDFAToken.TokenCharStart := 0;
    tmpDFAToken.TokenId := Integer(curToken^.TokenType);
    tmpDFAToken.TokenName := Copy(curToken^.Name, 1, Length(curToken^.Name));
    tmpDFAToken.TokenVal := Copy(curToken^.LexValue, 1, Length(curToken^.LexValue));

    (* Pass char into dfa state *)
    if not self.FDfa.nextToken(@tmpDFAToken, reprocessToken) then
    begin
        WriteLn('Error: no debugging info yet.');
    end;

    if not reprocessToken then
        Inc(curToken);

  end;

  // TODO: Loop all TLfnwLabelDefer
  //       Replace 0 with actual address of label

  self.FBinGen.SaveFile('test.bin');

end;

constructor TLfnwParseGen.Create();
var
    StartState, CommentState,
    LabelDefState,
    OpStartState,
    OpHALTState,
    OpMOVStartState, OpMOVRState, OpMOVRIlState,
    OpMOVHState, OpMOVHState2, OpMOVHIlState,
    OpMOVHHState, OpMOVHHBState, OpMOVHHBxState,

    OpPRINTIStartState, OpPRINTIHState, OpPRINTHOIState,
    OpPRINTCStartState, OpPRINTCHState, OpPRINTHOCState : TDFAState;

begin
  SetLength(self.FLabels, 0);
  self.FBinGen := TBinGen.Create();
  self.FDfa := TLexDFA.Create();


  (* Setup the DFA to Parse the LexTokens *)

  StartState := TDFAState.Create('START', 'START', @self.VM_OpNone);
  CommentState := TDFAState.Create('COMMENT', 'COMMENT', @self.VM_OpNone);
  OpStartState := TDFAState.Create('OPSTART', 'OPSTART', @self.VM_OpNone);
  OpHALTState := TDFAState.Create('HALT', 'HALT', @self.VM_OpHALT);
  OpMOVStartState := TDFAState.Create('MOVStart', 'MOVStart', @self.VM_OpNone);
  OpMOVRState := TDFAState.Create('MOVR', 'MOVR', @self.VM_OpNone);
  OpMOVRIlState := TDFAState.Create('MOVRIl', 'MOVRIl', @self.VM_OpMOVRIl);
  OpMOVHState := TDFAState.Create('MOVH', 'MOVH', @self.VM_OpNone);
  OpMOVHState2 := TDFAState.Create('MOVH2', 'MOVH2', @self.VM_OpNone);
  OpMOVHIlState := TDFAState.Create('MOVHIl', 'MOVHIl', @self.VM_OpMOVHIl);
  OpMOVHHState := TDFAState.Create('MOVHH', 'MOVHH', @self.VM_OpNone);
  OpMOVHHBState := TDFAState.Create('MOVHHB', 'MOVHHB', @self.VM_OpNone);
  OpMOVHHBxState := TDFAState.Create('MOVHHBx', 'MOVHHBx', @self.VM_OpMOVHHBx);
  OpPRINTIStartState := TDFAState.Create('PRINTIStart', 'PRINTIStart', @self.VM_OpNone);
  OpPRINTIHState := TDFAState.Create('PRINTIH', 'PRINTIH', @self.VM_OpNone);
  OpPRINTHOIState := TDFAState.Create('PRINTHOI', 'PRINTHOI', @self.VM_OpPRINTHOI);
  OpPRINTCStartState := TDFAState.Create('PRINTCStart', 'PRINTCStart', @self.VM_OpNone);
  OpPRINTCHState := TDFAState.Create('PRINTCH', 'PRINTCH', @self.VM_OpNone);
  OpPRINTHOCState := TDFAState.Create('PRINTHOC', 'PRINTHOC', @self.VM_OpPRINTHOC);

  self.FDfa.AddState(StartState);
  self.FDfa.AddState(CommentState);
  self.FDfa.AddState(OpStartState);
  self.FDfa.AddState(OpHALTState);
  self.FDfa.AddState(OpMOVStartState);
  self.FDfa.AddState(OpMOVRState);
  self.FDfa.AddState(OpMOVRIlState);
  self.FDfa.AddState(OpMOVHState);
  self.FDfa.AddState(OpMOVHState2);
  self.FDfa.AddState(OpMOVHIlState);
  self.FDfa.AddState(OpMOVHHState);
  self.FDfa.AddState(OpMOVHHBState);
  self.FDfa.AddState(OpMOVHHBxState);
  self.FDfa.AddState(OpPRINTIStartState);
  self.FDfa.AddState(OpPRINTIHState);
  self.FDfa.AddState(OpPRINTHOIState);
  self.FDfa.AddState(OpPRINTCStartState);
  self.FDfa.AddState(OpPRINTCHState);
  self.FDfa.AddState(OpPRINTHOCState);

  (* All OP Codes *)
  StartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexOp)), OpStartState, False, True));

  (* All Comments *)
  StartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexComment)), CommentState));

  (* Branch each OP Code By Name *)
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('HALT'), OpHALTState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('MOV'), OpMOVStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('PRINTI'), OpPRINTIStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('PRINTC'), OpPRINTCStartState));

  (* MOVH* OP Codes *)
  OpMOVStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpMOVHState, False));
  OpMOVHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHState2));

  (* MOVHIl Delta *)
  OpMOVHState2.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHIlState));

  (* Add Delta for MOVHHBx Op - extends the MOVH *)
  OpMOVHState2.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpMOVHHState, False));
  OpMOVHHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHHBState));
  OpMOVHHBState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHHBxState));

  (* PRINTHOI *)
  OpPRINTIStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpPRINTIHState, False));
  OpPRINTIHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpPRINTHOIState));

  (* PRINTHOC *)
  OpPRINTCStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpPRINTCHState, False));
  OpPRINTCHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpPRINTHOCState));

end;

destructor TLfnwParseGen.Destroy();
begin
  FreeAndNil(self.FDfa);
  FreeAndNil(self.FBinGen);

  inherited Destroy();
end;

end.

