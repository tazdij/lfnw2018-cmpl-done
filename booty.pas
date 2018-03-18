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


  TLfnwParseGen = class(TObject)
    private
      FLabels : Array of TLfnwLabel;

      FBinGen : TBinGen;
      FDfa : TLexDFA;

      procedure VM_OpNone(ADfaTokens : TDFATokenArray);
      procedure VM_OpHALT(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVRIl(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVHIl(ADfaTokens : TDFATokenArray);

      //procedure FreeLabels();
      //procedure NewLabel(id : AnsiString);

    public

      procedure Run(ATokens : TLfnwLexTokenArray);

      constructor Create();
      destructor Destroy(); override;
  end;

implementation

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
  self.FBinGen.WriteByte(10); // Write 01, OpCode for MOV (RIl)
end;

procedure TLfnwParseGen.VM_OpMOVHIl(ADfaTokens : TDFATokenArray);
begin
  WriteLn('MOVHIl.');
  self.FBinGen.WriteByte(1); // Write 01, OpCode for MOV (RIl)
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[2].TokenVal);
end;

(* Handle PRINT *)


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

  self.FBinGen.SaveFile('test.bin');

end;

constructor TLfnwParseGen.Create();
var
    StartState, CommentState,
    LabelDefState,
    OpStartState,
    OpHALTState,
    OpMOVStartState, OpMOVRState, OpMOVRIlState,
    OpMOVHState, OpMOVHState2, OpMOVHIlState : TDFAState;

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

  StartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexOp)), OpStartState, False, True));
  StartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexComment)), CommentState));

  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('HALT'), OpHALTState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('MOV'), OpMOVStartState));

  OpMOVStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpMOVHState, False));
  OpMOVHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHState2));
  OpMOVHState2.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHIlState));


end;

destructor TLfnwParseGen.Destroy();
begin
  FreeAndNil(self.FDfa);
  FreeAndNil(self.FBinGen);

  inherited Destroy();
end;

end.

