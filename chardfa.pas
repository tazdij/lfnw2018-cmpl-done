unit chardfa;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, StrUtils;

type
    PDFAToken = ^TDFAToken;
    TDFAToken = record
        TokenId : String[20];
        TokenVal : AnsiString;
        TokenCharStart : Integer;
        TokenLine : Integer;
    end;

    TDFAComparator = class(TObject)
        public
            procedure Free(); Virtual; Abstract;
            function Compare(AInput : AnsiString) : Boolean; Virtual; Abstract;
    end;

    TDFAComp_IsIn = class(TDFAComparator)
        private
            FCharList : Array of AnsiString;
        public
            constructor Create(ACharList : Array of AnsiString);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    TDFAComp_IsNotIn = class(TDFAComparator)
        private
            FCharList : Array of AnsiString;
        public
            constructor Create(ACharList : Array of AnsiString);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    TDFAComp_And = class(TDFAComparator)
        private
            FComparators : Array of TDFAComparator;
        public
            constructor Create(AComps : Array of TDFAComparator);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    TDFAComp_Or = class(TDFAComparator)
        private
            FComparators : Array of TDFAComparator;
        public
            constructor Create(AComps : Array of TDFAComparator);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    (* TDFAComp_None class(TDFAComparator)
        private
            FComparators : Array of TDFAComparator;
        public
            constructor Create(AComps : Array of TDFAComparator);
            destructor Destroy(); Override;
            
            function Compare(AInput : AnsiString) : Boolean; Override;
    end; *)
    
    TDFAState = class;

    TDFADelta = class
        private
            FComparator : TDFAComparator;
            FDestination : TDFAState;
            FAddToBuffer : Boolean;
            FReprocess : Boolean;

        public
            constructor Create(AComparator : TDFAComparator; ADestination : TDFAState; AAddToBuffer : Boolean = True; AReprocess : Boolean = False);
            destructor Destroy(); Override;
    end;
    
    TDFAState = class(TObject)
        private
            FDeltas : Array of TDFADelta;
            FName : AnsiString;
            FIdent : AnsiString;
            FTokenId : Integer;
        public
            function ProcessChar(c : AnsiString) : Boolean;
            procedure AddDelta(delta : TDFADelta);
            constructor Create(AName : AnsiString; AIdent : AnsiString; ATokenId : Integer);
            destructor Destroy(); Override;
            
            
    end;

    TCharDFA = class
        private
            FStates : Array of TDFAState;
            FCurState : TDFAState;
        protected
            
        public
            constructor Create();
            destructor Destroy(); Override;
            
            procedure AddState(state : TDFAState);
            function nextChar(c : AnsiString) : Boolean;
    end;

implementation

constructor TDFAComp_IsIn.Create(ACharList : Array of AnsiString);
var 
    el : AnsiString;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FCharList, Length(ACharList));
    i := 0;

    for el in ACharList do
    begin
       Self.FCharList[i] := el;
       Inc(i); 
    end;

end;

destructor TDFAComp_IsIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_IsIn.Free();
begin
    SetLength(Self.FCharList, 0);
    Self.FCharList := Nil;
end;

function TDFAComp_IsIn.Compare(AInput : AnsiString) : Boolean;
var
    el : AnsiString;
begin
    Result := False;
    for el in Self.FCharList do
    begin
        if el = AInput then
        begin
            Result := True;
            break;
        end;
    end;
end;

constructor TDFAComp_IsNotIn.Create(ACharList : Array of AnsiString);
var 
    el : AnsiString;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FCharList, Length(ACharList));
    i := 0;

    for el in ACharList do
    begin
       Self.FCharList[i] := el;
       Inc(i); 
    end;

end;

destructor TDFAComp_IsNotIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_IsNotIn.Free();
begin
    SetLength(Self.FCharList, 0);
    Self.FCharList := Nil;
end;

function TDFAComp_IsNotIn.Compare(AInput : AnsiString) : Boolean;
var
    el : AnsiString;
begin
    Result := True;
    for el in Self.FCharList do
    begin
        if el = AInput then
        begin
            Result := False;
            break;
        end;
    end;
end;

constructor TDFAComp_And.Create(AComps : Array of TDFAComparator);
var i : Integer;
    comp : TDFAComparator;
begin
    inherited Create();
    
    SetLength(Self.FComparators, Length(AComps));
    i := 0;
    
    for comp in AComps do
    begin
        Self.FComparators[i] := comp;
        Inc(i);
    end;
    
end;

destructor TDFAComp_And.Destroy();
begin

    inherited Destroy();
end;

procedure TDFAComp_And.Free();
begin

end;

function TDFAComp_And.Compare(AInput : AnsiString) : Boolean;
var comp : TDFAComparator;
begin
    Result := True;
    for comp in self.FComparators do
    begin
        if not comp.Compare(AInput) then
        begin
            Result := False;
            break;
        end;
    end;
end;

constructor TDFAComp_Or.Create(AComps : Array of TDFAComparator);
var i : Integer;
begin
    inherited Create();

    SetLength(Self.FComparators, Length(AComps));

    for i := 0 to Length(AComps) - 1 do
    begin
        self.FComparators[i] := AComps[i];
    end;
end;

destructor TDFAComp_Or.Destroy();
begin
    WriteLn('Destroy Or');
    inherited Destroy();
end;

procedure TDFAComp_Or.Free();
var comp : TDFAComparator;
    i : Integer;
begin
    WriteLn('Freeing Or');
    for i := 0 to Length(Self.FComparators) - 1 do
    begin
        comp := Self.FComparators[i];
        //Self.FComparators[i] := nil;
        Self.FComparators[i].Free();
        FreeAndNil(Self.FComparators[i]);
        //comp := nil;
    end;
    
    SetLength(Self.FComparators, 0);
    Self.FComparators := nil;
end;

function TDFAComp_Or.Compare(AInput : AnsiString) : Boolean;
var comp : TDFAComparator;
begin
    Result := False;
    for comp in self.FComparators do
    begin
        if comp.Compare(AInput) then
        begin
            Result := True;
            break;
        end;
    end;
end;

constructor TDFADelta.Create(AComparator : TDFAComparator; ADestination : TDFAState; AAddToBuffer : Boolean = True; AReprocess : Boolean = False);
begin
     self.FComparator := AComparator;
     self.FDestination := ADestination;
     self.FAddToBuffer := AAddToBuffer;
     self.FReprocess := AReprocess;
end;

destructor TDFADelta.Destroy();
begin
    FreeAndNil(self.FComparator);

    inherited Destroy();
end;

constructor TDFAState.Create(AName : AnsiString; AIdent : AnsiString; ATokenId : Integer);
begin

end;

destructor TDFAState.Destroy();
var curDelta : TDFADelta;
    i : Integer;
begin
    for i := 0 to Length(self.FDeltas) - 1 do
    begin
        curDelta := self.FDeltas[i];
        FreeAndNil(curDelta);
    end;
    
    inherited Destroy();
end;

function TDFAState.ProcessChar(c : AnsiString) : Boolean;
var delta : TDFADelta;
begin
    Result := False;

    for delta in self.FDeltas do
    begin
      if delta.FComparator.Compare(c) then
      begin
          Result := True;
          Exit;
      end;
    end;
end;

procedure TDFAState.AddDelta(delta : TDFADelta);
var numDeltas : Integer;
begin
    numDeltas := Length(self.FDeltas);
    SetLength(self.FDeltas, numDeltas + 1);
    self.FDeltas[numDeltas] := delta;
end;

constructor TCharDFA.Create();
begin

end;

destructor TCharDFA.Destroy();
var state : TDFAState;
    i : Integer;
begin
    (* Free up all States *)
    for i := 0 to Length(self.FStates) - 1 do
    begin
        state := self.FStates[i];
        (* Call free on state, then freeandnil *)
        //state.Free();
        FreeAndNil(State);

    end;


    inherited Destroy();
end;

procedure TCharDFA.AddState(state : TDFAState);
var numStates : Integer;
begin
    numStates := Length(self.FStates);
    SetLength(self.FStates, numStates + 1);
    self.FStates[numStates] := state;

    if not Assigned(self.FCurState) then
       self.FCurState := state;
end;

function TCharDFA.nextChar(c : AnsiString) : Boolean;
begin

    (* Test each Delta in the current state, if it can process this char *)
    Result := FCurState.ProcessChar(c);
    (* return if the char was processed correctly *)
    //Result := True;
end;

end.
