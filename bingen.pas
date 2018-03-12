unit bingen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type


    TBinGen = class(TObject)
        private
            FFile : File of Byte;
        public

            constructor Create(AFilename : AnsiString);
            destructor Destroy(); override;
    end;

implementation

constructor TBinGen.Create(AFilename : AnsiString);
begin

end;

destructor TBinGen.Destroy();
begin

end;

end.

