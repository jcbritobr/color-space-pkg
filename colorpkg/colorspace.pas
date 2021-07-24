unit ColorSpace;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, LCLType;

type
  IColorSpace = interface
    ['{775DDA6A-0019-4145-8136-FB1747684797}']
    function ToRGBTriple: TRGBTriple;
    procedure FromRGB(const ARed, AGreen, ABlue: Byte);
  end;

  TChangeValueEvent = procedure(ASender: TObject) of object;

implementation

end.

