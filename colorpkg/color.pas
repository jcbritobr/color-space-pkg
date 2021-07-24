{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit color;

{$warn 5023 off : no warning about unused units}
interface

uses
  HSV, CMY, ColorSpace, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('color', @Register);
end.
