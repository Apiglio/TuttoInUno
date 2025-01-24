unit item;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, basic;

type
    TItem = class(TTuttoInUnoData)
        FPosition:TTuttoInUnoData;
    protected
        function GetPosition:TTuttoInUnoData;
        procedure SetPosition(AValue:TTuttoInUnoData);
    public
        property Position:TTuttoInUnoData read GetPosition write SetPosition;
    public
        function MoveTo(ARegion:TTuttoInUnoData;Indice:TRegionCoords):Boolean;
    public
        constructor Create;
        class function AufTypeName:String; override;
    end;

implementation
uses region;

{ TItem }

function TItem.GetPosition:TTuttoInUnoData;
begin
    result:=FPosition;
end;

procedure TItem.SetPosition(AValue:TTuttoInUnoData);
begin
    if not (AValue is TRegion) then raise ETuttoInUnoDataError.Create('TItem.SetPosition argument is not a TRegion');
    FPosition:=AValue;
end;

function TItem.MoveTo(ARegion:TTuttoInUnoData;Indice:TRegionCoords):Boolean;
begin
    result:=false;
    if not (ARegion is TRegion) then raise ETuttoInUnoDataError.Create('TItem.MoveTo 不能将物品移动到区域之外的对象中');
    if FPosition<>nil then TRegion(FPosition).RemoveItem(Self);
    result:=TRegion(ARegion).AddItem(Self,Indice);
end;

constructor TItem.Create;
begin
    inherited Create;
    FPosition:=nil;
end;

class function TItem.AufTypeName:String;
begin
    result:='tiu.item';
end;


end.

