unit region;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, basic;

type

    TRegionItemRecord = record
        Item:TTuttoInUnoData;
        Indice:PRegionIndex;
        //Indice的内存由TRegion管理，不单独使用
    end;
    PRegionItemRecord = ^TRegionItemRecord;

    TRegion = class(TTuttoInUnoData)
    private
        FDimension: Integer;      //坐标维度，0维时为集合，1维时为线性，2维时为矩阵
        FMaxIndice: PRegionIndex; //0维时为nil，多维时长度与维度相同
        FMinIndice: PRegionIndex; //0维时为nil，多维时长度与维度相同
        FItemsList: TList;
    protected
        function GetMinIndex(Index:Integer):TRegionIndex;
        function GetMaxIndex(Index:Integer):TRegionIndex;
        procedure SetMinIndex(Index:Integer;AValue:TRegionIndex);
        procedure SetMaxIndex(Index:Integer;AValue:TRegionIndex);
        function GetItemCount:Integer;
    public
        function ItemIndice(Item:TTuttoInUnoData):TRegionCoords;
        function HasItem(Item:TTuttoInUnoData):Boolean;
        function GetItem(Indice:TRegionCoords):TTuttoInUnoData;
        function AddItem(Item:TTuttoInUnoData;Indice:TRegionCoords):boolean;
        function RemoveItem(Item:TTuttoInUnoData):Boolean;
        function DeleteItem(Indice:TRegionCoords):Boolean;
        property MinIndex[Index:Integer]:TRegionIndex read GetMinIndex write SetMinIndex;
        property MaxIndex[Index:Integer]:TRegionIndex read GetMaxIndex write SetMaxIndex;
        property ItemCount:Integer read GetItemCount;
        property Dimension:Integer read FDimension;
    public
        constructor Create(ADimension:Integer);
        destructor Destroy; override;
        class function AufTypeName:String; override;
    end;


implementation


{ TRegion }

function TRegion.GetMinIndex(Index:Integer):TRegionIndex;
begin
    if (Index>FDimension) or (Index<0) then raise ETuttoInUnoDataError.Create('TRegion.GetMinIndex 维度超界');
    result:=(FMinIndice+Index)^;
end;

function TRegion.GetMaxIndex(Index:Integer):TRegionIndex;
begin
    if (Index>FDimension) or (Index<0) then raise ETuttoInUnoDataError.Create('TRegion.GetMaxIndex 维度超界');
    result:=(FMaxIndice+Index)^;
end;

procedure TRegion.SetMinIndex(Index:Integer;AValue:TRegionIndex);
begin
    if (Index>FDimension) or (Index<0) then raise ETuttoInUnoDataError.Create('TRegion.SetMinIndex 维度超界');
    (FMinIndice+Index)^:=AValue;
end;

procedure TRegion.SetMaxIndex(Index:Integer;AValue:TRegionIndex);
begin
    if (Index>FDimension) or (Index<0) then raise ETuttoInUnoDataError.Create('TRegion.SetMaxIndex 维度超界');
    (FMaxIndice+Index)^:=AValue;
end;

function TRegion.GetItemCount:Integer;
begin
    result:=FItemsList.Count;
end;

function TRegion.ItemIndice(Item:TTuttoInUnoData):TRegionCoords;
var tmpRec:PRegionItemRecord;
    idx:Integer;
begin
    if FDimension=0 then raise ETuttoInUnoDataError.Create('TRegion.ItemIndice 零维区域不能返回坐标值');
    result:=nil;
    idx:=FItemsList.Count-1;
    while idx>=0 do begin
        tmpRec:=PRegionItemRecord(FItemsList.Items[idx]);
        if tmpRec^.Item.Equals(Item) then begin
            result:=TRegionCoords.Create;
            result.SetCoords(tmpRec^.Indice,FDimension);
            exit;
        end;
        dec(idx);
    end;
end;

function TRegion.HasItem(Item:TTuttoInUnoData):Boolean;
var tmpRec:PRegionItemRecord;
    idx:Integer;
begin
    result:=true;
    idx:=FItemsList.Count-1;
    while idx>=0 do begin
        tmpRec:=PRegionItemRecord(FItemsList.Items[idx]);
        if tmpRec^.Item.Equals(Item) then exit;
        dec(idx);
    end;
    result:=false;
end;

function TRegion.GetItem(Indice:TRegionCoords):TTuttoInUnoData;
var tmpRec:PRegionItemRecord;
    idx:Integer;
begin
    result:=nil;
    if Indice.Dimension<>FDimension then raise ETuttoInUnoDataError.Create('TRegion.GetItem 坐标维度与区域维度不符');
    idx:=FItemsList.Count-1;
    while idx>=0 do begin
        tmpRec:=PRegionItemRecord(FItemsList.Items[idx]);
        if CompareMem(Indice.FValue.datahead,tmpRec^.Indice,FDimension*sizeof(TRegionIndex)) then begin
            result:=tmpRec^.Item;
            exit;
        end;
        dec(idx);
    end;
end;

function TRegion.AddItem(Item:TTuttoInUnoData;Indice:TRegionCoords):Boolean;
var tmpRec:PRegionItemRecord;
    idx:Integer;
begin
    result:=false;
    if Indice.Dimension<>FDimension then raise ETuttoInUnoDataError.Create('TRegion.AddItem 坐标维度与区域维度不符');
    for idx:=FDimension-1 downto 0 do begin
        if (FMinIndice+idx)^>(FMaxIndice+idx)^ then continue;
        if Indice[idx]>(FMaxIndice+idx)^ then exit;
        if Indice[idx]<(FMinIndice+idx)^ then exit;
    end;
    tmpRec:=GetMem(sizeof(TRegionItemRecord));
    tmpRec^.Item:=Item;
    tmpRec^.Indice:=GetMem(FDimension*sizeof(TRegionIndex));
    for idx:=FDimension-1 downto 0 do begin
        (tmpRec^.Indice+idx)^:=Indice[idx];
    end;
    FItemsList.Add(tmpRec);
    result:=true;
end;

function TRegion.RemoveItem(Item:TTuttoInUnoData):Boolean;
var tmpRec:PRegionItemRecord;
    idx:Integer;
begin
    result:=false;
    for idx:=FDimension-1 downto 0 do begin
      tmpRec:=PRegionItemRecord(FItemsList.Items[idx]);
      if tmpRec^.Item.Equals(Item) then begin
          FItemsList.Delete(idx);
          result:=true;
          exit;
      end;
    end;
end;

function TRegion.DeleteItem(Indice:TRegionCoords):Boolean;
var tmpRec:PRegionItemRecord;
    idx:Integer;
begin
    result:=false;
    if FDimension=0 then raise ETuttoInUnoDataError.Create('TRegion.DeleteItem 零维区域无坐标值');
    if Indice.Dimension<>FDimension then raise ETuttoInUnoDataError.Create('TRegion.DeleteItem 坐标维度与区域维度不符');
    for idx:=FDimension-1 downto 0 do begin
        tmpRec:=PRegionItemRecord(FItemsList.Items[idx]);
        if CompareMem(Indice.FValue.datahead,tmpRec^.Indice,FDimension*sizeof(TRegionIndex)) then begin
            FItemsList.Delete(idx);
            result:=true;
            exit;
        end;
    end;
end;

constructor TRegion.Create(ADimension:Integer);
begin
    inherited Create;
    FDimension:=ADimension;
    FMaxIndice:=GetMem(FDimension*sizeof(TRegionIndex));
    FMinIndice:=GetMem(FDimension*sizeof(TRegionIndex));
    if FDimension>0 then begin
        FillByte(FMaxIndice^,FDimension*sizeof(TRegionIndex),255);
        FillByte(FMinIndice^,FDimension*sizeof(TRegionIndex),0);
    end else begin
        FMaxIndice:=nil;
        FMinIndice:=nil;
    end;
    FItemsList:=TList.Create;
end;

destructor TRegion.Destroy;
var idx:integer;
    tmpRec:PRegionItemRecord;
begin
    for idx:=FItemsList.Count-1 downto 0 do begin
        tmpRec:=PRegionItemRecord(FItemsList.Items[idx]);
        FreeMem(tmpRec^.Indice,FDimension*sizeof(TRegionIndex));
        FreeMem(tmpRec,sizeof(TRegionItemRecord));
    end;
    FItemsList.Free;
    FreeMem(FMaxIndice,FDimension*sizeof(TRegionIndex));
    FreeMem(FMinIndice,FDimension*sizeof(TRegionIndex));
    inherited Destroy;
end;

class function TRegion.AufTypeName:String;
begin
    result:='tiu.region';
end;

end.

