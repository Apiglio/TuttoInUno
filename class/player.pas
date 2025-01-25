unit player;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, auf_type_base, basic, item, region;

type

    TPlayer = class(TTuttoInUnoData)
    public
        function AskForNumber(min_value, max_value:longint):Longint;
        function AskForItem(Tag:String):TItem;
        function AskForCoords(region:TRegion):TRegionCoords;
        procedure Win;
    public
        class function AufTypeName:String; override;
    end;

    TPlayerLoop = class(TTuttoInUnoList)
        FCursor:Integer;
    protected
        function GetCurrent:TPlayer;
    public
        procedure SetRandomCursor;
        procedure NextPlayer;
        procedure PrevPlayer;
        property CurrentPlayer:TPlayer read GetCurrent;
    public
        class function AufTypeName:String; override;
    end;

implementation
uses Dialogs;

{ TPlayer }

function TPlayer.AskForNumber(min_value, max_value:longint):Longint;
var stmp:string;
begin
    stmp:=InputBox('玩家“'+Self.Value.ToString+'”选择数字','范围（'+IntToStr(min_value)+'-'+IntToStr(max_value)+'):',IntToStr(min_value));
    try
        result:=StrToInt(stmp);
        if (result>max_value) or (result < min_value) then raise ETuttoInUnoDataError.Create('');
    except
        result:=min_value;
    end;
end;

function TPlayer.AskForItem(Tag:String):TItem;
var idx,res:integer;
    ItemList:TStringList;
    tmpItem:TObject;
    dispname:string;
    tmpRegion:TRegion;
    tmpCoords:TRegionCoords;
begin
    result:=nil;
    ItemList:=TStringList.Create;
    try
        for idx:=TAufBase.Class_InstanceList.Count-1 downto 0 do begin
            tmpItem:=TObject(TAufBase.Class_InstanceList.Items[idx]);
            if tmpItem is TItem then begin
                if not TItem(tmpItem).HasTag(Tag) then continue;
                dispname:=TItem(tmpItem).Value.ToString;
                tmpRegion:=TItem(tmpItem).Position as TRegion;
                if tmpRegion<>nil then begin
                    dispname:=dispname+' - ';
                    dispname:=dispname+tmpRegion.Value.ToString;
                    tmpCoords:=tmpRegion.ItemIndice(tmpItem as TItem);
                    if tmpCoords<>nil then dispname:=dispname+' '+tmpCoords.Value.ToString;
                    tmpCoords.Free;
                end;
                ItemList.AddObject(dispname, tmpItem);
            end;
        end;
        res:=InputCombo(Format('玩家“%s”选择物品',[Self.Value.ToString]),'请选择',ItemList);
        if res<0 then result:=nil else result:=ItemList.Objects[res] as TItem;
    finally
        ItemList.Free;
    end;
end;

function TPlayer.AskForCoords(region:TRegion):TRegionCoords;
var stmp:string;
    idx,dimens:integer;
    pindex:PRegionIndex;
    coords_str:TStringList;
begin
    dimens:=region.Dimension;
    result:=nil;
    if dimens<1 then exit;
    coords_str:=TStringList.Create;
    coords_str.Delimiter:=',';
    pindex:=GetMem(dimens*SizeOf(TRegionIndex));
    try
      while true do begin
        try
            stmp:=InputBox('玩家“'+Self.Value.ToString+'”选择坐标','坐标值(逗号隔开, 维数='+IntToStr(dimens)+'):','');
            if stmp='' then begin
                result:=ZeroRegionCoords();
                exit;
            end else begin
                coords_str.CommaText:=stmp;
                if coords_str.Count<>dimens then begin
                    ShowMessage('输入坐标维度不符，请重新输入');
                end else begin
                    for idx:=coords_str.Count-1 downto 0 do (pindex+idx)^:=StrToInt(coords_str[idx]);
                    result:=TRegionCoords.Create;
                    result.SetCoords(pindex,dimens);
                    if region.IndiceValid(result) then exit
                    else ShowMessage('输入坐标不在区域有效坐标范围内，请重新输入'); ;
                end;
            end;
        except
            ShowMessage('输入坐标格式有误，请重新输入');
        end;
      end;
    finally
        coords_str.Free;
        FreeMem(pindex,dimens*sizeof(TRegionIndex));
    end;
end;

procedure TPlayer.Win;
begin
  //确实还不知道要做什么，可能是计分的事吧
end;

class function TPlayer.AufTypeName:String;
begin
    result:='tiu.player';
end;


{ TPlayerLoop }

function TPlayerLoop.GetCurrent:TPlayer;
begin
    result:=TPlayer(PTuttoInUnoDirectData(FList.Items[FCursor])^.AsObject);
end;

procedure TPlayerLoop.SetRandomCursor;
var count:Integer;
begin
    count:=Self.ItemCount;
    if count<1 then raise ETuttoInUnoDataError.Create('TPlayerLoop.SetRandomCursor 无元素');
    FCursor:=Random(count);
end;

procedure TPlayerLoop.NextPlayer;
begin
    FCursor:=FCursor+1;
    if FCursor>=Self.ItemCount then FCursor:=0;
end;

procedure TPlayerLoop.PrevPlayer;
begin
    FCursor:=FCursor-1;
    if FCursor<0 then FCursor:=Self.ItemCount-1;
end;

class function TPlayerLoop.AufTypeName:String;
begin
    result:='tiu.playerloop';
end;




initialization
    Randomize;

end.

