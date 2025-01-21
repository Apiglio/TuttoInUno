unit player;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, auf_type_base, basic, item;

type

    TPlayer = class(TTuttoInUnoData)
    public
        function AskForNumber(min_value, max_value:longint):Longint;
        function AskForItem(Tag:String):TTuttoInUnoData;
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
    stmp:=InputBox('玩家“'+Self.FValue.AsString+'”选择数字','范围（'+IntToStr(min_value)+'-'+IntToStr(max_value)+'):',IntToStr(min_value));
    try
        result:=StrToInt(stmp);
        if (result>max_value) or (result < min_value) then raise ETuttoInUnoDataError.Create('');
    except
        result:=min_value;
    end;
end;

function TPlayer.AskForItem(Tag:String):TTuttoInUnoData;
var idx,res:integer;
    ItemList:TStringList;
    tmpItem:TObject;
begin
    result:=nil;
    ItemList:=TStringList.Create;
    try
        for idx:=TAufBase.Class_InstanceList.Count-1 downto 0 do begin
            tmpItem:=TObject(TItem.Class_InstanceList.Items[idx]);
            if tmpItem is TItem then begin
                if not TItem(tmpItem).HasTag(Tag) then continue;
                ItemList.AddObject(TItem(tmpItem).Value.AsString, tmpItem);
            end;
        end;
        res:=InputCombo(Format('玩家“%s”选择物品',[Self.FValue.AsString]),'请选择',ItemList);
        if res<0 then result:=nil else result:=ItemList.Objects[res] as TTuttoInUnoData;
    finally
        ItemList.Free;
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

class function TPlayerLoop.AufTypeName:String;
begin
    result:='tiu.playerloop';
end;




initialization
    Randomize;

end.

