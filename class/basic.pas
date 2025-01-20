unit basic;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
    Classes, SysUtils, auf_type_base;

type

    TTIUDirectDataType = (dtUnknown, dtInteger, dtFloat, dtString, dtObject, dtNil);
    PTuttoInUnoDirectData = ^TTuttoInUnoDirectData;
    TTuttoInUnoDirectData = record
        datatype:TTIUDirectDataType;
        datasize:Integer;
        datahead:PByte;
    private
        procedure SetInteger(value:Integer);
        procedure SetDouble(value:Double);
        procedure SetString(value:String);
        procedure SetObject(value:TObject);
        function GetInteger:Integer;
        function GetDouble:Double;
        function GetString:String;
        function GetObject:TObject;
    public
        procedure Initialize;
        procedure Finalize;
        procedure Assign(source:TTuttoInUnoDirectData);
    public
        function ToString:string;
        property AsInteger:Integer read GetInteger write SetInteger;
        property AsDouble:Double read GetDouble write SetDouble;
        property AsString:String read GetString write SetString;
        property AsObject:TObject read GetObject write SetObject;
    end;

    ETuttoInUnoDataError = Exception;

    TTuttoInUnoData = class(TAufBase)
        FValue:TTuttoInUnoDirectData;
        FAttributeMap:TStringList;
        FTags:TStringList;
    protected
        procedure SetData(Key:string;Value:TTuttoInUnoDirectData);
        function GetData(Key:string):TTuttoInUnoDirectData;
        procedure SetObject(Key:string;Value:TObject);
        function GetObject(Key:string):TObject;
        procedure SetInteger(Key:string;Value:Integer);
        function GetInteger(Key:string):Integer;
        procedure SetDouble(Key:string;Value:Double);
        function GetDouble(Key:string):Double;
        procedure SetString(Key:string;Value:String);
        function GetString(Key:string):String;
    public
        function ToString:ansistring; override;
        property Value:TTuttoInUnoDirectData read FValue;
        property Data[Key:string]:TTuttoInUnoDirectData read GetData write SetData;
        property Objects[Key:string]:TObject read GetObject write SetObject;
        property Integers[Key:string]:Integer read GetInteger write SetInteger;
        property Doubles[Key:string]:Double read GetDouble write SetDouble;
        property Strings[Key:string]:String read GetString write SetString;
    public
        function Equals(Obj:TTuttoInUnoData):boolean; virtual;
        procedure AddTag(Tag:String);
        procedure DelTag(Tag:String);
        function HasTag(Tag:String):Boolean;
        procedure AssignAttributes(ASource:TTuttoInUnoData);
        constructor Create;
        destructor Destroy; override;
        class function AufTypeName:String; override;
    end;

    TTuttoInUnoList = class(TTuttoInUnoData)
        FList:TList;
    protected
        function GetItem(Index:Integer):TTuttoInUnoDirectData;
        procedure SetItem(Index:Integer;Item:TTuttoInUnoDirectData);
        function GetItemCount:Integer;
    public
        procedure Append(Item:PTuttoInUnoDirectData); virtual;
        procedure Insert(Index:Integer;Item:PTuttoInUnoDirectData); virtual;
        procedure Delete(Index:Integer); virtual;
        property ItemCount:Integer read GetItemCount;
        property Items[Index:Integer]:TTuttoInUnoDirectData read GetItem write SetItem;
    public
        constructor Create;
        destructor Destroy; override;
        class function AufTypeName:String; override;
    end;

    TData = class(TTuttoInUnoData)
    public
        class function AufTypeName:String; override;
    end;

    TRegionIndex = Integer;
    PRegionIndex = ^TRegionIndex;

    function NewDirectNil:PTuttoInUnoDirectData;
    function NewDirectInteger(value:Integer):PTuttoInUnoDirectData;
    function NewDirectFloat(value:Double):PTuttoInUnoDirectData;
    function NewDirectString(value:String):PTuttoInUnoDirectData;
    function NewDirectObject(value:TObject):PTuttoInUnoDirectData;
    procedure ReleaseDirectData(data:PTuttoInUnoDirectData);

implementation

function NewDirectNil:PTuttoInUnoDirectData;
begin
    result:=PTuttoInUnoDirectData(GetMem(sizeof(TTuttoInUnoDirectData)));
    result^.Initialize;
    result^.datatype:=dtNil;
end;

function NewDirectInteger(value:Integer):PTuttoInUnoDirectData;
begin
    result:=PTuttoInUnoDirectData(GetMem(sizeof(TTuttoInUnoDirectData)));
    result^.Initialize;
    result^.AsInteger:=value;
end;

function NewDirectFloat(value:Double):PTuttoInUnoDirectData;
begin
    result:=PTuttoInUnoDirectData(GetMem(sizeof(TTuttoInUnoDirectData)));
    result^.Initialize;
    result^.AsDouble:=value;
end;

function NewDirectString(value:String):PTuttoInUnoDirectData;
begin
    result:=PTuttoInUnoDirectData(GetMem(sizeof(TTuttoInUnoDirectData)));
    result^.Initialize;
    result^.AsString:=value;
end;

function NewDirectObject(value:TObject):PTuttoInUnoDirectData;
begin
    result:=PTuttoInUnoDirectData(GetMem(sizeof(TTuttoInUnoDirectData)));
    result^.Initialize;
    result^.AsObject:=value;
end;

procedure ReleaseDirectData(data:PTuttoInUnoDirectData);
begin
    data^.Finalize;
    FreeMem(data,sizeof(TTuttoInUnoDirectData));
end;

{ TTuttoInUnoDirectData }

procedure TTuttoInUnoDirectData.SetInteger(value:Integer);
begin
    if datahead<>nil then FreeMem(datahead, datasize);
    datasize:=sizeof(Integer);
    datahead:=GetMem(datasize);
    PInteger(datahead)^:=value;
    datatype:=dtInteger;
end;

procedure TTuttoInUnoDirectData.SetDouble(value:Double);
begin
    if datahead<>nil then FreeMem(datahead, datasize);
    datasize:=sizeof(Double);
    datahead:=GetMem(datasize);
    PDouble(datahead)^:=value;
    datatype:=dtFloat;
end;

procedure TTuttoInUnoDirectData.SetString(value:String);
begin
    if datahead<>nil then FreeMem(datahead, datasize);
    datasize:=length(value)+1;
    datahead:=GetMem(datasize);
    strcopy(pchar(datahead),pchar(value+#0));
    datatype:=dtString;
end;

procedure TTuttoInUnoDirectData.SetObject(value:TObject);
begin
    if datahead<>nil then FreeMem(datahead, datasize);
    datasize:=0;
    datahead:=Pointer(value);
    if value=nil then datatype:=dtNil else datatype:=dtObject;
end;

function TTuttoInUnoDirectData.GetInteger:Integer;
begin
    if datahead=nil then raise Exception.Create('TTuttoInUnoDirectData is not initialized');
    if datatype<>dtInteger then raise Exception.Create('TTuttoInUnoDirectData is not an integer');
    result:=PInteger(datahead)^;
end;

function TTuttoInUnoDirectData.GetDouble:Double;
begin
    if datahead=nil then raise Exception.Create('TTuttoInUnoDirectData is not initialized');
    if datatype<>dtFloat then raise Exception.Create('TTuttoInUnoDirectData is not a float');
    result:=PDouble(datahead)^;
end;

function TTuttoInUnoDirectData.GetString:String;
begin
    if datahead=nil then raise Exception.Create('TTuttoInUnoDirectData is not initialized');
    if datatype<>dtString then raise Exception.Create('TTuttoInUnoDirectData is not a string');
    result:=StrPas(pchar(datahead));
end;

function TTuttoInUnoDirectData.GetObject:TObject;
begin
    if datahead=nil then raise Exception.Create('TTuttoInUnoDirectData is not initialized');
    case datatype of
        dtObject: result:=TObject(datahead);
        dtNil:    result:=nil;
        else raise Exception.Create('TTuttoInUnoDirectData is not an object');
    end;
end;

procedure TTuttoInUnoDirectData.Initialize;
begin
    datatype:=dtUnknown;
    datasize:=0;
    datahead:=nil;
end;

procedure TTuttoInUnoDirectData.Finalize;
begin
    if datasize<>0 then Freemem(datahead, datasize);
    datatype:=dtUnknown;
    datasize:=0;
    datahead:=nil;
end;

procedure TTuttoInUnoDirectData.Assign(source:TTuttoInUnoDirectData);
begin
    {
    Self.Initialize;
    Self.datasize:=source.datasize;
    Self.datatype:=source.datatype;
    Self.datahead:=GetMem(source.datasize);
    move(pbyte(source.datahead), pbyte(Self.datahead), Self.datasize);
    }
    //上面这个assign问题在哪啊？？？
    case source.datatype of
        dtObject:  Self.AsObject  := source.AsObject;
        dtNil:     Self.AsObject  := source.AsObject;
        dtInteger: Self.AsInteger := source.AsInteger;
        dtFloat:   Self.AsDouble  := source.AsDouble;
        dtString:  Self.AsString  := source.AsString;
        else raise ETuttoInUnoDataError.Create('TTuttoInUnoDirectData.Assign unexpected datatype');
    end;
end;

function TTuttoInUnoDirectData.ToString:string;
var obj:TObject;
begin
    case Self.datatype of
        dtObject:
        begin
            obj:=Self.AsObject;
            if obj=nil then
                result:='nil'
            else
                result:=TTuttoInUnoData(obj).ToString;
        end;
        dtNil:     result:='nil';
        dtInteger: result:=IntToStr(Self.AsInteger);
        dtFloat:   result:=FloatToStr(Self.AsDouble);
        dtString:  result:=Format('"%s"',[Self.AsString]);
        else raise ETuttoInUnoDataError.Create('TTuttoInUnoDirectData.ToString unexpected datatype');
    end;
end;

{ TTuttoInUnoData }

procedure TTuttoInUnoData.SetData(Key:string;Value:TTuttoInUnoDirectData);
var index:integer;
    pdata:PTuttoInUnoDirectData;
begin
    if FAttributeMap.Find(Key, index) then begin
        pdata:=PTuttoInUnoDirectData(FAttributeMap.Objects[index]);
        pdata^.Finalize;
        pdata^.Assign(Value);
    end else begin
        index:=FAttributeMap.Add(Key);
        pdata:=PTuttoInUnoDirectData(GetMem(sizeof(TTuttoInUnoDirectData)));
        pdata^.Assign(Value);
        FAttributeMap.Objects[index]:=TObject(pdata);
    end;
end;

function TTuttoInUnoData.GetData(Key:string):TTuttoInUnoDirectData;
var index:integer;
begin
    result.Initialize;
    if not FAttributeMap.Find(Key, index) then exit;
    result.Assign(PTuttoInUnoDirectData(FAttributeMap.Objects[index])^);
end;

procedure TTuttoInUnoData.SetObject(Key:string;Value:TObject);
var index:integer;
    pdata:PTuttoInUnoDirectData;
begin
    if FAttributeMap.Find(Key, index) then begin
        pdata:=PTuttoInUnoDirectData(FAttributeMap.Objects[index]);
        pdata^.Finalize;
        pdata^.AsObject:=Value;
    end else begin
        index:=FAttributeMap.Add(Key);
        pdata:=NewDirectObject(Value);
        FAttributeMap.Objects[index]:=TObject(pdata);
    end;
end;

function TTuttoInUnoData.GetObject(Key:string):TObject;
var index:integer;
begin
    result:=nil;
    if not FAttributeMap.Find(Key, index) then exit;
    result:=PTuttoInUnoDirectData(FAttributeMap.Objects[index])^.AsObject;
end;

procedure TTuttoInUnoData.SetInteger(Key:string;Value:Integer);
var index:integer;
    pdata:PTuttoInUnoDirectData;
begin
    if FAttributeMap.Find(Key, index) then begin
        pdata:=PTuttoInUnoDirectData(FAttributeMap.Objects[index]);
        pdata^.Finalize;
        pdata^.AsInteger:=Value;
    end else begin
        index:=FAttributeMap.Add(Key);
        pdata:=NewDirectInteger(Value);
        FAttributeMap.Objects[index]:=TObject(pdata);
    end;
end;

function TTuttoInUnoData.GetInteger(Key:string):Integer;
var index:integer;
begin
    result:=0;
    if not FAttributeMap.Find(Key, index) then exit;
    result:=PTuttoInUnoDirectData(FAttributeMap.Objects[index])^.AsInteger;
end;

procedure TTuttoInUnoData.SetDouble(Key:string;Value:Double);
var index:integer;
    pdata:PTuttoInUnoDirectData;
begin
    if FAttributeMap.Find(Key, index) then begin
        pdata:=PTuttoInUnoDirectData(FAttributeMap.Objects[index]);
        pdata^.Finalize;
        pdata^.AsDouble:=Value;
    end else begin
        index:=FAttributeMap.Add(Key);
        pdata:=NewDirectFloat(Value);
        FAttributeMap.Objects[index]:=TObject(pdata);
    end;
end;

function TTuttoInUnoData.GetDouble(Key:string):Double;
var index:integer;
begin
    result:=0.0;
    if not FAttributeMap.Find(Key, index) then exit;
    result:=PTuttoInUnoDirectData(FAttributeMap.Objects[index])^.AsDouble;
end;

procedure TTuttoInUnoData.SetString(Key:string;Value:String);
var index:integer;
    pdata:PTuttoInUnoDirectData;
begin
    if FAttributeMap.Find(Key, index) then begin
        pdata:=PTuttoInUnoDirectData(FAttributeMap.Objects[index]);
        pdata^.Finalize;
        pdata^.AsString:=Value;
    end else begin
        index:=FAttributeMap.Add(Key);
        pdata:=NewDirectString(Value);
        FAttributeMap.Objects[index]:=TObject(pdata);
    end;
end;

function TTuttoInUnoData.GetString(Key:string):String;
var index:integer;
begin
    result:='';
    if not FAttributeMap.Find(Key, index) then exit;
    result:=PTuttoInUnoDirectData(FAttributeMap.Objects[index])^.AsString;
end;

function TTuttoInUnoData.ToString:ansistring;
var idx:integer;
begin
    result:=Format('%s{ ',[Self.ClassName]);
    if FValue.datatype<>dtUnknown then result:=result+Format('value: %s, ',[FValue.ToString]);
    result:=result+Format('tags:[%s], ',[FTags.CommaText]);
    result:=result+'attrs: {';
    for idx:= FAttributeMap.Count-1 downto 0 do begin
        result:=result+Format('%s: %s',[FAttributeMap.Strings[idx], PTuttoInUnoDirectData(FAttributeMap.Objects[idx])^.ToString]);
        if idx<>0 then result:=result+',';
    end;
    result:=result+'}}';
end;

function TTuttoInUnoData.Equals(Obj:TTuttoInUnoData):boolean;
begin
    result:=Self = Obj; //目前和TObject的比较是一样的
end;

procedure TTuttoInUnoData.AddTag(Tag:String);
begin
    FTags.Add(Tag);
end;

procedure TTuttoInUnoData.DelTag(Tag:String);
var index:integer;
begin
    if FTags.Find(Tag,index) then FTags.Delete(index);
end;

function TTuttoInUnoData.HasTag(Tag:String):Boolean;
var index:integer;
begin
    result:=FTags.Find(Tag,index);
end;

procedure TTuttoInUnoData.AssignAttributes(ASource:TTuttoInUnoData);
var idx:integer;
    key:string;
    pdata:PTuttoInUnoDirectData;
begin
    for key in ASource.FAttributeMap do begin
        pdata:=GetMem(sizeof(TTuttoInUnoDirectData));
        idx:=ASource.FAttributeMap.IndexOf(key);
        pdata^.Assign(PTuttoInUnoDirectData(ASource.FAttributeMap.Objects[idx])^);
        Self.FAttributeMap.AddObject(key,TObject(pdata));
    end;
    FTags.AddStrings(ASource.FTags, false);
end;

constructor TTuttoInUnoData.Create;
begin
    FAttributeMap:=TStringList.Create;
    FAttributeMap.Sorted:=true;
    FTags:=TStringList.Create;
    FTags.Sorted:=true;
    FValue.Initialize;
end;

destructor TTuttoInUnoData.Destroy;
var index:Integer;
    pdata:PTuttoInUnoDirectData;
begin
    for index:=FAttributeMap.Count-1 downto 0 do begin
        pdata:=PTuttoInUnoDirectData(FAttributeMap.Objects[index]);
        pdata^.Finalize;
        FreeMem(pdata,sizeof(TTuttoInUnoDirectData));
        FAttributeMap.Delete(index);
    end;
    FAttributeMap.Free;
    FTags.Free;
    FValue.Finalize;
end;

class function TTuttoInUnoData.AufTypeName:String;
begin
    result:='tiu';
end;


{ TTuttoInUnoList }

function TTuttoInUnoList.GetItem(Index:Integer):TTuttoInUnoDirectData;
begin
    if index>=FList.Count then raise ETuttoInUnoDataError.Create('TTuttoInUnoList.GetItem 超界');
    result:=PTuttoInUnoDirectData(FList.Items[Index])^;
end;

procedure TTuttoInUnoList.SetItem(Index:Integer;Item:TTuttoInUnoDirectData);
var pdata:PTuttoInUnoDirectData;
begin
    if index>=FList.Count then raise ETuttoInUnoDataError.Create('TTuttoInUnoList.SetItem 超界');
    pdata:=PTuttoInUnoDirectData(FList.Items[Index]);
    pdata^.Finalize;
    pdata^.Assign(Item);
end;

function TTuttoInUnoList.GetItemCount:Integer;
begin
    result:=FList.Count;
end;

procedure TTuttoInUnoList.Append(Item:PTuttoInUnoDirectData);
begin
    FList.Add(Item);
end;

procedure TTuttoInUnoList.Insert(Index:Integer;Item:PTuttoInUnoDirectData);
begin
    FList.Insert(Index,Item);
end;

procedure TTuttoInUnoList.Delete(Index:Integer);
var pdata:PTuttoInUnoDirectData;
begin
    pdata:=PTuttoInUnoDirectData(FList.Items[Index]);
    pdata^.Finalize;
    FreeMem(pdata,sizeof(TTuttoInUnoDirectData));
    FList.Delete(Index);
end;

constructor TTuttoInUnoList.Create;
begin
    inherited Create;
    FList:=TList.Create;
end;

destructor TTuttoInUnoList.Destroy;
var index:integer;
    pdata:PTuttoInUnoDirectData;
begin
    for index:=FList.Count-1 downto 0 do begin
        pdata:=PTuttoInUnoDirectData(FList.Items[index]);
        pdata^.Finalize;
        FreeMem(pdata,sizeof(TTuttoInUnoDirectData));
        FList.Delete(index);
    end;
    FList.Free;
    inherited Destroy;
end;

class function TTuttoInUnoList.AufTypeName:String;
begin
    result:='tiu.list';
end;

class function TData.AufTypeName:String;
begin
    result:='tiu.data';
end;

end.

