unit basic;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
    Classes, SysUtils, auf_type_base;

type

    TTIUDirectDataType = (dtNil, dtInteger, dtFloat, dtString, dtObject, dtIntArray);
    PTuttoInUnoDirectData = ^TTuttoInUnoDirectData;
    TTuttoInUnoDirectData = record
        datatype:TTIUDirectDataType;
        datasize:Integer;
        datahead:PByte;
        unitsize:Integer; //只有在dt*Array时用到，表示一个元素的内存长度，datasize必须是其整数倍
    private
        procedure SetInteger(value:Integer);
        procedure SetDouble(value:Double);
        procedure SetString(value:String);
        procedure SetObject(value:TObject);
        procedure SetIntArray(index,value:Integer);
        function GetInteger:Integer;
        function GetDouble:Double;
        function GetString:String;
        function GetObject:TObject;
        function GetIntArray(index:Integer):Integer;
    public
        procedure Initialize;
        procedure Finalize;
        procedure Assign(source:TTuttoInUnoDirectData);
    public
        function ToString:string;
        procedure CreateIntArray(len:Integer);
        property AsInteger:Integer read GetInteger write SetInteger;
        property AsDouble:Double read GetDouble write SetDouble;
        property AsString:String read GetString write SetString;
        property AsObject:TObject read GetObject write SetObject;
        property AsIntArray[index:Integer]:Integer read GetIntArray write SetIntArray;
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

    TRegionCoords = class(TData)
    protected
        function GetDimension:Integer;
        function GetCoord(index:integer):TRegionIndex;
        procedure SetCoord(index:integer;avalue:TRegionIndex);
    public
        procedure SetCoords(pindex:PRegionIndex;dimens:Integer);
        function GetCoords:PRegionIndex;
        function DistanceTo(Coords:TRegionCoords):Double;
        function Length:Double;
        procedure Add(Coords:TRegionCoords);
        procedure Sub(Coords:TRegionCoords);
        constructor Create;
        destructor Destroy; override;
        class function AufTypeName: String; override;
        property Dimension:Integer read GetDimension;
        property Coord[index:Integer]:TRegionIndex read GetCoord write SetCoord; default;
    end;


    function NewDirectNil:PTuttoInUnoDirectData;
    function NewDirectInteger(value:Integer):PTuttoInUnoDirectData;
    function NewDirectFloat(value:Double):PTuttoInUnoDirectData;
    function NewDirectString(value:String):PTuttoInUnoDirectData;
    function NewDirectObject(value:TObject):PTuttoInUnoDirectData;
    function NewDirectIntArray(arr:array of integer):PTuttoInUnoDirectData;
    procedure ReleaseDirectData(data:PTuttoInUnoDirectData);

    //创建TDATA的特殊值
    function ZeroRegionCoords:TRegionCoords;
    function DataNil:TData;


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

function NewDirectIntArray(arr:array of integer):PTuttoInUnoDirectData;
var len,idx:Integer;
begin
    len:=Length(arr);
    result:=PTuttoInUnoDirectData(GetMem(sizeof(TTuttoInUnoDirectData)));
    result^.Initialize;
    result^.unitsize:=sizeof(Integer);
    for idx:=len-1 downto 0 do (PInteger(result^.datahead)+idx)^:=arr[idx];
end;

procedure ReleaseDirectData(data:PTuttoInUnoDirectData);
begin
    data^.Finalize;
    FreeMem(data,sizeof(TTuttoInUnoDirectData));
end;

function ZeroRegionCoords:TRegionCoords;
begin
    result:=TRegionCoords.Create;
    result.SetCoords(nil,0);
end;

function DataNil:TData;
begin
  result:=TData.Create;
  result.FValue.datatype:=dtNil;
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

procedure TTuttoInUnoDirectData.SetIntArray(index,value:Integer);
begin
    if datahead=nil then raise Exception.Create('TTuttoInUnoDirectData is not initialized');
    if datatype<>dtIntArray then raise Exception.Create('TTuttoInUnoDirectData is not an int array');
    if (index+1)*sizeof(Integer)>datasize then raise Exception.Create('TTuttoInUnoDirectData array index overflow');
    (PInteger(datahead)+index)^:=value;
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

function TTuttoInUnoDirectData.GetIntArray(index:Integer):Integer;
begin
    if datahead=nil then raise Exception.Create('TTuttoInUnoDirectData is not initialized');
    if datatype<>dtIntArray then raise Exception.Create('TTuttoInUnoDirectData is not an int array');
    if (index+1)*sizeof(Integer)>datasize then raise Exception.Create('TTuttoInUnoDirectData array index overflow');
    result:=(PInteger(datahead)+index)^;
end;

procedure TTuttoInUnoDirectData.Initialize;
begin
    datatype:=dtNil;
    datasize:=0;
    datahead:=nil;
    unitsize:=1;
end;

procedure TTuttoInUnoDirectData.Finalize;
begin
    if datasize<>0 then Freemem(datahead, datasize);
    datatype:=dtNil;
    datasize:=0;
    datahead:=nil;
    unitsize:=1;
end;

procedure TTuttoInUnoDirectData.Assign(source:TTuttoInUnoDirectData);
begin
    Self.Initialize;
    Self.datasize:=source.datasize;
    Self.datatype:=source.datatype;
    Self.datahead:=GetMem(source.datasize);
    Self.unitsize:=source.unitsize;
    move(pbyte(source.datahead)^, pbyte(Self.datahead)^, Self.datasize);
end;

function TTuttoInUnoDirectData.ToString:string;
var obj:TObject;
    idx,max_idx:integer;
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
        dtIntArray:
        begin
            result:='[';
            max_idx:=datasize div unitsize - 1;
            for idx:=0 to max_idx do begin
               result:=result+IntToStr((PInteger(datahead)+idx)^);
               if idx<>max_idx then result:=result+',';
            end;
            result:=result+']';
        end
        else raise ETuttoInUnoDataError.Create('TTuttoInUnoDirectData.ToString unexpected datatype');
    end;
end;

procedure TTuttoInUnoDirectData.CreateIntArray(len:Integer);
begin
    if datahead<>nil then FreeMem(datahead, datasize);
    unitsize:=sizeof(Integer);
    datasize:=len*unitsize;
    datahead:=GetMem(datasize);
    datatype:=dtIntArray;
    FillByte(datahead^,datasize,0);
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
    result:=result+Format('value: %s, ',[FValue.ToString]);
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
    inherited Create;
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
    inherited Destroy;
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


{ TRegionCoords }

function TRegionCoords.GetDimension:Integer;
begin
    result:=FValue.datasize div FValue.unitsize;
end;

function TRegionCoords.GetCoord(index:integer):TRegionIndex;
begin
    if index >= (FValue.datasize div FValue.unitsize) then raise ETuttoInUnoDataError.Create('TRegionCoords.GetCoord 维度超界');
    result:=(PInteger(FValue.datahead)+index)^;
end;

procedure TRegionCoords.SetCoord(index:integer;avalue:TRegionIndex);
begin
    if index >= (FValue.datasize div FValue.unitsize) then raise ETuttoInUnoDataError.Create('TRegionCoords.SetCoord 维度超界');
    (PInteger(FValue.datahead)+index)^:=avalue;
end;

procedure TRegionCoords.SetCoords(pindex:PRegionIndex;dimens:Integer);
begin
    FValue.CreateIntArray(dimens);
    //这里有一个潜在的风险，如果RegionIndex被改成Integer以外的类型，有可能出现内存错误
    move(pindex^,FValue.datahead^,dimens*sizeof(Integer));
end;

function TRegionCoords.GetCoords:PRegionIndex;
begin
  result:=PInteger(FValue.datahead);
end;

function TRegionCoords.DistanceTo(Coords:TRegionCoords):Double;
var idx,v1,v2:integer;
begin
  result:=0;
  if Coords.Dimension<>Self.Dimension then raise ETuttoInUnoDataError.Create('TRegionCoords.DistanceTo 维数不同不能计算距离');
  for idx:= Dimension-1 downto 0 do begin
      v1:=Self[idx];
      v2:=Coords[idx];
      result:=result+v1*v1+v2*v2;
  end;
  result:=sqrt(result);
end;

function TRegionCoords.Length:Double;
var idx,v1:integer;
begin
  result:=0;
  for idx:= Dimension-1 downto 0 do begin
      v1:=Self[idx];
      result:=result+v1*v1;
  end;
  result:=sqrt(result);
end;

procedure TRegionCoords.Add(Coords:TRegionCoords);
var idx:integer;
begin
  if Coords.Dimension<>Self.Dimension then raise ETuttoInUnoDataError.Create('TRegionCoords.Add 维数不同不能计算距离');
  for idx:= Dimension-1 downto 0 do begin
      Self[idx]:=Self[idx]+Coords[idx];
  end;
end;

procedure TRegionCoords.Sub(Coords:TRegionCoords);
var idx:integer;
begin
  if Coords.Dimension<>Self.Dimension then raise ETuttoInUnoDataError.Create('TRegionCoords.Add 维数不同不能计算距离');
  for idx:= Dimension-1 downto 0 do begin
      Self[idx]:=Self[idx]-Coords[idx];
  end;
end;

constructor TRegionCoords.Create;
begin
    inherited Create;
    FValue.Initialize;
end;

destructor TRegionCoords.Destroy;
begin
    FValue.Finalize;
    inherited Destroy;
end;

class function TRegionCoords.AufTypeName: String;
begin
  result:='tiu.coords';
end;


end.

