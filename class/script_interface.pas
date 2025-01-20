unit script_interface;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Apiglio_Useful, auf_ram_var;

procedure GenerateAufScriptFunction(AufScpt:TAufScript);

implementation
uses basic, item, player, region, gamerule, strutils;

function GetArgData(AAuf:TAuf;ArgNumber:byte):PTuttoInUnoDirectData;
var tmpFunc:pFuncAufStr;
    stmp:string;
    ltmp:longint;
    dtmp:double;
    obj:TObject;
begin
    result:=nil;
    tmpFunc:=AAuf.Script.IO_fptr.error;
    AAuf.Script.IO_fptr.error:=nil;
    if AAuf.nargs[ArgNumber].pre='"' then begin
        if AAuf.TryArgToString(ArgNumber,stmp) then result:=NewDirectString(stmp);
    end else begin
        if AAuf.TryArgToLong(ArgNumber,ltmp) then begin
            result:=NewDirectInteger(ltmp);
        end else if AAuf.TryArgToDouble(ArgNumber,dtmp) then begin
            result:=NewDirectFloat(dtmp);
        end else begin
            if AAuf.TryArgToObject(ArgNumber,TData,obj) then begin
                result:=GetMem(sizeof(TTuttoInUnoDirectData));
                result^.Assign(TData(obj).FValue);
          end;
        end;
    end;
    AAuf.Script.IO_fptr.error:=tmpFunc;
end;

procedure func_nil(Sender:TObject);
begin
    (Sender as TAufScript).writeln('该方法仅用于高亮方案占位，请检查是否存在类型错误');
end;

procedure setAttribute(Sender:TObject); // data . setAttribute key value
var AufScpt:TAufScript;
    AAuf:TAuf;
    key:string;
    obj:TObject;
    tmpDD:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(4) then exit;
    if not AAuf.TryArgToObject(1,TTuttoInUnoData,obj) then exit;
    if not AAuf.TryArgToString(2,key) then exit;
    tmpDD:=GetArgData(AAuf,3);
    if tmpDD<>nil then begin
        TTuttoInUnoData(obj).Data[key]:=tmpDD^;
        ReleaseDirectData(tmpDD);
    end;
end;

procedure addTag(Sender:TObject); // data . addTag tag
var AufScpt:TAufScript;
    AAuf:TAuf;
    tag:string;
    obj:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TTuttoInUnoData,obj) then exit;
    if not AAuf.TryArgToString(2,tag) then exit;
    TTuttoInUnoData(obj).AddTag(tag);
end;

procedure delTag(Sender:TObject); // data . deltag tag
var AufScpt:TAufScript;
    AAuf:TAuf;
    tag:string;
    obj:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TTuttoInUnoData,obj) then exit;
    if not AAuf.TryArgToString(2,tag) then exit;
    TTuttoInUnoData(obj).DelTag(tag);
end;

procedure getAttribute(Sender:TObject); // value = getAttribute data key
var AufScpt:TAufScript;
    AAuf:TAuf;
    key:string;
    value,data:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(4) then exit;
    if not AAuf.TryArgToObject(1,TTuttoInUnoData,value) then exit;
    if not AAuf.TryArgToObject(2,TTuttoInUnoData,data) then exit;
    if not AAuf.TryArgToString(3,key) then exit;
    TData(value).FValue.Assign(TTuttoInUnoData(data).Data[Key]);
end;

procedure inspectAttribute(Sender:TObject); // inspectAttribute data
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(2) then exit;
    if not AAuf.TryArgToObject(1,TTuttoInUnoData,data) then exit;
    AufScpt.writeln(TTuttoInUnoData(data).ToString);
end;

procedure newPlayerPrototype(Sender:TObject); // player_proto = newPlayerPrototype
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    obj:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(2) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    obj:=TPlayer.Create;
    obj_to_arv(obj,arv);
    AufScpt.writeln('成功创建玩家原型');
end;

procedure newPlayer(Sender:TObject); // player = newPlayer proto name
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    player,prototype:TObject;
    player_name:string;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(4) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    if not AAuf.TryArgToObject(2,TPlayer,prototype) then exit;
    if not AAuf.TryArgToString(3,player_name) then exit;
    player:=TPlayer.Create;
    TPlayer(player).AssignAttributes(prototype as TPlayer);
    TPlayer(player).Value.AsString:=player_name;
    obj_to_arv(player,arv);
    AufScpt.writeln(Format('成功创建玩家：%s',[TPlayer(player).Value.AsString]));
end;

procedure Player_win(Sender:TObject); // player . win
var AufScpt:TAufScript;
    AAuf:TAuf;
    player:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(2) then exit;
    if not AAuf.TryArgToObject(1,TPlayer,player) then exit;
    TPlayer(player).Win;
    AufScpt.writeln(Format('玩家“%s”获得胜利',[TPlayer(player).FValue.AsString]));
end;

procedure PlayerLoop_new(Sender:TObject); // player_loop = newPlayerLoop [player, ...]
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    player_loop,player:TObject;
    player_argnumber,count_argument:integer;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    count_argument:=AAuf.ArgsCount;
    if not AAuf.CheckArgs(2) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    player_argnumber:=2;
    player_loop:=TPlayerLoop.Create;

    while player_argnumber<count_argument do begin
        if not AAuf.TryArgToObject(player_argnumber, TPlayer, player) then exit;
        TPlayerLoop(player_loop).Append(NewDirectObject(player)); //List.Append内会深复制一个，所以外面用临时变量
        inc(player_argnumber);
    end;

    obj_to_arv(player_loop,arv);
    AufScpt.writeln('成功创建玩家循环');
end;

procedure PlayerLoop_randomSelect(Sender:TObject); // player . rselect
var AufScpt:TAufScript;
    AAuf:TAuf;
    player_loop:TObject;
    player:TPlayer;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(2) then exit;
    if not AAuf.TryArgToObject(1,TPlayerLoop,player_loop) then exit;
    TPlayerLoop(player_loop).SetRandomCursor;
    player:=TPlayerLoop(player_loop).CurrentPlayer;
    AufScpt.writeln(Format('随机选中：%s',[player.FValue.AsString]));
end;

procedure PlayerLoop_nextPlayer(Sender:TObject); // player_loop . nextPlayer
var AufScpt:TAufScript;
    AAuf:TAuf;
    player_loop:TObject;
    player:TPlayer;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(2) then exit;
    if not AAuf.TryArgToObject(1,TPlayerLoop,player_loop) then exit;
    TPlayerLoop(player_loop).NextPlayer;
    player:=TPlayerLoop(player_loop).CurrentPlayer;
    AufScpt.writeln(Format('下一个玩家：%s',[player.FValue.AsString]));
end;

procedure PlayerLoop_currentPlayer(Sender:TObject); // player = currentplayer playerloop
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    player_loop,player:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    if not AAuf.TryArgToObject(2,TPlayerLoop,player_loop) then exit;
    player:=TPlayerLoop(player_loop).CurrentPlayer;
    obj_to_arv(player,arv);
end;

procedure newItemPrototype(Sender:TObject); // item_proto = newItemPrototype
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    obj:TObject;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(2) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    obj:=TItem.Create;
    obj_to_arv(obj,arv);
    AufScpt.writeln('成功创建物品原型');
end;

procedure newItem(Sender:TObject); // item = newItem proto name
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    item,prototype:TObject;
    item_name:string;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(4) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    if not AAuf.TryArgToObject(2,TItem,prototype) then exit;
    if not AAuf.TryArgToString(3,item_name) then exit;
    item:=TItem.Create;
    TItem(item).AssignAttributes(prototype as TItem);
    TItem(item).Value.AsString:=item_name;
    obj_to_arv(item,arv);
    AufScpt.writeln(Format('成功创建物品：%s',[TItem(item).Value.AsString]));
end;

procedure Item_moveTo(Sender:TObject); // item . moveTo region
var AufScpt:TAufScript;
    AAuf:TAuf;
    item,region:TObject;
    player:TPlayer;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TItem,item) then exit;
    if not AAuf.TryArgToObject(2,TRegion,region) then exit;
    TItem(item).MoveTo(region as TTuttoInUnoData,[]);
    AufScpt.writeln(Format('将物品%s移动到%s',[TItem(item).Value.AsString, TRegion(region).Value.AsString]));
end;

procedure newRegion(Sender:TObject); // region = newRegion name dimension
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    region:TObject;
    region_name:string;
    dimension:byte;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    if not AAuf.TryArgToString(2,region_name) then exit;
    if AAuf.ArgsCount>3 then begin
        if not AAuf.TryArgToByte(3,dimension) then exit;
    end else dimension:=0;
    region:=TRegion.Create(dimension);
    TRegion(region).Value.AsString:=region_name;
    obj_to_arv(region,arv);
    AufScpt.writeln(Format('成功创建区域：%s',[TRegion(region).Value.AsString]));
end;

procedure Region_itemCount(Sender:TObject); // data = itemCount @region
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    region:TObject;
    data:TData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    if not AAuf.TryArgToObject(2,TRegion,region) then exit;
    data:=TData.Create;
    data.Value.AsInteger:=TRegion(region).ItemCount;
    obj_to_arv(data,arv);
    AufScpt.writeln(Format('区域“%s”内的物品数量为：%d',[TRegion(region).Value.AsString, data.Value.AsInteger]));
end;

procedure askForNumber(Sender:TObject); // number = askForNumber player min max
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    player,data:TObject;
    nmin,nmax:longint;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(5) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    if not AAuf.TryArgToObject(2,TPlayer,player) then exit;
    if not AAuf.TryArgToLong(3,nmin) then exit;
    if not AAuf.TryArgToLong(4,nmax) then exit;
    data:=TData.Create;
    TData(data).Value.AsInteger:=TPlayer(player).AskForNumber(nmin,nmax);
    obj_to_arv(data,arv);
    AufScpt.writeln('玩家“'+TPlayer(player).Value.AsString+'”选择数字'+IntToStr(TData(data).Value.AsInteger));
end;

procedure Data_new(Sender:TObject); // data = newData value
var AufScpt:TAufScript;
    AAuf:TAuf;
    arv:TAufRamVar;
    data:TData;
    value:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToARV(1,8,8,[ARV_FixNum],arv) then exit;
    value:=GetArgData(AAuf,2);
    data:=TData.Create;
    TData(data).Value.Assign(value^);
    obj_to_arv(data,arv);
    ReleaseDirectData(value);
end;

procedure Data_mov(Sender:TObject); // data . mov value
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
    value:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TData,data) then exit;
    value:=GetArgData(AAuf,2);
    TData(data).Value.Assign(value^);
    ReleaseDirectData(value);
end;

procedure Data_add(Sender:TObject); // data . add operand
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
    value:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TData,data) then exit;
    value:=GetArgData(AAuf,2);
    try
      try
        case TData(data).FValue.datatype of
            dtInteger:
            case value^.datatype of
                dtInteger:TData(data).Value.AsInteger:=TData(data).Value.AsInteger+value^.AsInteger;
                dtFloat:TData(data).Value.AsInteger:=TData(data).Value.AsInteger+trunc(value^.AsDouble);
                else raise Exception.Create('');
            end;
            dtFloat:
            case value^.datatype of
                dtInteger:TData(data).Value.AsDouble:=TData(data).Value.AsDouble+value^.AsInteger;
                dtFloat:TData(data).Value.AsDouble:=TData(data).Value.AsDouble+value^.AsDouble;
                else raise Exception.Create('');
            end;
            dtString:
            case value^.datatype of
                dtString:TData(data).Value.AsString:=TData(data).Value.AsString+value^.AsString;
                else raise Exception.Create('');
            end;
            else raise Exception.Create('');
        end;
      except
        AufScpt.send_error(Format('%s与%s不能直接计算，代码未执行',[TData(data).FValue.ToString,value^.ToString]));
      end;
    finally
        ReleaseDirectData(value);
    end;
end;

procedure Data_sub(Sender:TObject); // data . sub operand
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
    value:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TData,data) then exit;
    value:=GetArgData(AAuf,2);
    try
      try
        case TData(data).FValue.datatype of
            dtInteger:
            case value^.datatype of
                dtInteger:TData(data).Value.AsInteger:=TData(data).Value.AsInteger-value^.AsInteger;
                dtFloat:TData(data).Value.AsInteger:=TData(data).Value.AsInteger-trunc(value^.AsDouble);
                else raise Exception.Create('');
            end;
            dtFloat:
            case value^.datatype of
                dtInteger:TData(data).Value.AsDouble:=TData(data).Value.AsDouble-value^.AsInteger;
                dtFloat:TData(data).Value.AsDouble:=TData(data).Value.AsDouble-value^.AsDouble;
                else raise Exception.Create('');
            end;
            else raise Exception.Create('');
        end;
      except
        AufScpt.send_error(Format('%s与%s不能直接计算，代码未执行',[TData(data).FValue.ToString,value^.ToString]));
      end;
    finally
        ReleaseDirectData(value);
    end;
end;

procedure Data_mul(Sender:TObject); // data . mul operand
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
    value:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TData,data) then exit;
    value:=GetArgData(AAuf,2);
    try
      try
        case TData(data).FValue.datatype of
            dtInteger:
            case value^.datatype of
                dtInteger:TData(data).Value.AsInteger:=TData(data).Value.AsInteger*value^.AsInteger;
                dtFloat:TData(data).Value.AsInteger:=trunc(TData(data).Value.AsInteger*value^.AsDouble);
                else raise Exception.Create('');
            end;
            dtFloat:
            case value^.datatype of
                dtInteger:TData(data).Value.AsDouble:=TData(data).Value.AsDouble*value^.AsInteger;
                dtFloat:TData(data).Value.AsDouble:=TData(data).Value.AsDouble*value^.AsDouble;
                else raise Exception.Create('');
            end;
            else raise Exception.Create('');
        end;
      except
        AufScpt.send_error(Format('%s与%s不能直接计算，代码未执行',[TData(data).FValue.ToString,value^.ToString]));
      end;
    finally
        ReleaseDirectData(value);
    end;
end;

procedure Data_div(Sender:TObject); // data . div operand
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
    value:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TData,data) then exit;
    value:=GetArgData(AAuf,2);
    try
      try
        case TData(data).FValue.datatype of
          dtInteger:
          case value^.datatype of
              dtInteger:TData(data).Value.AsInteger:=TData(data).Value.AsInteger div value^.AsInteger;
              dtFloat:TData(data).Value.AsInteger:=TData(data).Value.AsInteger div trunc(value^.AsDouble);
              else raise Exception.Create('');
          end;
          dtFloat:
          case value^.datatype of
              dtInteger:TData(data).Value.AsDouble:=TData(data).Value.AsDouble / value^.AsInteger;
              dtFloat:TData(data).Value.AsDouble:=TData(data).Value.AsDouble / value^.AsDouble;
              else raise Exception.Create('');
          end;
          else raise Exception.Create('');
        end;
      except
        AufScpt.send_error(Format('%s与%s不能直接计算，代码未执行',[TData(data).FValue.ToString,value^.ToString]));
      end;
    finally
        ReleaseDirectData(value);
    end;
end;

procedure Data_mod(Sender:TObject); // data . mod operand
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
    value:PTuttoInUnoDirectData;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(3) then exit;
    if not AAuf.TryArgToObject(1,TData,data) then exit;
    value:=GetArgData(AAuf,2);
    try
      try
        case TData(data).FValue.datatype of
            dtInteger:
            case value^.datatype of
                dtInteger:TData(data).Value.AsInteger:=TData(data).Value.AsInteger mod value^.AsInteger;
                dtFloat:TData(data).Value.AsInteger:=TData(data).Value.AsInteger mod trunc(value^.AsDouble);
                else raise Exception.Create('');
            end;
            else raise Exception.Create('');
        end;
      except
        AufScpt.send_error(Format('%s与%s不能直接计算，代码未执行',[TData(data).FValue.ToString,value^.ToString]));
      end;
    finally
        ReleaseDirectData(value);
    end;
end;

function compare(a,b:TTuttoInUnoDirectData):string;
var ia,ib:longint;
    fa,fb:double;
    sa,sb:string;
begin
    result:='_';
    case a.datatype of
        dtInteger:
        begin
            ia:=a.AsInteger;
            case b.datatype of
                dtInteger:ib:=b.AsInteger;
                dtFloat:ib:=trunc(b.AsDouble);
                else exit;
            end;
            if ia>ib then result:='>' else if ia<ib then result:='<' else result:='=';
        end;
        dtFloat:
        begin
            fa:=a.AsDouble;
            case b.datatype of
                dtInteger:fb:=b.AsInteger;
                dtFloat:fb:=b.AsDouble;
                else exit;
            end;
            if fa>fb then result:='>' else if fa<fb then result:='<' else result:='=';
        end;
        dtString:
        begin
            sa:=a.AsString;
            case b.datatype of
                dtString:sb:=b.AsString;
                else exit;
            end;
            if sa=sb then result:='=' else result:='!';
        end
        else exit;
    end;
end;

procedure Data_cj(Sender:TObject); // data . cjx value :addr
var AufScpt:TAufScript;
    AAuf:TAuf;
    data:TObject;
    value:PTuttoInUnoDirectData;
    method_name,compare_result:string;
    is_not,is_call:boolean;
    addr:pRam;
    poss:integer;
    procedure switch_addr(addr:dword;iscall:boolean);
    begin
        if iscall then AAuf.Script.push_addr(AufScpt.ScriptLines,AufScpt.ScriptName,addr)
        else AufScpt.jump_addr(addr);
    end;
begin
    AufScpt:=Sender as TAufScript;
    AAuf:=AufScpt.Auf as TAuf;
    if not AAuf.CheckArgs(4) then exit;
    if not AAuf.TryArgToObject(1,TData,data) then exit;
    if not AAuf.TryArgToAddr(3,addr) then exit;
    value:=GetArgData(AAuf,2);

    method_name:=AAuf.args[0];
    compare_jump_mode(method_name,is_not,is_call);

    compare_result:=compare(TData(data).Value,value^);
    case method_name of
        'cje':
        case compare_result of
            '=':if not is_not then switch_addr(addr,is_call);
            '>','<','!':if is_not then switch_addr(addr,is_call);
            else AufScpt.send_error('变量不可比较');
        end;
        'cjm':
        case compare_result of
            '>':if not is_not then switch_addr(addr,is_call);
            '=','<','!':if is_not then switch_addr(addr,is_call);
            else AufScpt.send_error('变量不可比较');
        end;
        'cjl':
        case compare_result of
            '<':if not is_not then switch_addr(addr,is_call);
            '>','=','!':if is_not then switch_addr(addr,is_call);
            else AufScpt.send_error('变量不可比较');
        end;
    end;
    ReleaseDirectData(value);

end;

procedure GenerateAufScriptFunction(AufScpt:TAufScript);
begin
    with AufScpt do begin
        add_func('tiu.setattribute',@setAttribute,'key, value','设置属性值');
        add_func('tiu.addtag',@addTag,'tag','添加标签');
        add_func('tiu.deltag',@delTag,'tag','移除标签');
        add_func('getattribute',@getAttribute,'key','读取属性值',TData);
        add_func('inspectattribute',@inspectAttribute,'data','显示属性值');

        add_func('newplayerproto',@newPlayerPrototype,'','创建玩家原型',TPlayer);
        add_func('newplayer',@newPlayer,'玩家原型,玩家名称','创建新玩家',TPlayer);
        add_func('tiu.player.win',@Player_win,'','宣布玩家胜利');

        add_func('newplayerloop',@PlayerLoop_new,'[player, ...]','创建玩家循环',TPlayerLoop);
        add_func('tiu.playerloop.rselect',@PlayerLoop_randomSelect,'','随机选择一个玩家开始');
        add_func('tiu.playerloop.nextplayer',@PlayerLoop_nextPlayer,'','下一个玩家');
        add_func('currentplayer',@PlayerLoop_currentPlayer,'','返回当前玩家',TPlayer);


        add_func('newitemproto',@newItemPrototype,'','创建物品原型',TItem);
        add_func('newitem',@newItem,'物品原型,物品名称','创建新物品',TItem);
        add_func('tiu.item.moveto',@Item_moveTo,'','移动物品到区域');

        add_func('newregion',@newRegion,'维度','创建新区域',TRegion);
        add_func('itemcount',@Region_itemCount,'','返回区域内物品数量',TData);

        add_func('askfornumber',@askForNumber,'玩家,最小值,最大值','询问玩家数量',TData);

        //Data运算
        add_func('newdata',@Data_new,'value','创建新Data数值',TData);
        add_func('tiu.data.mov',@Data_mov,'A,B','A := B');

        add_func('tiu.data.add',@Data_add,'A,B','A += B');
        add_func('tiu.data.sub',@Data_sub,'A,B','A -= B');
        add_func('tiu.data.mul',@Data_mul,'A,B','A *= B');
        add_func('tiu.data.div',@Data_div,'A,B','A //= B');
        add_func('tiu.data.mod',@Data_mod,'A,B','A %= B');

        add_func('tiu.data.cje',@Data_cj,'A,B,:addr','A==B则跳转');
        add_func('tiu.data.ncje',@Data_cj,'A,B,:addr','A<>B则跳转');
        add_func('tiu.data.cjl',@Data_cj,'A,B,:addr','A==B则跳转');
        add_func('tiu.data.ncjl',@Data_cj,'A,B,:addr','A<>B则跳转');
        add_func('tiu.data.cjm',@Data_cj,'A,B,:addr','A==B则跳转');
        add_func('tiu.data.ncjm',@Data_cj,'A,B,:addr','A<>B则跳转');

        add_func('tiu.data.cjec',@Data_cj,'A,B,:addr','A==B则跳转');
        add_func('tiu.data.ncjec',@Data_cj,'A,B,:addr','A<>B则跳转');
        add_func('tiu.data.cjlc',@Data_cj,'A,B,:addr','A==B则跳转');
        add_func('tiu.data.ncjlc',@Data_cj,'A,B,:addr','A<>B则跳转');
        add_func('tiu.data.cjmc',@Data_cj,'A,B,:addr','A==B则跳转');
        add_func('tiu.data.ncjmc',@Data_cj,'A,B,:addr','A<>B则跳转');

        //高亮显示占用
        add_func('setattribute',@func_nil,'','仅用于高亮方案占位');
        add_func('rselect',@func_nil,'','仅用于高亮方案占位');
        add_func('addtag',@func_nil,'','仅用于高亮方案占位');
        add_func('deltag',@func_nil,'','仅用于高亮方案占位');
        add_func('moveto',@func_nil,'','仅用于高亮方案占位');


    end;
end;

end.

