program StringCast;

var
    ItemJson, MemberName, Detail: string;
    ItemKind: integer;

begin
    ItemJson := '{' +
        '"label":' + '"' + string(MemberName) + '",' +
        '"kind":' + string(Detail);
    if Detail <> '' then
        ItemJson := ItemJson + ',"detail":' + '"' + string(Detail) + '"';
    ItemJson := ItemJson + '}';
end.
