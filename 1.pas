program zy (input, output);
{var x:integer;}
const N = 1000;
const M = N;
var x:integer;
var ans: integer;
var a: array[1..100] of integer;

function wwork(yyy:integer): integer;
var i:integer;
begin
    wwork:=yyy;
    for i:= 1 to 10 do
        write(yyy);
    writeln;
end;

begin
    ans:= wwork(1);
    {readln(x);}
    writeln('123');
    x:=666;
    writeln(x);
    a[1] := x;
    readln;
end.   
