program test(input,output);
{$t+}
var i,ans,j,k:integer;
function mymax(x:integer; y:integer):integer;
begin
	mymax:=x;
	if (y>x) then 
		mymax:=y;
end;
begin
	ans:=0;
	for i := 1 to 10 do
		ans:= ans+i;
	ans := ans + mymax(99,88);
	writeln(ans);
end.
