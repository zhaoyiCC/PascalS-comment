program test(input,output);
{$t+}
	var i,ans,j,k:integer;
	procedure init();
		var d: double;
		begin
			d:=3.14;
		end;
	function mymax(x:integer; y:integer):integer;
		var mm:integer;
		function job():integer;
			var h:integer;
			var p,q:char;
			begin
				h:=10;
				job:=h;
			end;
		begin
			mymax:=x;
			if (y>x) then 
				mymax:=y;
			mymax:=mymax + job();
		end;
	begin
		ans:=0;
		for i := 1 to 10 do
			ans:= ans+i;
		ans := ans + mymax(99,88);
		writeln(ans);
	end.
