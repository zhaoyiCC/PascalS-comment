program PASCALS(INPUT,OUTPUT,PRD,PRR);
{*  author:N.Wirth, E.T.H. CH-8092 Zurich,1.3.76 *}
{  modified by R.E.Berry
    Department of computer studies
    University of Lancaster

    Variants of this program are used on
    Data General Nova,Apple,and
    Western Digital Microengine machines. }
{   further modified by M.Z.Jin
    Department of Computer Science&Engineering BUAA,0ct.1989
}
const nkw = 27;    {* no. of key words *}
      alng = 10;   {* no. of significant chars in identifiers *}
      llng = 121;  {* input line length *}
      emax = 322;  {* max exponent of real numbers *}
      emin = -292; {* min exponent *}
      kmax = 15;   {* max no. of significant digits *}
      tmax = 100;  {* size of table *}
      bmax = 20;   {* size of block-talbe *}
      amax = 30;   {* size of array-table *}
      c2max = 20;  {* size of real constant table *}
      csmax = 30;  {* max no. of cases *}
      cmax = 800;  {* size of code *}
      lmax = 7;    {* maximum level *}
      smax = 600;  {* size of string-table *}
      ermax = 58;  {* max error no. *}
      omax = 63;   {* highest order code *}
      xmax = 32767;  {* 2**15-1 *}
      nmax = 32767;  {* 2**15-1 *}
      lineleng = 132; {* output line length *}
      linelimit = 200;
      stacksize = 1450;
type symbol = ( intcon, realcon, charcon, stringcon,
                notsy, plus, minus, times, idiv, rdiv, imod, andsy, orsy,
                eql, neq, gtr, geq, lss, leq,
                lparent, rparent, lbrack, rbrack, comma, semicolon, period,
                colon, becomes, constsy, typesy, varsy, funcsy,
                procsy, arraysy, recordsy, programsy, ident,
                beginsy, ifsy, casesy, repeatsy, whilesy, forsy,
                endsy, elsesy, untilsy, ofsy, dosy, tosy, downtosy, thensy);

     index = -xmax..+xmax;
     alfa = packed array[1..alng]of char;
     objecttyp = (konstant, vvariable, typel, prozedure, funktion );
     types = (notyp, ints, reals, bools, chars, arrays, records );
     symset = set of symbol; {*代表符号表的集合，在block中常用*}
     typset = set of types;
     item = record
               typ: types;
               ref: index;
            end;

     order = packed record
                f: -omax..+omax;
x: -lmax..+lmax;
                y: -nmax..+nmax
end;
var   ch:         char; {* last character read from source program *}
      rnum:       real; {* real number from insymbol *}
inum:       integer;     {* integer from insymbol *}
sleng:      integer;     {* string length *}
      cc:         integer;     {* character counter *}
      lc:         integer;     {* program location counter *}
      ll:         integer;     {* length of current line *}
      errpos:     integer;
      t,a,b,sx,c1,c2:integer;  {* indices to tables *}
      iflag, oflag, skipflag, stackdump, prtables: boolean;
      sy:         symbol;      {* last symbol read by insymbol *}
      errs:       set of 0..ermax;
      id:         alfa;        {* identifier from insymbol *}
      progname:   alfa;
      stantyps:   typset;
      constbegsys, typebegsys, blockbegsys, facbegsys, statbegsys: symset;
      line:       array[1..llng] of char;
      key:        array[1..nkw] of alfa;
      ksy:        array[1..nkw] of symbol;
      sps:        array[char]of symbol;  {* special symbols *}
      display:    array[0..lmax] of integer;
      tab:        array[0..tmax] of      {* indentifier lable *}
                 packed record
                     name: alfa;
                     link: index;
                     obj:  objecttyp;
                     typ:  types;
                     ref:  index;
normal: boolean;
                     lev:  0..lmax;
                     adr: integer
end;
     atab:       array[1..amax] of    {* array-table *}
                 packed record
                     inxtyp,eltyp: types;
                     elref,low,high,elsize,size: index
                 end;
     btab:       array[1..bmax] of    {* block-table *}
                 packed record
                     last, lastpar, psize, vsize: index
                 end;
     stab:       packed array[0..smax] of char; {* string table *}
     rconst:     array[1..c2max] of real;
     code:       array[0..cmax] of order;
     psin,psout,prr,prd:text;      {* default in pascal p *}
     inf, outf, fprr: string;

procedure errormsg;
  var k : integer;
     msg: array[0..ermax] of alfa;
  begin
    msg[0] := 'undef id  ';    msg[1] := 'multi def ';
    msg[2] := 'identifier';    msg[3] := 'program   ';
msg[4] := ')         ';    msg[5] := ':         ';
    msg[6] := 'syntax    ';    msg[7] := 'ident,var '; 
msg[8] := 'of        ';    msg[9] := '(         ';
    msg[10] := 'id,array  ';    msg[11] := '(         ';
    msg[12] := ']         ';    msg[13] := '..        ';
    msg[14] := ';         ';    msg[15] := 'func. type';
    msg[16] := '=         ';    msg[17] := 'boolean   ';
    msg[18] := 'convar typ';    msg[19] := 'type      ';
    msg[20] := 'prog.param';    msg[21] := 'too big   ';
    msg[22] := '.         ';    msg[23] := 'type(case)';
    msg[24] := 'character ';    msg[25] := 'const id  ';
    msg[26] := 'index type';    msg[27] := 'indexbound';
    msg[28] := 'no array  ';    msg[29] := 'type id   ';
    msg[30] := 'undef type';    msg[31] := 'no record ';
    msg[32] := 'boole type';    msg[33] := 'arith type';
msg[34] := 'integer   ';    msg[35] := 'types     ';
    msg[36] := 'param type';    msg[37] := 'variab id ';
msg[38] := 'string    ';    msg[39] := 'no.of pars';
    msg[40] := 'real numbr';    msg[41] := 'type      ';
msg[42] := 'real type ';    msg[43] := 'integer   ';
    msg[44] := 'var,const ';    msg[45] := 'var,proc  ';
msg[46] := 'types(:=) ';    msg[47] := 'typ(case) ';
    msg[48] := 'type      ';    msg[49] := 'store ovfl';
    msg[50] := 'constant  ';    msg[51] := ':=        ';
    msg[52] := 'then      ';    msg[53] := 'until     ';
    msg[54] := 'do        ';    msg[55] := 'to downto ';
    msg[56] := 'begin     ';    msg[57] := 'end       ';
    msg[58] := 'factor';

    writeln(psout);
    writeln(psout,'key words');
    k := 0;
    while errs <> [] do
      begin
        while not( k in errs )do k := k + 1;
writeln(psout, k, ' ', msg[k] );
        errs := errs - [k]
end {* while errs *}
  end {* errormsg *} ;

procedure endskip; 
  begin                 {* underline skipped part of input *}
    while errpos < cc do
      begin
        write( psout, '-'); {*在错误的代码下用-来标识*}
        errpos := errpos + 1
      end;
    skipflag := false
  end {* endskip *};


procedure nextch;  {* read next character; process line end *}
  begin
    if cc = ll
    then begin
           if eof( psin )
           then begin
                  writeln( psout );
                  writeln( psout, 'program incomplete' );
                  errormsg;
                  exit;
                end;
           if errpos <> 0
           then begin
                  if skipflag then endskip;
                  writeln( psout );
                  errpos := 0
                end;
           write( psout, lc: 5, ' ');
           ll := 0;
           cc := 0;
           while not eoln( psin ) do
             begin
               ll := ll + 1;
               read( psin, ch );
               write( psout, ch );
               line[ll] := ch
             end;
           ll := ll + 1;
           readln( psin );
           line[ll] := ' ';
           writeln( psout );
         end;
         cc := cc + 1;
         ch := line[cc];
  end {* nextch *};

procedure error( n: integer );
begin
  if errpos = 0
  then write ( psout, '****' );
  if cc > errpos
  then begin
         write( psout, ' ': cc-errpos, '^', n:2);
errpos := cc + 3;
         errs := errs +[n]
end
end {* error *};

procedure fatal( n: integer );
  var msg : array[1..7] of alfa;
  begin
    writeln( psout );
    errormsg;
    msg[1] := 'identifier';   msg[2] := 'procedures';
    msg[3] := 'reals     ';   msg[4] := 'arrays    ';
msg[5] := 'levels    ';   msg[6] := 'code      ';
    msg[7] := 'strings   ';
writeln( psout, 'compiler table for ', msg[n], ' is too small');
    exit; {*terminate compilation *}
  end {* fatal *};

procedure insymbol;  {*reads next symbol*}
label 1,2,3;
  var  i,j,k,e: integer;
procedure readscale; {*处理2e5即2*10^5这种科学计数法的e后面的数据*}
    var s,sign: integer;
    begin
      nextch;
      sign := 1; {*符号默认为正号，即不出现正号就代表是正号*}
      s := 0;
      if ch = '+' 
      then nextch
      else if ch = '-'
           then begin
                  nextch;
                  sign := -1 {*标记上为负数*}
                end;
      if not(( ch >= '0' )and (ch <= '9' ))
      then error( 40 )
      else repeat
           s := 10*s + ord( ord(ch)-ord('0'));
           nextch;
           until not(( ch >= '0' ) and ( ch <= '9' ));
      e := s*sign + e
    end {* readscale *};

  procedure adjustscale; {*计算出科学计数法的实际数据*}
    var s : integer;
        d, t : real;
    begin
      if k + e > emax {*e前后的数据长度和不能超过emax*}
      then error(21)
      else if k + e < emin {*如果比emin(-292)还要小，已经超出精度范围了，直接赋值为0*}
           then rnum := 0 
           else begin {*快速幂求10^s，即不停地除2*}
                  s := abs(e);
                  t := 1.0;
                  d := 10.0;
                  repeat
                    while not odd(s) do
                      begin
                        s := s div 2;
                        d := sqr(d)
                      end;
                    s := s - 1;
                    t := d * t
                  until s = 0;
                  if e >= 0 then {*如果指数是正数，那么就直接相乘*}
                        rnum := rnum * t
                  else rnum := rnum / t {*否则就相除*}
            end
    end {* adjustscale *};

  procedure options;
    procedure switch( var b: boolean );
      begin
        b := ch = '+';
        if not b
        then if not( ch = '-' )
             then begin {* print error message *}
                    while( ch <> '*' ) and ( ch <> ',' ) do
                      nextch;
                  end
             else nextch
        else nextch
      end {* switch *};
    begin {* options  *}
      repeat
        nextch;
        if ch <> '*'
        then begin
               if ch = 't'
               then begin
                      nextch;
                      switch( prtables )
                    end
               else if ch = 's'
                  then begin
                          nextch;
                          switch( stackdump )
                       end;

             end
      until ch <> ','
    end {* options *};
  begin {* insymbol  *}
  1: while( ch = ' ' ) or ( ch = chr(9) ) do {*遇到空格或者指标符就继续读下一个字符*}
       nextch;    {* space & htab *}
    case ch of
      'a','b','c','d','e','f','g','h','i',
      'j','k','l','m','n','o','p','q','r',
      's','t','u','v','w','x','y','z':
        begin {* identifier of wordsymbol *}
          k := 0;
          id := '          ';
          repeat {*一直读到不是数字或者小写字符为止*}
            if k < alng {*规定了标识符的长度不能长于alng，即不能长于10*}
            then begin
                   k := k + 1;
                   id[k] := ch 
                end;
            nextch
          until not((( ch >= 'a' ) and ( ch <= 'z' )) or (( ch >= '0') and (ch <= '9' )));
          i := 1;
          j := nkw; {* binary search *}
          repeat {*下面这段是通过二分查找找到对应的那个key*}
            k := ( i + j ) div 2;
            if id <= key[k]
            then j := k - 1;
            if id >= key[k]
            then i := k + 1;
          until i > j;
          if i - 1 > j
          then sy := ksy[k]
          else sy := ident
        end;
      '0','1','2','3','4','5','6','7','8','9': {*下面这段是找出数字并把它存到inum里面，长度为k*}
        begin {* number *}
          k := 0;
          inum := 0;
          sy := intcon;
repeat
            inum := inum * 10 + ord(ch) - ord('0');
k := k + 1;
            nextch
          until not (( ch >= '0' ) and ( ch <= '9' ));
          if( k > kmax ) or ( inum > nmax ) {*数字的长度不能15，数字的大小不能大于32767*}
          then begin
                 error(21);
                 inum := 0;
                 k := 0
               end;
          if ch = '.' {*处理实数部分，即n.m这种*}
          then begin
                 nextch;
                 if ch = '.'
                 then ch := ':' {*..用法，即array[1..10]这种写法*}
                 else begin
                        sy := realcon; {*注明这个数是实数*}
                        rnum := inum; {*先赋值为inum即整数部分*}
                        e := 0;
                        while ( ch >= '0' ) and ( ch <= '9' ) do
                        begin
                            e := e - 1;
                            rnum := 10.0 * rnum + (ord(ch) - ord('0'));
                            nextch
                        end;
                        if e = 0
                        then error(40);
                        if ch = 'e'
                        then readscale; {*处理底数是实数例如2.3e10这种科学计数法的指数*}
                        if e <> 0 then adjustscale {*求出这个科学计数法的具体数据*}
                      end
                end
          else if ch = 'e' {*处理底数是整数的科学计数法*}
               then begin
                      sy := realcon;
                      rnum := inum;
                      e := 0;
                      readscale;
                      if e <> 0
                      then adjustscale
                    end;
        end;
      ':': {*处理冒号*}
        begin
          nextch;
          if ch = '=' {*代表是赋值号*}
          then begin
                 sy := becomes; {*赋值号*}
                 nextch
               end
          else  sy := colon {*单纯的冒号，例如定义*}
         end;
      '<':
        begin
          nextch;
          if ch = '='
          then begin
                 sy := leq; {*小于等于号*}
                 nextch
               end
          else
            if ch = '>'
            then begin
                   sy := neq;
                   nextch
                 end
            else  sy := lss
        end;
      '>':
        begin
          nextch;
          if ch = '='
          then begin
                 sy := geq; {*大于等于号*}
                 nextch
               end
          else  sy := gtr
        end;
      '.':
        begin
          nextch;
          if ch = '.'
          then begin
                 sy := colon; {*..，例如array里面的范围*}
                 nextch
               end
          else sy := period {*单纯的点号*}
        end;
      '''':
        begin
          k := 0;
   2:     nextch;
          if ch = ''''
          then begin
                 nextch;
                 if ch <> ''''
                 then goto 3 {*代表是单个字符即不是字符串*}
               end;
          if sx + k = smax {*超过了字符串的长度限制*}
          then fatal(7);
          stab[sx+k] := ch; {*保存到字符串常量表里*}
          k := k + 1; 
          if cc = 1
          then begin {* end of line *}
                 k := 0;
               end
          else goto 2;
   3:     if k = 1 
          then begin
                 sy := charcon; {*单个字符*}
                 inum := ord( stab[sx] )
               end
          else if k = 0 {*单个引号，报错*}
               then begin
                      error(38);
                      sy := charcon; {*容错，代表是单个字符*}
                      inum := 0
                    end
               else begin
                      sy := stringcon; {*代表是字符串*}
                      inum := sx; {*字符串出现的起始位置*}
                      sleng := k; {*字符串的长度}
                      sx := sx + k {*所有的字符的个数*}
                    end
        end;
      '(': 
        begin
          nextch;
          if ch <> '*' {*不是(**}
          then sy := lparent
          else begin {* comment *}
                 nextch;
                 if ch = '$' {*($的写法，是编译的pascal文件的参数*}
                 then options; 
                 repeat
                   while ch <> '*' do nextch; {*一直处理到**}
                   nextch
                 until ch = ')'; {*最后应该是个)*}
                 nextch;
                 goto 1 {*返回到1继续处理*}
               end
        end;
      '{': 
        begin
          nextch;
          if ch = '$'	{*处理程序的参数*}
          then options;
          while ch <> '}' do
            nextch;
          nextch;
          goto 1
        end;
      '+', '-', '*', '/', ')', '=', ',', '[', ']', ';':
        begin
          sy := sps[ch]; 
          nextch
        end;
      '$','"' ,'@', '?', '&', '^', '!':
        begin
          error(24);
          nextch;
          goto 1
        end
      end {* case *}
    end {* insymbol *};

procedure enter(x0:alfa; x1:objecttyp; x2:types; x3:integer );
  begin
    t := t + 1;    {* enter standard identifier *}
    with tab[t] do {*把当前标识符t的信息录入到符号表里*}
      begin
        name := x0; 
        link := t - 1; {*指向前一个标识符*}
        obj := x1; {*种类*}
        typ := x2; {*类型*}
        ref := 0;
        normal := true; {*这个变量除了变参都是true*}
        lev := 0;
        adr := x3; {*在运行栈S中分配存储单元的相对地址*}
      end
  end; {* enter *}

procedure enterarray( tp: types; l,h: integer ); {*记录数组信息报atab*}
  begin
    if l > h {*下界显然要小于上界*}
    then error(27);
    if( abs(l) > xmax ) or ( abs(h) > xmax ) {*不允许超过大小限制*}
    then begin
           error(27);
           l := 0;
           h := 0;
         end;
    if a = amax 
    then fatal(4)
    else begin
           a := a + 1; {*表里元素的个数增加*}
           with atab[a] do
             begin
               inxtyp := tp;
               low := l; {*atab[a].low = l;*}
               high := h
             end
         end
  end {* enterarray *};

procedure enterblock; {*进入下一个分程序块*}
  begin
    if b = bmax {*只允许有至多20个分程序块*}
    then fatal(2)
    else begin 
           b := b + 1;
           btab[b].last := 0; {*指向这个过程最后一个符号在符号表的位置*}
           btab[b].lastpar := 0; {*指向这个过程的最后一个参数在符号表的位置*}
         end
  end {* enterblock *};

procedure enterreal( x: real ); {填实型变量}
  begin
    if c2 = c2max - 1
    then fatal(3)
    else begin
           rconst[c2+1] := x;
           c1 := 1;
           while rconst[c1] <> x do {*实常量表中不允许出现重复的项，因此找前面有没有出现过x*}
             c1 := c1 + 1;
           if c1 > c2 {*注意在这里我们没有更新inc(c2), 因此直接通过c1来更新，c1最大是c2+1*}
           then  c2 := c1
         end
  end {* enterreal *};

procedure emit( fct: integer ); {*生成PCode方法，没有操作数*}
  begin
    if lc = cmax {*超出了生成代码的数量限制，即800*}
    then fatal(6);
    code[lc].f := fct; {*操作码等于fct*}
    lc := lc + 1 {*代表生成的指令数增加一个*}
  end {* emit *};


procedure emit1( fct, b: integer ); {*生成PCode方法，有一个操作数*}
  begin
    writeln('fct = ',fct,' b= ',b);
    if lc = cmax {*超出了生成代码的数量限制，即800*}
    then fatal(6); 
    with code[lc] do {*修改P代表表code的记录值*}
      begin
        f := fct;
        y := b; {*???是说如果只有一个操作数，只填到y吗*}
      end;
    lc := lc + 1 {*代表生成的指令数增加一个*}
  end {* emit1 *};

procedure emit2( fct, a, b: integer ); {*生成PCode方法，有2个操作数*}
  begin
    if lc = cmax then fatal(6); {*超出了生成代码的数量限制，即800*}
    with code[lc] do {*修改P代表表code的记录值*}
      begin
        f := fct;
        x := a; {*x操作数*}
        y := b {*y操作数*}
      end;
    lc := lc + 1; {*代表生成的指令书增加一个*}
end {* emit2 *};

procedure printtables; {*打印Pcode表*}
  var  i: integer;
    o: order;
      mne: array[0..omax] of
           packed array[1..5] of char;
  begin {*下面是各种操作码对应的助记符，即动作*}
    mne[0] := 'LDA  ';   mne[1] := 'LOD  ';  mne[2] := 'LDI  ';
mne[3] := 'DIS  ';   mne[8] := 'FCT  ';  mne[9] := 'INT  ';
    mne[10] := 'JMP  ';   mne[11] := 'JPC  ';  mne[12] := 'SWT  ';
    mne[13] := 'CAS  ';   mne[14] := 'F1U  ';  mne[15] := 'F2U  ';
    mne[16] := 'F1D  ';   mne[17] := 'F2D  ';  mne[18] := 'MKS  ';
    mne[19] := 'CAL  ';   mne[20] := 'IDX  ';  mne[21] := 'IXX  ';
    mne[22] := 'LDB  ';   mne[23] := 'CPB  ';  mne[24] := 'LDC  ';
mne[25] := 'LDR  ';   mne[26] := 'FLT  ';  mne[27] := 'RED  ';
mne[28] := 'WRS  ';   mne[29] := 'WRW  ';  mne[30] := 'WRU  ';
    mne[31] := 'HLT  ';   mne[32] := 'EXP  ';  mne[33] := 'EXF  ';
    mne[34] := 'LDT  ';   mne[35] := 'NOT  ';  mne[36] := 'MUS  ';
mne[37] := 'WRR  ';   mne[38] := 'STO  ';  mne[39] := 'EQR  ';
mne[40] := 'NER  ';   mne[41] := 'LSR  ';  mne[42] := 'LER  ';
    mne[43] := 'GTR  ';   mne[44] := 'GER  ';  mne[45] := 'EQL  ';
mne[46] := 'NEQ  ';   mne[47] := 'LSS  ';  mne[48] := 'LEQ  ';
    mne[49] := 'GRT  ';   mne[50] := 'GEQ  ';  mne[51] := 'ORR  ';
    mne[52] := 'ADD  ';   mne[53] := 'SUB  ';  mne[54] := 'ADR  ';
    mne[55] := 'SUR  ';   mne[56] := 'AND  ';  mne[57] := 'MUL  ';
    mne[58] := 'DIV  ';   mne[59] := 'MOD  ';  mne[60] := 'MUR  ';
    mne[61] := 'DIR  ';   mne[62] := 'RDL  ';  mne[63] := 'WRL  ';

    writeln(psout); {*写入到psout文件里去*}
    writeln(psout);
    writeln(psout);
    writeln(psout,'   identifiers  link  obj  typ  ref  nrm  lev  adr');
    writeln(psout);
    for i := btab[1].last to t do {*不需要输出0..btab[1].last，因为btab[1].last也就是28也就是把所有Pascal自带类型都存进去的位置，这个后面就是我们自己定义的变量了*}
    with tab[i] do
        writeln( psout, i,' ', name, link:5, ord(obj):5, ord(typ):5,ref:5, ord(normal):5,lev:5,adr:5);
    writeln( psout );
    writeln( psout );
    writeln( psout );
    writeln( psout, 'blocks   last  lpar  psze  vsze' );
    writeln( psout );
    for i := 1 to b do {*输出所有block的分程序表*}
       with btab[i] do
         writeln( psout, i:4, last:9, lastpar:5, psize:5, vsize:5 );
    writeln( psout );
    writeln( psout );
    writeln( psout );
    writeln( psout, 'arrays xtyp etyp eref low high elsz size');
    writeln( psout );
    for i := 1 to a do {*输出所有的数组信息的向量表*}
      with atab[i] do
        writeln( psout, i:4, ord(inxtyp):9, ord(eltyp):5, elref:5, low:5, high:5, elsize:5, size:5);
    writeln( psout );
    writeln( psout );
    writeln( psout );
    writeln( psout, 'code:');
    writeln( psout );
    for i := 0 to lc-1 do {*打印最终的所有Pcode*}
    begin
        write( psout, i:5 );
        o := code[i];  
        write( psout, mne[o.f]:8, o.f:5 );
        if o.f < 31 {*操作码大于等于31的都代表是无操作数的操作*}
        then if o.f < 4 {*小于4的操作码代表是所有*}
             then write( psout, o.x:5, o.y:5 )
             else write( psout, o.y:10 )
        else write( psout, '          ' );
        writeln( psout, ',' )
      end;
      writeln( psout );
      writeln( psout, 'Starting address is ', tab[btab[1].last].adr:5 ) {*输出Pcode程序的起始地址，相当于主函数begin下的第一句的起始地址，因为生成的PCode的顺序是按照程序的顺序的，即从上而下生成，而上面可能有很多函数这样。*}
      {*之前有tab[prt].adr := lc;就是把这一函数或者过程(即block)的起始地址是哪一句Pcode,例如tab[28]=11就是第一个block也就是主程序的入口*}
    end {* printtables *};


procedure block( fsys: symset; isfun: boolean; level: integer );
  type conrec = record
                  case tp: types of
                    ints, chars, bools : ( i:integer );
                    reals :( r:real )
                end;
  var dx : integer ;  {* data allocation index *}
      prt: integer ;  {* t-index of this procedure *}
      prb: integer ;  {* b-index of this procedure *}
x  : integer ;

  procedure skip( fsys:symset; n:integer); {*第n号错误，处理错误的函数，跳过错误的这些代码*}
begin
      error(n);
      skipflag := true;
      while not ( sy in fsys ) do {*把所有不在fsys里的标识符全跳过去*}
        insymbol;
      if skipflag then endskip
    end {* skip *};

  procedure test( s1,s2: symset; n:integer ); {*检查当前符号sy是否合法*}
    begin
      if not( sy in s1 ) {*sy在s1中就是合法的*}
      then skip( s1 + s2, n )
    end {* test *};

  procedure testsemicolon; {*检查分号是否合法*}
    begin
      if sy = semicolon {*如果是分号就合法*}
      then insymbol
      else begin
             error(14);
             if sy in [comma, colon] {*!!!如果是逗号或者句号，进行一定的容错处理*} 
             then insymbol
           end;
      test( [ident] + blockbegsys, fsys, 6 )
    end {* testsemicolon *};


  procedure enter( id: alfa; k:objecttyp ); {*将标识符id填入符号表*}
    var j,l : integer;
    begin
      if t = tmax {*符号表已经填满了，即100个*}
      then fatal(1)
      else begin
             tab[0].name := id; {*临时存在第0号符号表里，马上会用到*}
             j := btab[display[level]].last; {*当前层次中最后一个标识符在符号表的位置*}
             l := j;
             while tab[j].name <> id do {*一直往前找，只要和id相等就停下来*}
               j := tab[j].link;
             if j <> 0 {*说明已经出现过了，报错*}
             then error(1)
             else begin {*加入到符号表里*}
                    t := t + 1; {*标识符数量增加一个*}
                    with tab[t] do {*填写这个标识符的记录信息*}
                      begin
                        name := id; {*id是传进来的参数*}
                        link := l; {*指向之前的最后一个标识符*}
                        obj := k; {*这个标识符的种类，代表常量、变量、类型、过程和函数*}
                        typ := notyp; {*具体的类型，例如整型、实型等等*}
                        ref := 0; {*最普通的标识符，ref为0*}
                        lev := level; {*标识符所在程序的层次*}
                        adr := 0;
                        normal := false {* initial value } {???*}
                      end;
                    btab[display[level]].last := t {*当前层的最后一个标识符的位置变了*}
                  end
           end
    end {* enter *};

  function loc( id: alfa ):integer; {*查找id在符号表的位置在哪里*}
    var i,j : integer;        {* locate if in table *}
    begin
        i := level;
        tab[0].name := id;  {* sentinel *}
        repeat
            j := btab[display[i]].last; {*从这个块的最后一个变量开始*}
            while tab[j].name <> id do
                j := tab[j].link; {*不停地向前匹配是不是名字一样*}
            i := i - 1; {保证可以查过所有的标识符，包括类型名(相当于都在level为0的里面)}
        until ( i < 0 ) or ( j <> 0 ); 
        if j = 0 {*没有找到这个标识符，说明定义的类型不存在，报错*}
        then error(0);
        loc := j
    end {* loc *} ;

  procedure entervariable; {*将变量填入符号表*}
    begin
      if sy = ident
      then begin
             enter( id, vvariable ); {*主要是放到了enter里面去执行*}
             insymbol
           end
      else error(2)
    end {* entervariable *};

  procedure constant( fsys: symset; var c: conrec ); {*处理常量，变参c用于返回常量的信息*}
    var x, sign : integer;
    begin
      c.tp := notyp; {*常量类型是无变量类型*}
      c.i := 0;
      test( constbegsys, fsys, 50 );
      if sy in constbegsys {*首先出现的是const，继续分析*}
      then begin
             if sy = charcon {*对于字符类型的常量*}
             then begin
                    c.tp := chars; {*类型是char*}
                    c.i := inum; {*存储它的ascii码值*}
                    insymbol
                  end
             else begin
                  sign := 1; {*非char类型的常量*}
                  if sy in [plus, minus] {*有前导的正号或者负号*}
                  then begin
                         if sy = minus {*如果有-号就变号*}
                         then sign := -1;
                         insymbol 
                       end;
                  if sy = ident {*接下来是标识符*}
                  then begin
                         x := loc(id); {*在符号表中找一下，例如const N=M, M是之前定义的常量. ???没有找到是没有处理吗*} 
                         if x <> 0
                         then
                           if tab[x].obj <> konstant {*如果对应的不是常量类型，那么就报错*}
                           then error(25)
                           else begin
                                  c.tp := tab[x].typ; {*读取这个常量的类型*}
                                  if c.tp = reals
                                  then c.r := sign*rconst[tab[x].adr] {*实型就从rconst里面去读*}
                                  else c.i := sign*tab[x].adr {*整形就从符号表的adr里去读*}
                                end;
                         insymbol
                       end
                  else if sy = intcon {*读入到整数*}
                       then begin
                              c.tp := ints; {*整型*}
                              c.i := sign*inum; 
                              insymbol
                            end
                       else if sy = realcon {*读入到实数*}
                            then begin
                                   c.tp := reals; {*实型*}
                                   c.r := sign*rnum;
                                   insymbol
                                 end
                            else skip(fsys,50) {*跳过无用符号*}
                end;
                test(fsys,[],6)
           end
    end {* constant *};

procedure typ( fsys: symset; var tp: types; var rf,sz:integer );
    var eltp : types;
        elrf, x : integer;
    elsz, offset, t0, t1 : integer;

procedure arraytyp( var aref, arsz: integer ); {*处理数组类型*}
var eltp : types; {*数组保存的是什么类型的变量*}
      low, high : conrec; {*下界和上界，例如array[1..100]的下界为1，上界为100*}
      elrf, elsz: integer; {*数组元素的大小和改数组的大小*}
      begin
        constant( [colon, rbrack, rparent, ofsy] + fsys, low); {*求数组的下界*}
        if low.tp = reals {*下界的类型是实型，报错*}
        then begin
               error(27);
               low.tp := ints; {*容错，把它设为整形*}
               low.i := 0 {*容错，下界为0*}
             end;
        if sy = colon {*下界之后跟着的是.. 类型是句号???这里的colon是..还是.*}
        then insymbol
        else error(13);
        constant( [rbrack, comma, rparent, ofsy ] + fsys, high ); 
        if high.tp <> low.tp {*上下界类型不一样，报错*}
        then begin
               error(27);
               high.i := low.i {*容错，使得上界类型和下界是一样的*}
             end;
        enterarray( low.tp, low.i, high.i ); {*将数组类型的信息填入到符号表里*}
        aref := a; {*找到在atab中的位置*}
        if sy = comma {*遇到逗号了，即例如array[1..100,1..100]这样的二维数组*}
        then begin
               insymbol;
               eltp := arrays;
               arraytyp( elrf, elsz ) {*递归调用该过程处理数组的元素，即每一维都放到数组符号表里*}
            end
        else begin
               if sy = rbrack {*遇到右中括号了*}
               then insymbol 
               else begin
                      error(12);
                      if sy = rparent {*右括号就容错*}
                      then insymbol
                    end;
               if sy = ofsy {*遇到了of*}
               then insymbol
               else error(8);
               typ( fsys, eltp, elrf, elsz) {*处理后面的typ类型*}
             end;
             with atab[aref] do {*填这个数组的信息*}
               begin
                 arsz := (high-low+1) * elsz; {*计算数组的大小*}
                 size := arsz; {*保存到atab[size]里*}
                 eltyp := eltp; {*保存数组的元素是什么类型*}
                 elref := elrf; {*保存数组从哪儿开始登陆的, 之前的typ过程的elrf是变参*}
                 elsize := elsz {*保存每个元素的大小*}
               end
      end {* arraytyp *};
    begin {* typ  } {处理类型的开始*}
      tp := notyp; {*存储该符号是什么类型，默认是无类型变量*}
      rf := 0; {*用来存储该符号在符号表的位置*}
      sz := 0; {*用来存储该类型的大小*}
      test( typebegsys, fsys, 10 ); {*检测是不是数组、记录或者标识符类型*}
      if sy in typebegsys
      then begin
             if sy = ident {*如果是标识符类型*}
             then begin
                    x := loc(id); {*找在符号表的位置*}
                    if x <> 0 {*如果找到了*}
                    then with tab[x] do {*将x的信息保存到变参里*}
                           if obj <> typel
                           then error(29)
                           else begin
                                  tp := typ; {*相当于tp := tab[x].typ;*}
                                  rf := ref; {*相当于rf := tab[x].ref;*}
                                  sz := adr; {*相当于sz := tab[x].adr;*}
                                  if tp = notyp {*如果是未定义的类型，则报错*}
                                  then error(30)
                                end;
                    insymbol
                  end
             else if sy = arraysy {*如果是数组类型的标识符*}
                  then begin
                         insymbol;
                         if sy = lbrack {*遇到左中括号，读入下一个标识符*}
                         then insymbol
                         else begin
                                error(11);
                                if sy = lparent {*容错，对于左小括号*}
                                then insymbol
                              end;
                         tp := arrays; {*tp是数组类型的变量*}
                         arraytyp(rf,sz) {*获得数组在数组符号表的位置和数组的大小*}
                         end
             else begin {* records } {处理是记录类型的标识符} {例如item = record typ: types; ref: index; end;*}
                    insymbol;
                    enterblock;
                    tp := records;
                    rf := b; {*???rf指向当前过程在符号表的位置*}
                    if level = lmax {*当前嵌套已经到了lmax就不能再嵌套处理了*}
                    then fatal(5);
                    level := level + 1; {*继续嵌套*}
                    display[level] := b; {*设置当前层次的display区*}
                    offset := 0;
                    while not ( sy in fsys - [semicolon,comma,ident]+ [endsy] ) do
                      begin {* field section *}
                        if sy = ident
                        then begin
                               t0 := t; {*将t0赋值为当前符号的位置*}
                               entervariable; {*把变量填入符号表*}
                               while sy = comma do {*同种变量间是用逗号隔开的*}
                                 begin
                                   insymbol; 
                                   entervariable
                                 end;
                               if sy = colon {*如果是冒号，这个变量都读完了，读类型*}
                               then insymbol
                               else error(5);
                               t1 := t;
                               typ( fsys + [semicolon, endsy, comma,ident], eltp, elrf, elsz ); {*递归调用typ处理记录类型的其它成员变量*}
                               while t0 < t1 do {*填写t0~t1中的信息*}
                               begin
                                 t0 := t0 + 1; 
                                 with tab[t0] do
                                   begin
                                     typ := eltp; {*eltp是之前递归调用的typ的变参*}
                                     ref := elrf; {*elrf..,*}
                                     normal := true; {*给normal标记复制*}
                                     adr := offset; {*相对于起始地址的位移*}
                                     offset := offset + elsz {*下一个变量的唯一*}
                                   end
                               end
                             end; {* sy = ident *}
                        if sy <> endsy {*遇到end则代表records的成员声明结束了*}
                        then begin
                               if sy = semicolon {*end后面必须跟分号*}
                               then insymbol
                               else begin
                                      error(14);
                                      if sy = comma {*容错，允许是逗号*}
                                      then insymbol
                                    end;
                                    test( [ident,endsy, semicolon],fsys,6 )
                             end
                      end; {* field section *}
                    btab[rf].vsize := offset; {*???*}
                    sz := offset; {*储存了它占用空间的总数*}
                    btab[rf].psize := 0; {*records没有参数占用空间*}
                    insymbol; 
                    level := level - 1 {*level结束后level减少1*}
                  end; {* record *}
             test( fsys, [],6 )
           end;
      end {* typ *};

  procedure parameterlist; {* formal parameter list  } {处理参数列表*}
    var tp : types;
        valpar : boolean;
        rf, sz, x, t0 : integer;
    begin
      insymbol;
      tp := notyp; {*初始化 下同*}
      rf := 0;
      sz := 0;
      test( [ident, varsy], fsys+[rparent], 7 );
      while sy in [ident, varsy] do {*一直是标识符或者var就一直进行*}
      begin
          if sy <> varsy
          then valpar := true {*值形参就填值*}
          else begin
                 insymbol;
                 valpar := false {*变量形参就传引用*}
          end;
          t0 := t; {*记录当前符号表的栈顶位置*}
          entervariable; 
          while sy = comma do {*遇到逗号，var x,y: integer; 同类型的继续放入符号表*}
          begin
              insymbol;
              entervariable; {*继续填到符号表*}
          end;
          if sy = colon {*遇到冒号，处理后面的类型*}
          then begin
                 insymbol;
                 if sy <> ident {*不是标识符就报错*}
                 then error(2) 
                 else begin
                    x := loc(id); {*找到这个标识符在符号表里的位置*}
                    insymbol;
                    if x <> 0 {*找到了这个标识符*}
                    then with tab[x] do {*拿到这个标识符的记录信息*}
                    if obj <> typel {*如果不是type，报错*}
                    then error(29)
                    else begin
                        tp := typ; {*获取参数的类型给tp*}
                        rf := ref; {*获取参数在符号表的位置*}
                        if valpar {*如果是值形参*}
                        then sz := adr {*sz为在符号表中的位置*}
                        else sz := 1 {*如果不是值形参，标记sz为1*}
                    end;
                end;
                test( [semicolon, rparent], [comma,ident]+fsys, 14 )
            end
            else error(5); {*必须是冒号，否则报错*}
          while t0 < t do {*我们之前在entervariable每次填符号表总会加上1，因此t0~t都是新处理的标识符*}
            begin
              t0 := t0 + 1; 
              with tab[t0] do {*把这个符号表里的信息填完整*}
                begin
                  typ := tp; {*设置类型是tp*}
                  ref := rf; {*设置位置是类型在符号表的位置*}
                  adr := dx; {*???设置形参的相对地址*}
                  lev := level; {*设置形参的level*}
                  normal := valpar; {*设置是不是形参的标记*}
                  dx := dx + sz {*???更新位移*}
                end
            end;
            if sy <> rparent {*不是右括号，说明刚刚那个声明之后还有东西*}
            then begin
                   if sy = semicolon {*分号,ok继续while循环来处理*} 
                   then insymbol
                   else begin
                          error(14);
                          if sy = comma {*容错，允许逗号隔开*}
                          then insymbol
                        end;
                        test( [ident, varsy],[rparent]+fsys,6)
                 end
        end {* while *};
      if sy = rparent {*函数参数的最后应该是一个右括号，之后如果是函数就是: integer;这种，过程就是;*}
      then begin
             insymbol;
             test( [semicolon, colon],fsys,6 )
           end
      else error(4)
    end {* parameterlist *};


procedure constdec; {*处理常量声明*}
    var c : conrec;
    begin
      insymbol;
      test([ident], blockbegsys, 2 );
      while sy = ident do {*???如果是标识符，一直处理*}
        begin
          enter(id, konstant); {*将常量的名字加入到符号表*}
          insymbol;
          if sy = eql {*如果是等号*}
          then insymbol {*读下一个标识符*}
          else begin
                 error(16);
                 if sy = becomes
                 then insymbol
               end;
          constant([semicolon,comma,ident]+fsys,c); {*求常量sy的值和类型*}
          tab[t].typ := c.tp; {*把常量填到符号表*}
          tab[t].ref := 0;
          if c.tp = reals {*如果是实数的常量*}
          then begin
                enterreal(c.r);
                tab[t].adr := c1; {*保存到实数*}
          end
          else tab[t].adr := c.i;
          testsemicolon
        end
    end {* constdec *};

  procedure typedeclaration; {*处理类型的申明*}
    var tp: types;
        rf, sz, t1 : integer;
    begin
      insymbol;
      test([ident], blockbegsys,2 ); {*检查获取到的类型*}
      while sy = ident do 
        begin
          enter(id, typel); {*将变量的类型和名字填到表里*}
          t1 := t; 
          insymbol;
          if sy = eql {找到了=号，处理后面的了}
          then insymbol
          else begin
                 error(16);
                 if sy = becomes {*容错，允许写成赋值号*}
                 then insymbol
               end;
          typ( [semicolon,comma,ident]+fsys, tp,rf,sz ); {*类型，位置，大小*}
          with tab[t1] do
            begin
              typ := tp;
              ref := rf;
              adr := sz
            end;
          testsemicolon
        end
    end {* typedeclaration *};

  procedure variabledeclaration; {*变量的声明*}
    var tp : types;
        t0, t1, rf, sz : integer;
    begin
      insymbol;
      while sy = ident do
      begin
          t0 := t; {*栈顶位置*}
          entervariable;
          while sy = comma do {*遇到逗号代表是同类型的，全都放到符号表里*}
            begin
              insymbol;
              entervariable;
            end;
          if sy = colon {*冒号代表前面一坨var的变量读完了*}
          then insymbol
          else error(5);
          t1 := t;
          typ([semicolon,comma,ident]+fsys, tp,rf,sz );
          while t0 < t1 do {*把从t0到t1里的符号表里的信息都更新*}
            begin
              t0 := t0 + 1;
              with tab[t0] do
                begin
                  typ := tp;
                  ref := rf;
                  lev := level;
                  adr := dx;
                  normal := true;
                  dx := dx + sz
                end
            end;
          testsemicolon
        end
    end {* variabledeclaration *};

  procedure procdeclaration; {*过程的声明，即procedure work...或function sum...*}
    var isfun : boolean;
    begin
      isfun := sy = funcsy; {*isfun代表不是函数，这后面的是个条件表但是，即判断sy是不是函数*}
      insymbol;
      if sy <> ident
      then begin
             error(2);
             id :='          '
           end;
      if isfun
      then enter(id,funktion) {*如果是函数就把函数名填到符号表*}
      else enter(id,prozedure); {*否则把过程名填到符号表*}
      tab[t].normal := true; {*初始为true*}
      insymbol;
      block([semicolon]+fsys, isfun, level+1 ); {*递归调用block*}
      if sy = semicolon
      then insymbol
      else error(14);
      emit(32+ord(isfun)) {*exit*}
    end {* proceduredeclaration *};


procedure statement( fsys:symset ); {*最外层的处理表达式的值*}
    var i : integer; {*当前在处理tab[i]*}

procedure expression(fsys:symset; var x:item); forward; {*forward代表内层的过程能调用外层的。变量形参x可以求得表达式结果的类型*}
    procedure selector(fsys:symset; var v:item); {*j结构变量的处理*}
    var x : item;
        a,j : integer;
    begin {* sy in [lparent, lbrack, period] *}
      repeat
        if sy = period {*如果是单个句号，开始识别句号后面的标识符*}
        then begin
               insymbol; {* field selector *}
               if sy <> ident
               then error(2)
               else begin
                      if v.typ <> records {*不是记录类型显然没有.这种操作，报错*}
                      then error(31)
                      else begin {* search field identifier *}
                             j := btab[v.ref].last; {*找到这个结构体的最后一个变量*}
                             tab[0].name := id; 
                             while tab[j].name <> id do
                               j := tab[j].link;
                             if j = 0 {*之前符号表里没有id，报错*}
                             then error(0);
                             v.typ := tab[j].typ; {*把找到的记录的类型赋值给v*}
                             v.ref := tab[j].ref; {*把找到的记录的类型所在的btab位置赋值给v*}
                             a := tab[j].adr; {*与这个记录起始的偏移*}
                             if a <> 0
                             then emit1(9,a) {*INT指令，栈顶元素加上偏移*}
                           end;
                      insymbol
                    end
             end
        else begin {* array selector *} {*处理数组*}
               if sy <> lbrack {*左括号开始*}
               then error(11);
               repeat
                 insymbol;
                 expression( fsys+[comma,rbrack],x);
                 if v.typ <> arrays {*必须是数组类型*}
                 then error(28)
                 else begin
                        a := v.ref;
                        if atab[a].inxtyp <> x.typ
                        then error(26)
                        else if atab[a].elsize = 1 {*代表是变参???*}
                             then emit1(20,a) {*IDX*}
                             else emit1(21,a); {*IXX*}
                        v.typ := atab[a].eltyp;
                        v.ref := atab[a].elref {*在atab的位置*}
                      end
               until sy <> comma; {*不是逗号，则返回操作*}
               if sy = rbrack {*右中括号*}
               then insymbol
               else begin
                      error(12);
                      if sy = rparent {*容错，允许出现右小括号*}
                      then insymbol
                    end
             end
      until not( sy in[lbrack, lparent, period]);
      test( fsys,[],6)
    end {* selector *};

    procedure call( fsys: symset; i:integer ); {*处理函数和过程调用，i是在符号表中的位置*}
    var x : item;
          lastp,cp,k : integer;
    begin
        writeln('call!!!');
        emit1(18,i); {* mark stack *} {*MKS，标记栈*}
        lastp := btab[tab[i].ref].lastpar; {*最后一个参数在符号表的位置*}
        cp := i;
        writeln('cp = ',cp,' lastp = ', lastp,' name= ',tab[cp].name);
        if sy = lparent {*左小括号，代表参数列表的开始*}
        then begin {* actual parameter list *}
               repeat
                 insymbol;
                 if cp >= lastp {*当前参数的位置显然不能超过在符号表的位置*}
                 then error(39)
                 else begin
                        cp := cp + 1; {*cp指针往后移动一个*}
                        writeln('name = ',tab[cp].name);
                        if tab[cp].normal {*false:变参; true:值参*}
                        then begin {* value parameter *} {*值形参*}
                               expression( fsys+[comma, colon,rparent],x); {*递归处理表达式x*}
                               writeln('x.typ = ', x.typ, 'tab[cp].cp = ',tab[cp].typ, 'x.ref = ',x.ref);
                               if x.typ = tab[cp].typ 
                               then begin
                                      if x.ref <> tab[cp].ref {*指向要相同*}
                                      then error(36)
                                      else if x.typ = arrays
                                           then emit1(22,atab[x.ref].size) {*实参表达式的值装入块???*}
                                           else if x.typ = records
                                                then emit1(22,btab[x.ref].vsize)
                                    end
                               else if ( x.typ = ints ) and ( tab[cp].typ = reals ) {*调用的类型是整形，但参数要求是实型*}
                                    then emit1(26,0) {*转换成浮点数*}
                                    else if x.typ <> notyp 
                                         then error(36);
                             end
                        else begin {* variable parameter *} {变参}
                               if sy <> ident
                               then error(2)
                               else begin
                                      k := loc(id); 
                                      writeln('id = ',id,'k = ',k);
                                      insymbol;
                                      if k <> 0
                                      then begin
                                             if tab[k].obj <> vvariable {*获取到的类型必须是变量类型*}
                                             then error(37);
                                             x.typ := tab[k].typ;
                                             x.ref := tab[k].ref;
                                             if tab[k].normal
                                             then emit2(0,tab[k].lev,tab[k].adr) {*LDA, 把变量装入栈顶???*}
                                              else emit2(1,tab[k].lev,tab[k].adr);
                                             if sy in [lbrack, lparent, period] {*[(.*}
                                             then selector(fsys+[comma,colon,rparent],x); {*递归*}
                                             if ( x.typ <> tab[cp].typ ) or ( x.ref <> tab[cp].ref )
                                             then error(36)
                                           end
                                    end
                             end {*variable parameter *}
                      end;
                 test( [comma, rparent],fsys,6)
               until sy <> comma; {*出现逗号，参数调用结束了*}
               if sy = rparent
               then insymbol
               else error(4)
             end;
        if cp < lastp {*参数列表到最后也没打到最后一个符号的位置，说明参数少了*}
        then error(39); {* too few actual parameters *}
        emit1(19,btab[tab[i].ref].psize-1 );
        if tab[i].lev < level {*符号表当前层次小于level更新???*}
        then emit2(3,tab[i].lev, level ) {*LDI,间接装入???*}
      end {* call *};

    function resulttype( a, b : types) :types; {*处理类型转换*}
      begin
        if ( a > reals ) or ( b > reals )
        then begin
               error(33);
               resulttype := notyp
             end
        else if ( a = notyp ) or ( b = notyp ) {*一个操作数无类型*}
             then resulttype := notyp {*返回无类型*}
             else if a = ints 
                  then if b = ints 
                       then resulttype := ints{*a是int，b也是int，返回int*}
                       else begin
                              resulttype := reals; {*转化成实型*}
                              emit1(26,1)
                            end
                  else begin
                         resulttype := reals; {*a是实型，返回实型*}
                         if b = ints
                         then emit1(26,0)
                       end
      end {* resulttype *} ;

    procedure expression( fsys: symset; var x: item ); {*处理表达式*}
    var y : item;
          op : symbol;

      procedure simpleexpression( fsys: symset; var x: item ); {*简单表达式*}
        var y : item;
            op : symbol;

        procedure term( fsys: symset; var x: item ); {*处理一个项*}
          var y : item;
              op : symbol;

          procedure factor( fsys: symset; var x: item ); {*因子*}
            var i,f : integer;

            procedure standfct( n: integer ); {*标准函数*}
              var ts : typset;
              begin  {* standard function no. n *}
                if sy = lparent {*左括号*}
                then insymbol
                else error(9);
                if n < 17
                then begin
                       expression( fsys+[rparent], x );
                       case n of {*n代表不同的操作*}
                       {* abs, sqr *} 0,2: begin
                                           ts := [ints, reals];
                                           tab[i].typ := x.typ;
                                           if x.typ = reals
                                           then n := n + 1
                                         end;
                       {* odd, chr *} 4,5: ts := [ints];
                       {* odr *}        6: ts := [ints,bools,chars];
                       {* succ,pred *} 7,8 : begin
                                             ts := [ints, bools,chars];
                                             tab[i].typ := x.typ
                                           end;
                       {* round,trunc *} 9,10,11,12,13,14,15,16:
                       {* sin,cos,... *}     begin
                                             ts := [ints,reals];
                                             if x.typ = ints
                                             then emit1(26,0)
                                           end;
                     end; {* case *}
                     if x.typ in ts {*函数的类型在刚刚出现过*}
                     then emit1(8,n) {*d调用标准函数*}
                     else if x.typ <> notyp
                          then error(48);
                   end
                else begin {* n in [17,18] *}
                       if sy <> ident
                       then error(2)
                       else if id <> 'input    '
                            then error(0)
                            else insymbol;
                       emit1(8,n);
                     end;
              x.typ := tab[i].typ; {*返回值类型*}
              if sy = rparent
                then insymbol
                else error(4)
              end {* standfct *} ;
            begin {* factor *}
              x.typ := notyp; {*初始化返回值类型，默认为无类型*}
              x.ref := 0;
              test( facbegsys, fsys,58 ); 
              while sy in facbegsys do {*因子开始符号*} 
                begin
                  if sy = ident {*识别到标识符*}
                  then begin
                         i := loc(id); {*标识符的位置是i*}
                         insymbol;
                         with tab[i] do 
                           case obj of
                             konstant: begin {*常量类型*}
                                         x.typ := typ;
                                         x.ref := 0;
                                         if x.typ = reals
                                          then emit1(25,adr)
                                         else emit1(24,adr)
                                       end;
                             vvariable:begin {*变量类型*}
                                         x.typ := typ;
                                         x.ref := ref;
                                         if sy in [lbrack, lparent,period] {*如果这个标识符后面跟的是[,(,. 那么说明不是简单的一个变量，而是什么数组啊函数啊记录啊等等*}
                                         then begin
                                                if normal
                                                then f := 0
                                                 else f := 1;
                                                emit2(f,lev,adr);
                                                selector(fsys,x);
                                                if x.typ in stantyps
                                                then emit(34)
                                              end
                                         else begin {*是一个简单的变量，例如整数ans*}
                                                if x.typ in stantyps
                                                then if normal {*如果是值形参*}
                                                     then f := 1 {*LOD,直接把这个变量的值取出来放到栈顶空间去*}
                                                     else f := 2 {*LDI，间接取值???*}
                                                else if normal {*变参，取这个*}
                                                     then f := 0 {*LDA，取这个数的地址*}
else f := 1;
                                                emit2(f,lev,adr)
end
                                       end;
                             typel,prozedure: error(44);
                             funktion: begin
                                         x.typ := typ;
                                         if lev <> 0
                                         then call(fsys,i)
                                         else standfct(adr)
                                       end
                           end {* case,with *}
                       end
                  else if sy in [ charcon,intcon,realcon ] {*代表是字符或者整数或者实数*}
                       then begin
                              if sy = realcon
                              then begin
x.typ := reals;
                                     enterreal(rnum);
emit1(25,c1)
                                   end
                              else begin
                                     if sy = charcon
                                     then x.typ := chars
                                     else x.typ := ints;
                                     emit1(24,inum)
                                   end;
                              x.ref := 0;
                              insymbol
                            end
                       else if sy = lparent
                            then begin
                                   insymbol;
                                   expression(fsys + [rparent],x);
                                   if sy = rparent
                                   then insymbol
                                   else error(4)
                                 end
                             else if sy = notsy
                                  then begin
                                         insymbol;
                                         factor(fsys,x);
                                         if x.typ = bools
                                         then emit(35)
                                         else if x.typ <> notyp
                                              then error(32)
                                       end;
                  test(fsys,facbegsys,6)
                end {* while *}
            end {* factor *};
          begin {* term   *}
            factor( fsys + [times,rdiv,idiv,imod,andsy],x);
            writeln('term: term', x.typ,' ',x.ref);
            while sy in [times,rdiv,idiv,imod,andsy] do {**,/,div,mod,and*}
              begin
                op := sy; 
                insymbol;
                factor(fsys+[times,rdiv,idiv,imod,andsy],y );
                if op = times
                then begin
                       x.typ := resulttype(x.typ, y.typ);
                       case x.typ of
                         notyp: ;
                         ints : emit(57);
                         reals: emit(60);
                       end
                     end
                else if op = rdiv
                     then begin
                            if x.typ = ints
                            then begin
                                   emit1(26,1);
                                   x.typ := reals;
                                 end;
                            if y.typ = ints
                            then begin
                                   emit1(26,0);
                                   y.typ := reals;
                                 end;
                            if (x.typ = reals) and (y.typ = reals)
                            then emit(61)
                            else begin
                                   if( x.typ <> notyp ) and (y.typ <> notyp)
                                   then error(33);
                                   x.typ := notyp
                                 end
                          end
                     else if op = andsy
                          then begin
                                 if( x.typ = bools )and(y.typ = bools)
                                 then emit(56)
                                 else begin
                                        if( x.typ <> notyp ) and (y.typ <> notyp)
                                        then error(32);
                                        x.typ := notyp
                                      end
                               end
                          else begin {* op in [idiv,imod] *}
                                 if (x.typ = ints) and (y.typ = ints)
                                 then if op = idiv
                                      then emit(58)
                                      else emit(59)
                                 else begin
                                        if ( x.typ <> notyp ) and (y.typ <> notyp)
                                        then error(34);
                                        x.typ := notyp
                                      end
                               end
              end {* while *}
          end {* term *};
        begin {* simpleexpression *} {*处理简单表达式*}
          if sy in [plus,minus] {*加号减号*}
          then begin
                 op := sy; {*运算符*}
                 insymbol;
                 term( fsys+[plus,minus],x); {*处理项*}
                 writeln('Simpleexpression: term', x.typ,' ',x.ref);
                 if x.typ > reals
                 then error(33)
                 else if op = minus
                      then emit(36)
               end
          else term(fsys+[plus,minus,orsy],x);
          while sy in [plus,minus,orsy] do
            begin
              op := sy;
              insymbol;
              term(fsys+[plus,minus,orsy],y);
              if op = orsy
              then begin
                     if ( x.typ = bools )and(y.typ = bools)
                     then emit(51)
                     else begin
                            if( x.typ <> notyp) and (y.typ <> notyp)
                            then error(32);
                            x.typ := notyp
                          end
                   end
              else begin
                     x.typ := resulttype(x.typ,y.typ);
                     case x.typ of
                       notyp: ;
                       ints: if op = plus
                             then emit(52)
                             else emit(53);
                       reals:if op = plus
                             then emit(54)
                             else emit(55)
                     end {* case *}
                   end
            end {* while *}
          end {* simpleexpression *};
      begin {* expression  *}
        simpleexpression(fsys+[eql,neq,lss,leq,gtr,geq],x);
        if sy in [ eql,neq,lss,leq,gtr,geq]
        then begin
               op := sy;
               insymbol;
               simpleexpression(fsys,y);
               if(x.typ in [notyp,ints,bools,chars]) and (x.typ = y.typ)
               then case op of
                      eql: emit(45);
                      neq: emit(46);
                      lss: emit(47);
                      leq: emit(48);
                      gtr: emit(49);
                      geq: emit(50);
                    end
               else begin
                      if x.typ = ints
                      then begin
                             x.typ := reals;
                             emit1(26,1)
                           end
                      else if y.typ = ints
                           then begin
                                  y.typ := reals;
                                  emit1(26,0)
                                end;
                      if ( x.typ = reals)and(y.typ=reals)
                      then case op of
                             eql: emit(39);
                             neq: emit(40);
                             lss: emit(41);
                             leq: emit(42);
                             gtr: emit(43);
                             geq: emit(44);
                           end
                      else error(35)
                    end;
               x.typ := bools
             end
      end {* expression *};

    procedure assignment( lv, ad: integer ); {*处理赋值语句的过程*}
      var x,y: item;
          f  : integer;
      begin   {* tab[i].obj in [variable,prozedure] *}
        x.typ := tab[i].typ; {*把x的值用tab[i]的类型*}
        x.ref := tab[i].ref;
        if tab[i].normal
        then f := 0 {LDA指令，将变量地址填入栈顶，因为不是变参???}
        else f := 1; {LOD指令，将值填入栈顶，代表是变参}
        emit2(f,lv,ad);
        if sy in [lbrack,lparent,period]
        then selector([becomes,eql]+fsys,x);
        if sy = becomes
        then insymbol
        else begin
               error(51);
               if sy = eql
               then insymbol
             end;
        expression(fsys,y);
        if x.typ = y.typ
        then if x.typ in stantyps
             then emit(38)
             else if x.ref <> y.ref
                  then error(46)
                  else if x.typ = arrays
                       then emit1(23,atab[x.ref].size)
                       else emit1(23,btab[x.ref].vsize)
        else if(x.typ = reals )and (y.typ = ints)
        then begin
               emit1(26,0);
               emit(38)
             end
        else if ( x.typ <> notyp ) and ( y.typ <> notyp )
             then error(46)
      end {* assignment *};

    procedure compoundstatement;
      begin
        insymbol;
        statement([semicolon,endsy]+fsys);
        while sy in [semicolon]+statbegsys do
          begin
            if sy = semicolon
            then insymbol
            else error(14);
            statement([semicolon,endsy]+fsys)
          end;
        if sy = endsy
        then insymbol
        else error(57)
      end {* compoundstatement *};

    procedure ifstatement; {*处理if表达式*} 
    var x : item;
        lc1,lc2: integer;
    begin
        insymbol;
        expression( fsys+[thensy,dosy],x);
        if not ( x.typ in [bools,notyp]) {*if后面跟的必须是布尔类型或者是函数类型的变量*}
        then error(17);
        lc1 := lc; {*???*}
        emit(11);  {* jmpc *}
        if sy = thensy {*如果后面紧接的是then*}
        then insymbol
        else begin
               error(52);
               if sy = dosy
               then insymbol
             end;
        statement( fsys+[elsesy]);
        if sy = elsesy
        then begin
               insymbol;
               lc2 := lc;
               emit(10);
               code[lc1].y := lc;
               statement(fsys);
               code[lc2].y := lc
             end
        else code[lc1].y := lc
end {* ifstatement *};

    procedure casestatement; {*处理case语句*}
      var x : item;
          i,j,k,lc1 : integer;
          casetab : array[1..csmax]of {*各个标号对应的地址*}
                     packed record
                       val,lc : index
                     end;
        exittab : array[1..csmax] of integer;

procedure caselabel; {*各种标号，将标号对应的目标代码入口地址填入casetab表*}
        var lab : conrec;
            k : integer;
        begin
          constant( fsys+[comma,colon],lab ); {*标签用常量表示*}
          if lab.tp <> x.typ {*标签的类型和case 后的x的类型不同*}
          then error(47)
          else if i = csmax {*申明的case不能超过csmax*}
               then fatal(6)
               else begin
                      i := i+1; {*case表里的数量增加*}
                      k := 0; 
                      casetab[i].val := lab.i; {*保存到casetab里是第几个*}
                      casetab[i].lc := lc; {*更新出现的位置*}
                      repeat {*从头到尾比一下，看有没有重复的*}
                        k := k+1
                      until casetab[k].val = lab.i;
                      if k < i {*表明出现了重复的*}
                      then error(1); {* multiple definition *}
                    end
        end {* caselabel *};

      procedure onecase; {*处理case的一个分支*}
        begin
          if sy in constbegsys
          then begin
                 caselabel; {*求标签和位置*}
                 while sy = comma do {*有逗号代表多个标号对应case*}
                   begin
                     insymbol;
                     caselabel {*继续求标签和位置*}
                   end;
                 if sy = colon {*冒号代表，标号读完了*}
                 then insymbol
                 else error(5);
                 statement([semicolon,endsy]+fsys); {*递归调用冒号后面的部分*}
                 j := j+1; {*当前有多少个case*}
                 exittab[j] := lc; {*分支结束的代码位置*}
                 emit(10){*打印一条Pcode，结束分支*}
               end
          end {* onecase *};
      begin  {* casestatement  *}
        insymbol;
        i := 0;
        j := 0;
        expression( fsys + [ofsy,comma,colon],x ); {*求case后的变量表达式*}
        if not( x.typ in [ints,bools,chars,notyp ]) {*case后的表达式必须是这几个类型之一*}
            then error(23);
        lc1 := lc; {*记录当前Pcode指针*}
        emit(12); {*jmpx} {生成SWT代码*}
        if sy = ofsy {*case后的of*}
        then insymbol
        else error(8);
        onecase; {*调用onecase处理后面的case分支*}
        while sy = semicolon do
          begin
            insymbol;
            onecase {*不停地调用case处理分支*}
          end;
        code[lc1].y := lc; {*case的起始地址是lc???*}
        for k := 1 to i do {*写所有的case分支*}
          begin
            emit1( 13,casetab[k].val); {*每个对应一个匹配的值*}
            emit1( 13,casetab[k].lc); {*每个对应一个跳转地址*}
          end;
        emit1(10,0); {*生成JMP指令，无条件跳转*}
        for k := 1 to j do {*给每一个分支结束之后跳转到哪里*}
            code[exittab[k]].y := lc; {*每一个分支都跳到这里*}
        if sy = endsy {*遇到end，case*}
        then insymbol
        else error(57)
      end {* casestatement *};

    procedure repeatstatement; {*处理repeat*}
      var x : item;
          lc1: integer;
      begin
        lc1 := lc; {*当前代码位置*}
        insymbol;
        statement( [semicolon,untilsy]+fsys); {*处理语句*}
        while sy in [semicolon]+statbegsys do
          begin
            if sy = semicolon
            then insymbol
            else error(14);
            statement([semicolon,untilsy]+fsys) {*递归处理语句*}
          end;
        if sy = untilsy {*遇到*}
        then begin
               insymbol;
               expression(fsys,x);
               if not(x.typ in [bools,notyp] )
               then error(17);
               emit1(11,lc1);
             end
        else error(53)
      end {* repeatstatement *};

    procedure whilestatement; {处理while循环}
      var x : item;
          lc1,lc2 : integer;
      begin
        insymbol;
        lc1 := lc; {存储当前的位置}
        expression( fsys+[dosy],x); {处理表达式}
        if not( x.typ in [bools, notyp] ) {必须是布尔类型或者无类型的}
        then error(17);
        lc2 := lc; {标记到做完while后面的判断到哪}
        emit(11);
        if sy = dosy {遇到了do}
        then insymbol
        else error(54);
        statement(fsys);
        emit1(10,lc1); {打印while语句,JMP}
        code[lc2].y := lc {跳转到调用while的地方}
      end {* whilestatement *};

    procedure forstatement; {*处理for循环语句*}
      var  cvt : types; {*循环变量的类型*}
           x :  item;
           i,f,lc1,lc2 : integer;
      begin
        insymbol;
        if sy = ident
        then begin
               i := loc(id); {*找到循环变量在符号表里的位置*}
               insymbol;
               if i = 0 {*没有找到循环*}
               then cvt := ints
               else if tab[i].obj = vvariable {*种类是变量*}
                    then begin
                           cvt := tab[i].typ; {*获取类型*}
                           if not tab[i].normal {*只有变参的normal位false，这里这么写就是说循环变量不能是变参*}
                           then error(37)
                           else emit2(0,tab[i].lev, tab[i].adr ); {*打印到Pcode里*}
                           if not ( cvt in [notyp, ints, bools, chars])
                           then error(18)
                         end
                    else begin {*不是变量类型*}
                           error(37); {*报错*}
                           cvt := ints {*容错，将计数变量设置成整形*}
                         end
             end
        else skip([becomes,tosy,downtosy,dosy]+fsys,2); {*跳过这个符号*}
        if sy = becomes {*识别到赋值符号*}
        then begin
               insymbol;
               expression( [tosy, downtosy,dosy]+fsys,x); {*递归调用表达式*}
               if x.typ <> cvt {*那个循环的变量返回的第一个数和循环变量的类型不一致，报错*}
               then error(19);
             end
        else skip([tosy, downtosy,dosy]+fsys,51); {*未识别赋值符号，继续执行*}
        f := 14; {*for语句的指令码是14*}
        if sy in [tosy,downtosy] {*to代表for循环从小到大变，downto代表从大到小变*}
        then begin
               if sy = downtosy {*downto的指令码是16*}
               then f := 16;
               insymbol;
               expression([dosy]+fsys,x); {*递归处理for循环的后面一个数，保存到x里*}
               if x.typ <> cvt
               then error(19)
             end
        else skip([dosy]+fsys,55); {*一直跳到do*}
        lc1 := lc; {*F1U指令的位置*}
        emit(f); {*打印F1U/F1D指令*}
        if sy = dosy {*如果标识符是do*}
        then insymbol
        else error(54);
        lc2 := lc; {*F2U开始的指令*}
        statement(fsys); {*递归调用来处理循环体的语句*}
        emit1(f+1,lc2); {*生成F2U/F2D指令*}
        code[lc1].y := lc {*调用F1U的指令标记成之前的之前的指令位置*}
end {* forstatement *};

    procedure standproc( n: integer ); {*处理pascal的标准函数，例如read,readln,wrtite,writeln*}
      var i,f : integer;
          x,y : item;
      begin
        case n of 
          1,2 : begin {* read *}
                  if not iflag
                  then begin
                         error(20);
                         iflag := true
                       end;
                  if sy = lparent
                  then begin
                         repeat
                           insymbol;
                           if sy <> ident
                           then error(2)
                           else begin
                                  i := loc(id);
                                  insymbol;
                                  if i <> 0
                                  then if tab[i].obj <> vvariable
                                       then error(37)
                                       else begin
                                              x.typ := tab[i].typ;
                                              x.ref := tab[i].ref;
                                              if tab[i].normal
                                              then f := 0
                                              else f := 1;
                                              emit2(f,tab[i].lev,tab[i].adr);
                                              if sy in [lbrack,lparent,period]
                                              then selector( fsys+[comma,rparent],x);
                                              if x.typ in [ints,reals,chars,notyp]
                                              then emit1(27,ord(x.typ))
                                              else error(41)
                                            end
                                end;
                           test([comma,rparent],fsys,6);
                         until sy <> comma;
                         if sy = rparent
                         then insymbol
                         else error(4)
                       end;
                  if n = 2
                  then emit(62)
                end;
          3,4 : begin {* write *}
                  if sy = lparent
                  then begin
                         repeat
                           insymbol;
                           if sy = stringcon
                           then begin
                                  emit1(24,sleng);
                                  emit1(28,inum);
                                  insymbol
                                end
                           else begin
                                  expression(fsys+[comma,colon,rparent],x);
                                  if not( x.typ in stantyps )
                                  then error(41);
                                  if sy = colon
                                  then begin
                                         insymbol;
                                         expression( fsys+[comma,colon,rparent],y);
                                         if y.typ <> ints
                                         then error(43);
                                         if sy = colon
                                         then begin
                                                if x.typ <> reals
                                                then error(42);
                                                insymbol;
                                                expression(fsys+[comma,rparent],y);
                                                if y.typ <> ints
                                                then error(43);
                                                emit(37)
                                              end
                                         else emit1(30,ord(x.typ))
                                       end
                                  else emit1(29,ord(x.typ))
                                end
                         until sy <> comma;
                         if sy = rparent
                         then insymbol
                         else error(4)
                       end;
                  if n = 4
                  then emit(63) {*代表是writeln       *}
                end; {* write *}
        end {* case *};
      end {* standproc *} ;
    begin {* statement *}
      if sy in statbegsys+[ident]
      then case sy of
             ident : begin {*如果是标识符*}
                       i := loc(id); {*寻找id在符号表的位置*}
                       insymbol;
                       if i <> 0
                       then case tab[i].obj of {*判断i的种类*}
                              konstant,typel : error(45); {*如果是常量或者type，报错*}
                              vvariable:       assignment( tab[i].lev,tab[i].adr); {*如果是变量，就调用*}
                              prozedure:       if tab[i].lev <> 0
                                               then call(fsys,i)
                                               else standproc(tab[i].adr);
                              funktion:        if tab[i].ref = display[level] {*如果这个函数名指向的指针和当前层的指针相同，即对应了同一个btab里的分程序索引，则说明就是函数里的函数名(在Pascal的语法里也就是返回值)，此时我们需要调用赋值语句*}
                                               then assignment(tab[i].lev+1,0)
                                               else error(45)
                            end {* case *}
                     end;
             beginsy : compoundstatement;
             ifsy    : ifstatement;
             casesy  : casestatement;
             whilesy : whilestatement;
             repeatsy: repeatstatement;
             forsy   : forstatement;
           end;  {* case *}
      test( fsys, [],14);
    end {* statement *};
  begin  {* block *}
    dx := 5; {*变量存储分配的索引，预设为5，留出空间给内务区*}
    prt := t; {*保存的通常是函数名或者过程名在符号表里的位置*}
    if level > lmax {*子程序的层次不允许大于5，即不能嵌套太多procedure*}
        then fatal(5);
    test([lparent,colon,semicolon],fsys,14); {*判断sy是不是左括号或者分号或者句号*}
    enterblock; {*增加一个块*}
    prb := b; {*保存一下之前的块是哪一个*}
    display[level] := b; {*保存当前level的前一个(即调用它的在哪)*}
    tab[prt].typ := notyp; {*block都在符号表里填的是notyp，代表无类型的变量，其实就是procedure/function*}
    tab[prt].ref := prb; {*函数指向之前的块的位置*}
    if ( sy = lparent ) and ( level > 1 )
        then parameterlist;
    btab[prb].lastpar := t;
    btab[prb].psize := dx;
    if isfun {如果是函数，我们还需要把函数名这个tab保存一个类型，因为在Pascal里面，函数是有返回类型的}
    then if sy = colon
         then begin
                insymbol; {* function type *}
                if sy = ident
                then begin
                       x := loc(id);
                       insymbol;
                       if x <> 0
                       then if tab[x].typ in stantyps
                            then tab[prt].typ := tab[x].typ
                            else error(15)
                     end
                else skip( [semicolon]+fsys,2 )
              end
         else error(5);
    if sy = semicolon
    then insymbol
    else error(14);
    repeat
      if sy = constsy 
      then constdec; {*处理const*}
      if sy = typesy
      then typedeclaration; {*处理type*}
      if sy = varsy 
      then variabledeclaration; {*处理var*}
      btab[prb].vsize := dx;
      while sy in [procsy,funcsy] do
        procdeclaration; {*处理过程的声明*}
      test([beginsy],blockbegsys+statbegsys,56)
    until sy in statbegsys; {*直到遇到begin或其它条件判断的选项*}
    tab[prt].adr := lc; {*这句话代表的是这一函数或者过程(即block)的起始地址是哪一句Pcode,例如tab[28]=11就是第一个block也就是主程序的入口，即代表主程序从第11行Pcode开始*}
    insymbol;
    statement([semicolon,endsy]+fsys);
    while sy in [semicolon]+statbegsys do
      begin
        if sy = semicolon
        then insymbol
        else error(14);
        statement([semicolon,endsy]+fsys);
      end;
    if sy = endsy
    then insymbol
    else error(57);
    test( fsys+[period],[],6 )
  end {* block *};



procedure interpret;
  var ir : order ;         {* instruction buffer *}
      pc : integer;        {* program counter *}
      t  : integer;        {* top stack index *}
      b  : integer;        {* base index *}
      h1,h2,h3: integer;
      lncnt,ocnt,blkcnt,chrcnt: integer;     {* counters *}
      ps : ( run,fin,caschk,divchk,inxchk,stkchk,linchk,lngchk,redchk );
      fld: array [1..4] of integer;  {* default field widths *}
      display : array[0..lmax] of integer;
      s  : array[1..stacksize] of   {* blockmark:     *}
            record
              case cn : types of        {* s[b+0] = fct result *}
                ints : (i: integer );   {* s[b+1] = return adr *}
                reals :(r: real );      {* s[b+2] = static link *}
                bools :(b: boolean );   {* s[b+3] = dynamic link *}
                chars :(c: char )       {* s[b+4] = table index *}
end;

  procedure dump;
    var p,h3 : integer;
begin
      h3 := tab[h2].lev;
      writeln(psout);
      writeln(psout);
      writeln(psout,'       calling ', tab[h2].name );
      writeln(psout,'         level ',h3:4);
      writeln(psout,' start of code ',pc:4);
      writeln(psout);
      writeln(psout);
      writeln(psout,' contents of display ');
      writeln(psout);
      for p := h3 downto 0 do
        writeln(psout,p:4,display[p]:6);
      writeln(psout);
      writeln(psout);
      writeln(psout,' top of stack  ',t:4,' frame base ':14,b:4);
      writeln(psout);
      writeln(psout);
      writeln(psout,' stack contents ':20);
      writeln(psout);
      for p := t downto 1 do
        writeln( psout, p:14, s[p].i:8);
      writeln(psout,'< = = = >':22)
    end; {*dump *}

  procedure inter0;
    begin
      case ir.f of
        0 : begin {* load addrss *} {*LDA指令，取地址*}
              t := t + 1; {*上移一个栈顶指针*}
              if t > stacksize
              then ps := stkchk
              else s[t].i := display[ir.x]+ir.y {*当前level的起始地址加上偏移量，得到*}
            end;
        1 : begin  {* load value *} {*LOD*}
              t := t + 1;
              if t > stacksize
              then ps := stkchk
              else s[t] := s[display[ir.x]+ir.y] {*从这个地址中取出来值放到栈顶*}
            end;
        2 : begin  {* load indirect *}
              t := t + 1;
              if t > stacksize
              then ps := stkchk
              else s[t] := s[s[display[ir.x]+ir.y].i]
            end;
        3 : begin  {* update display *}
              h1 := ir.y;
              h2 := ir.x;
              h3 := b;
              repeat
                display[h1] := h3;
                h1 := h1-1;
                h3 := s[h3+2].i
              until h1 = h2
            end;
        8 : case ir.y of
              0 : s[t].i := abs(s[t].i);
              1 : s[t].r := abs(s[t].r);
              2 : s[t].i := sqr(s[t].i);
              3 : s[t].r := sqr(s[t].r);
              4 : s[t].b := odd(s[t].i);
              5 : s[t].c := chr(s[t].i);
              6 : s[t].i := ord(s[t].c);
              7 : s[t].c := succ(s[t].c);
              8 : s[t].c := pred(s[t].c);
              9 : s[t].i := round(s[t].r);
              10 : s[t].i := trunc(s[t].r);
              11 : s[t].r := sin(s[t].r);
              12 : s[t].r := cos(s[t].r);
              13 : s[t].r := exp(s[t].r);
              14 : s[t].r := ln(s[t].r);
              15 : s[t].r := sqrt(s[t].r);
              16 : s[t].r := arcTan(s[t].r);
              17 : begin
                     t := t+1;
                     if t > stacksize
                     then ps := stkchk
                     else s[t].b := eof(prd)
                   end;
              18 : begin
                     t := t+1;
                     if t > stacksize
                     then ps := stkchk
                     else s[t].b := eoln(prd)
                   end;
            end;
        9 : s[t].i := s[t].i + ir.y; {* offset *}
      end {* case ir.y *}
    end; {* inter0 *}

procedure inter1; {*执行的过程1*}
    var h3, h4: integer;
begin
      case ir.f of
        10 : pc := ir.y ; {* jump *} {跳转指令}
        11 : begin  {* conditional jump *}
               if not s[t].b
                then pc := ir.y;
               t := t - 1
        end;
        12 : begin {* switch *} 
               h1 := s[t].i; 
               t := t-1;
               h2 := ir.y;
               h3 := 0;
               repeat
                 if code[h2].f <> 13
                 then begin
                        h3 := 1;
                        ps := caschk
                      end
                 else if code[h2].y = h1
                      then begin
                             h3 := 1;
                             pc := code[h2+1].y
                           end
                      else h2 := h2 + 2
               until h3 <> 0
             end;
        14 : begin {* for1up *}
               h1 := s[t-1].i;
               if h1 <= s[t].i
               then s[s[t-2].i].i := h1
               else begin
                      t := t - 3;
                      pc := ir.y
                    end
             end;
        15 : begin {* for2up *}
               h2 := s[t-2].i;
               h1 := s[h2].i+1;
               if h1 <= s[t].i
               then begin
                      s[h2].i := h1;
                      pc := ir.y
                    end
               else t := t-3;
             end;
        16 : begin  {* for1down *}
               h1 := s[t-1].i;
               if h1 >= s[t].i
               then s[s[t-2].i].i := h1
               else begin
                      pc := ir.y;
                      t := t - 3
                    end
             end;
        17 : begin  {* for2down *}
               h2 := s[t-2].i;
               h1 := s[h2].i-1;
               if h1 >= s[t].i
               then begin
                      s[h2].i := h1;
                      pc := ir.y
                    end
               else t := t-3;
             end;
        18 : begin  {* mark stack *}
               h1 := btab[tab[ir.y].ref].vsize;
               if t+h1 > stacksize
               then ps := stkchk
               else begin
                      t := t+5;
                      s[t-1].i := h1-1;
                      s[t].i := ir.y
                    end
             end;
        19 : begin  {* call *}
               h1 := t-ir.y;  {* h1 points to base *}
               h2 := s[h1+4].i;  {* h2 points to tab *}
               h3 := tab[h2].lev;
               display[h3+1] := h1;
               h4 := s[h1+3].i+h1;
               s[h1+1].i := pc;
               s[h1+2].i := display[h3];
               s[h1+3].i := b;
               for h3 := t+1 to h4 do
                 s[h3].i := 0;
               b := h1;
               t := h4;
               pc := tab[h2].adr;
               if stackdump
               then dump
             end;
      end {* case *}
    end; {* inter1 *}

  procedure inter2;
    begin
      case ir.f of
        20 : begin   {* index1 *}
               h1 := ir.y;  {* h1 points to atab *}
               h2 := atab[h1].low;
               h3 := s[t].i;
               if h3 < h2
               then ps := inxchk
               else if h3 > atab[h1].high
                    then ps := inxchk
                    else begin
                           t := t-1;
                           s[t].i := s[t].i+(h3-h2)
                         end
             end;
        21 : begin  {* index *}
               h1 := ir.y ; {* h1 points to atab *}
               h2 := atab[h1].low;
               h3 := s[t].i;
               if h3 < h2
               then ps := inxchk
               else if h3 > atab[h1].high
                    then ps := inxchk
                    else begin
                           t := t-1;
                           s[t].i := s[t].i + (h3-h2)*atab[h1].elsize
                         end
             end;
        22 : begin  {* load block *}
               h1 := s[t].i;
               t := t-1;
               h2 := ir.y+t;
               if h2 > stacksize
               then ps := stkchk
               else while t < h2 do
                      begin
                        t := t+1;
                        s[t] := s[h1];
                        h1 := h1+1
                      end
             end;
        23 : begin  {* copy block *}
               h1 := s[t-1].i;
               h2 := s[t].i;
               h3 := h1+ir.y;
               while h1 < h3 do
                 begin
                   s[h1] := s[h2];
                   h1 := h1+1;
                   h2 := h2+1
                 end;
               t := t-2
             end;
        24 : begin  {* literal *} {*LDC，装进去常量*}
               t := t+1;
               if t > stacksize
               then ps := stkchk
               else s[t].i := ir.y {把y放到栈顶空间去*}
             end;
        25 : begin  {* load real *}
               t := t+1;
               if t > stacksize
               then ps := stkchk
               else s[t].r := rconst[ir.y]
             end;
        26 : begin  {* float *}
               h1 := t-ir.y;
               s[h1].r := s[h1].i
             end;
        27 : begin  {* read *}
               if eof(prd)
               then ps := redchk
               else case ir.y of
                      1 : read(prd, s[s[t].i].i);
                      2 : read(prd, s[s[t].i].r);
                      4 : read(prd, s[s[t].i].c);
                    end;
               t := t-1
             end;
        28 : begin   {* write string *}
h1 := s[t].i;
               h2 := ir.y;
               t := t-1;
               chrcnt := chrcnt+h1;
if chrcnt > lineleng
               then ps := lngchk;
               repeat
                 write(prr,stab[h2]);
                 h1 := h1-1;
                 h2 := h2+1
               until h1 = 0
             end;
        29 : begin  {* write1 *}
               chrcnt := chrcnt + fld[ir.y];
               if chrcnt > lineleng
               then ps := lngchk
               else case ir.y of
                      1 : write(prr,s[t].i:fld[1]);
                      2 : write(prr,s[t].r:fld[2]);
                      3 : if s[t].b
                          then write('true')
                          else write('false');
                      4 : write(prr,chr(s[t].i));
                    end;
               t := t-1
             end;
      end {* case *}
    end; {* inter2 *}

  procedure inter3;
    begin
      case ir.f of
        30 : begin {* write2 *}
               chrcnt := chrcnt+s[t].i;
               if chrcnt > lineleng
               then ps := lngchk
               else case ir.y of
                      1 : write(prr,s[t-1].i:s[t].i);
                      2 : write(prr,s[t-1].r:s[t].i);
                      3 : if s[t-1].b
                          then write('true')
                          else write('false');
                    end;
               t := t-2
             end;
        31 : ps := fin; {*HLT，程序结束*}
        32 : begin  {* exit procedure *}
               t := b-1;
               pc := s[b+1].i;
               b := s[b+3].i
             end;
        33 : begin  {* exit function *}
               t := b;
               pc := s[b+1].i;
               b := s[b+3].i
             end;
        34 : s[t] := s[s[t].i];
        35 : s[t].b := not s[t].b;
        36 : s[t].i := -s[t].i;
        37 : begin
               chrcnt := chrcnt + s[t-1].i;
               if chrcnt > lineleng
               then ps := lngchk
               else write(prr,s[t-2].r:s[t-1].i:s[t].i);
               t := t-3
             end;
        38 : begin  {* store *} 
               s[s[t-1].i] := s[t]; {*把栈顶内容复制到次元素*}
               t := t-2 {*同时弹出这两个元素*}
             end;
        39 : begin
               t := t-1;
               s[t].b := s[t].r=s[t+1].r
             end;
      end {* case *}
    end; {* inter3 *}

  procedure inter4;
    begin
      case ir.f of
        40 : begin
               t := t-1;
               s[t].b := s[t].r <> s[t+1].r
             end;
        41 : begin
               t := t-1;
               s[t].b := s[t].r < s[t+1].r
             end;
        42 : begin
               t := t-1;
               s[t].b := s[t].r <= s[t+1].r
             end;
        43 : begin
               t := t-1;
               s[t].b := s[t].r > s[t+1].r
             end;
        44 : begin
               t := t-1;
               s[t].b := s[t].r >= s[t+1].r
             end;
        45 : begin
               t := t-1;
               s[t].b := s[t].i = s[t+1].i
             end;
        46 : begin
               t := t-1;
               s[t].b := s[t].i <> s[t+1].i
             end;
        47 : begin
               t := t-1;
               s[t].b := s[t].i < s[t+1].i
             end;
        48 : begin
               t := t-1;
               s[t].b := s[t].i <= s[t+1].i
             end;
        49 : begin
               t := t-1;
               s[t].b := s[t].i > s[t+1].i
             end;
      end {* case *}
    end; {* inter4 *}

  procedure inter5;
    begin
      case ir.f of
        50 : begin
               t := t-1;
               s[t].b := s[t].i >= s[t+1].i
             end;
        51 : begin
               t := t-1;
               s[t].b := s[t].b or s[t+1].b
             end;
        52 : begin
               t := t-1;
               s[t].i := s[t].i+s[t+1].i
             end;
        53 : begin
               t := t-1;
               s[t].i := s[t].i-s[t+1].i
             end;
        54 : begin
               t := t-1;
               s[t].r := s[t].r+s[t+1].r;
             end;
        55 : begin
               t := t-1;
               s[t].r := s[t].r-s[t+1].r;
             end;
        56 : begin
               t := t-1;
               s[t].b := s[t].b and s[t+1].b
             end;
        57 : begin
               t := t-1;
               s[t].i := s[t].i*s[t+1].i
             end;
        58 : begin
               t := t-1;
               if s[t+1].i = 0
               then ps := divchk
               else s[t].i := s[t].i div s[t+1].i
             end;
        59 : begin
               t := t-1;
               if s[t+1].i = 0
               then ps := divchk
               else s[t].i := s[t].i mod s[t+1].i
             end;
      end {* case *}
    end; {* inter5 *}

  procedure inter6;
    begin
      case ir.f of
        60 : begin
               t := t-1;
               s[t].r := s[t].r*s[t+1].r;
             end;
        61 : begin
               t := t-1;
               s[t].r := s[t].r/s[t+1].r;
             end;
        62 : if eof(prd)
             then ps := redchk
             else readln;
        63 : begin
               writeln(prr);
               lncnt := lncnt+1;
               chrcnt := 0;
               if lncnt > linelimit
               then ps := linchk
             end
      end {* case *};
    end; {* inter6 *}
  begin {* interpret *}
    s[1].i := 0;
    s[2].i := 0;
    s[3].i := -1;
    s[4].i := btab[1].last;
    display[0] := 0;
    display[1] := 0;
    t := btab[2].vsize-1;
    b := 0;
    pc := tab[s[4].i].adr;
    lncnt := 0;
    ocnt := 0;
    chrcnt := 0;
    ps := run;
    fld[1] := 10;
    fld[2] := 22;
    fld[3] := 10;
    fld[4] := 1;
    repeat {*反复执行，直到指令的状态ps不为run，因为最终的终止指令会让PS=FIN*}
      ir := code[pc];
      pc := pc+1;
      ocnt := ocnt+1;
      case ir.f div 10 of
        0 : inter0;
        1 : inter1;
        2 : inter2;
        3 : inter3;
        4 : inter4;
        5 : inter5;
        6 : inter6;
      end; {* case *}
    until ps <> run;

    if ps <> fin
    then begin
           writeln(prr);
           write(prr, ' halt at', pc :5, ' because of ');
           case ps of
             caschk  : writeln(prr,'undefined case');
             divchk  : writeln(prr,'division by 0');
             inxchk  : writeln(prr,'invalid index');
             stkchk  : writeln(prr,'storage overflow');
             linchk  : writeln(prr,'too much output');
             lngchk  : writeln(prr,'line too long');
             redchk  : writeln(prr,'reading past end or file');
           end;
           h1 := b;
           blkcnt := 10;    {* post mortem dump *}
           repeat
             writeln( prr );
             blkcnt := blkcnt-1;
             if blkcnt = 0
             then h1 := 0;
             h2 := s[h1+4].i;
             if h1 <> 0
             then writeln( prr, '',tab[h2].name, 'called at', s[h1+1].i:5);
             h2 := btab[tab[h2].ref].last;
             while h2 <> 0 do
               with tab[h2] do
                 begin
                   if obj = vvariable
                   then if typ in stantyps
                        then begin
                               write(prr,'',name,'=');
                               if normal
                               then h3 := h1+adr
                               else h3 := s[h1+adr].i;
                               case typ of
                                 ints : writeln(prr,s[h3].i);
                                 reals: writeln(prr,s[h3].r);
                                 bools: if s[h3].b
                                        then writeln(prr,'true')
                                        else writeln(prr,'false');
                                 chars: writeln(prr,chr(s[h3].i mod 64 ))
                               end
                             end;
                   h2 := link
                 end;
             h1 := s[h1+3].i
           until h1 < 0
         end;
    writeln(prr);
    writeln(prr,ocnt,' steps');
  end; {* interpret *}



procedure setup;
  begin
    key[1] := 'and       ';
    key[2] := 'array     ';
    key[3] := 'begin     ';
    key[4] := 'case      ';
    key[5] := 'const     ';
    key[6] := 'div       ';
    key[7] := 'do        ';
    key[8] := 'downto    ';
    key[9] := 'else      ';
    key[10] := 'end       ';
    key[11] := 'for       ';
    key[12] := 'function  ';
    key[13] := 'if        ';
    key[14] := 'mod       ';
    key[15] := 'not       ';
    key[16] := 'of        ';
    key[17] := 'or        ';
    key[18] := 'procedure ';
    key[19] := 'program   ';
    key[20] := 'record    ';
    key[21] := 'repeat    ';
    key[22] := 'then      ';
    key[23] := 'to        ';
    key[24] := 'type      ';
    key[25] := 'until     ';
    key[26] := 'var       ';
    key[27] := 'while     ';

    ksy[1] := andsy;
    ksy[2] := arraysy;
    ksy[3] := beginsy;
    ksy[4] := casesy;
    ksy[5] := constsy;
    ksy[6] := idiv;
    ksy[7] := dosy;
    ksy[8] := downtosy;
    ksy[9] := elsesy;
    ksy[10] := endsy;
    ksy[11] := forsy;
    ksy[12] := funcsy;
    ksy[13] := ifsy;
    ksy[14] := imod;
    ksy[15] := notsy;
    ksy[16] := ofsy;
    ksy[17] := orsy;
    ksy[18] := procsy;
    ksy[19] := programsy;
    ksy[20] := recordsy;
    ksy[21] := repeatsy;
    ksy[22] := thensy;
    ksy[23] := tosy;
    ksy[24] := typesy;
    ksy[25] := untilsy;
    ksy[26] := varsy;
    ksy[27] := whilesy;


    sps['+'] := plus;
    sps['-'] := minus;
    sps['*'] := times;
    sps['/'] := rdiv;
    sps['('] := lparent;
    sps[')'] := rparent;
    sps['='] := eql;
    sps[','] := comma;
    sps['['] := lbrack;
    sps[']'] := rbrack;
    sps[''''] := neq;
    sps['!'] := andsy;
    sps[';'] := semicolon;
  end {* setup *};

procedure enterids; {*把全部的的标准类型都先填入到tab里，adr对于类型名，实际填的是该类型数据所需存储单元的数目*}
  begin
    {*第一个参数代表name即名字，第二个参数代表obj即种类，第三个参数代表类型，第四个参数代表adr，对于类型其代表的是所需要存储单元的数目*}
    enter('          ',vvariable,notyp,0); {* sentinel *}
    enter('false     ',konstant,bools,0); 
    enter('true      ',konstant,bools,1);
    enter('real      ',typel,reals,1);
    enter('char      ',typel,chars,1);
    enter('boolean   ',typel,bools,1);
    enter('integer   ',typel,ints,1);
    enter('abs       ',funktion,reals,0);
    enter('sqr       ',funktion,reals,2);
    enter('odd       ',funktion,bools,4); {???为啥odd需要4个存储单元，接下来递增的同理}
    enter('chr       ',funktion,chars,5);
    enter('ord       ',funktion,ints,6);
    enter('succ      ',funktion,chars,7);
    enter('pred      ',funktion,chars,8);
    enter('round     ',funktion,ints,9);
    enter('trunc     ',funktion,ints,10);
    enter('sin       ',funktion,reals,11);
    enter('cos       ',funktion,reals,12);
    enter('exp       ',funktion,reals,13);
    enter('ln        ',funktion,reals,14);
    enter('sqrt      ',funktion,reals,15);
    enter('arctan    ',funktion,reals,16);
    enter('eof       ',funktion,bools,17);
    enter('eoln      ',funktion,bools,18);
    enter('read      ',prozedure,notyp,1);
    enter('readln    ',prozedure,notyp,2);
    enter('write     ',prozedure,notyp,3);
    enter('writeln   ',prozedure,notyp,4);
    enter('          ',prozedure,notyp,0); {*重要！就是把主函数放进符号表里，因为我们知道函数和过程是也要放进符号表的，这里面先把主过程放进符号表里，因为没有人调用主过程，所以我们先放进去便于之后的处理*}
  end;


begin  {* main *}      
setup;
  constbegsys := [ plus, minus, intcon, realcon, charcon, ident ];
  typebegsys := [ ident, arraysy, recordsy ];
  blockbegsys := [ constsy, typesy, varsy, procsy, funcsy, beginsy ]; {*都是一个块语句的起始的标记*}
  facbegsys := [ intcon, realcon, charcon, ident, lparent, notsy ];
  statbegsys := [ beginsy, ifsy, whilesy, repeatsy, forsy, casesy ]; {*条件判断的块的起始字符*}
  stantyps := [ notyp, ints, reals, bools, chars ];
  lc := 0;
  ll := 0;
  cc := 0;
  ch := ' ';
  errpos := 0;
  errs := [];
  writeln( 'NOTE input/output for users program is console : ' );
  writeln;
  write( 'Source input file ?');
  readln( inf );
  assign( psin, inf );
  reset( psin );
  write( 'Source listing file ?');
  readln( outf );
  assign( psout, outf );
  rewrite( psout );
  assign ( prd, 'con' );
  write( 'result file : ' );
  readln( fprr );
  assign( prr, fprr );
  {reset ( prd );}
  rewrite( prr );

  t := -1;
  a := 0;
  b := 1;
  sx := 0;
  c2 := 0;
  display[0] := 1;
  iflag := false;
  oflag := false;
  skipflag := false;
  prtables := false;
  stackdump := false;

  insymbol;

  if sy <> programsy 
  then error(3) {*如果刚刚读入到的不是程序名就报错*}
  else begin
         insymbol; 
         if sy <> ident 
         then error(2)
         else begin
                progname := id; {*把刚刚求得的标识符的名字赋值给progname，代表程序名*}
                insymbol; {*读入下一个字符*}
                if sy <> lparent {*！！！规定必须是program name (input, output); 这样的格式*}
                then error(9)
                else repeat 
                       insymbol;
                       if sy <> ident
                       then error(2)
                       else begin
                              if id = 'input     ' {*如果是input把iflag置为true*}
                              then iflag := true
                              else if id = 'output    ' {*如果是output把oflag置为true*}
                                   then oflag := true
                                   else error(0);
                              insymbol
                            end
                     until sy <> comma; {*反复读入，直到不是逗号，即)，因为是(input, outout)相当于最后那个)*}
                if sy = rparent 
                then insymbol
                else error(4);
                if not oflag then error(20)
              end
       end;
  enterids;
  with btab[1] do {*记录主函数的情况，首先这个last我们可以考虑成是整个文件的最后一个出现的标识符，它就是主过程*}
    begin
      last := t;
      lastpar := 1;
      psize := 0;
      vsize := 0;
    end;
  block( blockbegsys + statbegsys, false, 1 );{*进入主模块处理的过程 即主程序的begin end处理*}

  if sy <> period
  then error(2);
  emit(31);  {* halt *}
  if prtables
  then printtables;
  if errs = []
  then interpret
  else begin
         writeln( psout );
         writeln( psout, 'compiled with errors' );
         writeln( psout );
         errormsg;
       end;
  writeln( psout );
  close( psout );
  close( prr )
end.                                              

