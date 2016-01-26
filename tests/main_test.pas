///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TestResult(Cond, Text);
begin
	if not Cond then writeln(Text+' ********** FAIL **********');
end;

function TrueFunc();
begin
	result := true;
end;

function FalseFunc();
begin
	result := false;
end;

function Func_1();
begin
	result := 1;
end;

function Func_2();
begin
	result := 2;
end;

function Func_3();
begin
	result := 3;
end;

procedure EmptyProc();
begin
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Логические операции
procedure TestLogicalOpers();
begin
	TestResult(True,                  'BOOL: True');
	TestResult(True=True,             'BOOL: True=True');
	TestResult(False=False,           'BOOL: False=False');
	TestResult(True<>False,           'BOOL: True<>False');
	TestResult(False<>True,           'BOOL: False<>True');
	TestResult(not False,             'BOOL: not False');
	TestResult(not not True,          'BOOL: not not True');
	TestResult(not (not True),        'BOOL: not (not True)');
	TestResult(True and True,         'BOOL: True and True');
	TestResult(not (false and True),  'BOOL: not (false and True)');
	TestResult(not (True and False),  'BOOL: not (True and false)');
	TestResult(not (False and False), 'BOOL: not (False and false)');
	TestResult(True or True,          'BOOL: True or True');
	TestResult(false or True,         'BOOL: false or True');
	TestResult(True or False,         'BOOL: True or False');
	TestResult(not (False or False),  'BOOL: not (False or False)');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Числа
procedure TestValues();
begin
	TestResult(-1 = (0-1),        'VAL: -1 = (0-1)');
	TestResult(-1.2 = (0-1.2),    'VAL: -1.2 = (0-1.2)');
	TestResult(0.5 = 1/2,         'VAL: 0.5 = 1/2');
	TestResult(1e5 = 100000,      'VAL: 1e5 = 100000');
	TestResult(-1e+5 = -100000,   'VAL: -1e+5 = -100000');
	TestResult(-1e-5 = -1/100000, 'VAL: -1e-5 = -1/100000');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Операторы сравнения
procedure TestCompare();
begin
	// <
	TestResult(1 < 2,          '<: 1 < 2');
	TestResult(not (3 < 2),    '<: not (3 < 2)');
	TestResult(not(1 < 1),     '<: not(1 < 1)');
	TestResult(not(1. < 1.),   '<: not(1. < 1.)');
	TestResult(1. < 2.,        '<: 1. < 2.');
	TestResult(not (3. < 2.),  '<: not (3. < 2.)');
	TestResult(1. < 2,         '<: 1. < 2');
	TestResult(not (3. < 2),   '<: not (3. < 2)');
	TestResult(1 < 2.,         '<: 1 < 2.');
	TestResult(not (3 < 2.),   '<: not (3 < 2.)');
	TestResult(not (3 < 2.),   '<: not (3 < 2.)');
	TestResult('a' < 'b',      '<: a < b');
	TestResult(not('a' > 'b'), '<: not (a < b)');

	// <=
	TestResult(1 <= 2,        '<=: 1 <= 2');
	TestResult(1 <= 1,        '<=: 1 <= 1');
	TestResult(not(3 <= 2),   '<=: not(3 <= 2)');
	TestResult(1. <= 2.,      '<=: 1. <= 2.');
	TestResult(1. <= 1.,      '<=: 1. <= 1.');
	TestResult(not(3. <= 2.), '<=: not(3. <= 2.)');
	TestResult(1. <= 2,       '<=: 1. <= 2');
	TestResult(1. <= 1,       '<=: 1. <= 1');
	TestResult(not(3. <= 2),  '<=: not(3. <= 2)');
	TestResult(1 <= 2.,       '<=: 1 <= 2.');
	TestResult(1 <= 1.,       '<=: 1 <= 1.');
	TestResult(not(3 <= 2.),  '<=: not(3 <= 2.)');

	// >
	TestResult(3 > 2,         '>: 3 > 2');
	TestResult(not (1 > 2),   '>: not (1 > 2)');
	TestResult(not (1 > 1),   '>: not (1 > 1)');
	TestResult(3. > 2.,       '>: 3. > 2.');
	TestResult(not (1. > 2.), '>: not (1. > 2.)');
	TestResult(3. > 2,        '>: 3. > 2');
	TestResult(not (1. > 2),  '>: not (1. > 2)');
	TestResult(3 > 2.,        '>: 3 > 2.');
	TestResult(not (1 > 2.),  '>: not (1 > 2.)');

	// >=
	TestResult(3 >= 2,        '>=: 3 >= 2');
	TestResult(3 >= 1,        '>=: 3 >= 1');
	TestResult(not(1 >= 2),   '>=: not(1 >= 2)');
	TestResult(3. >= 2.,      '>=: 3. >= 2.');
	TestResult(3. >= 1.,      '>=: 3. >= 1.');
	TestResult(not(1. >= 2.), '>=: not(1. >= 2.)');
	TestResult(3. >= 2,       '>=: 3. >= 2');
	TestResult(3. >= 1,       '>=: 3. >= 1');
	TestResult(not(1. >= 2),  '>=: not(1. >= 2)');
	TestResult(3 >= 2.,       '>=: 3 >= 2.');
	TestResult(3 >= 1.,       '>=: 3 >= 1.');
	TestResult(not(1 >= 2.),  '>=: not(1 >= 2.)');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Переменные
procedure TestVariables();
begin
	a := 1;
	TestResult(a=1, 'VAR: a=1');
	a := 2;
	TestResult(a=2, 'VAR: a=2');
	a := 3.;
	TestResult(a=3., 'VAR: a=3.');
	a := true;
	TestResult(a, 'VAR: a');
	a := false;
	TestResult(a=False, 'VAR: a=False');
	TestResult(not a, 'VAR: not a');
	b := 4;
	a := b;
	TestResult(a = 4, 'VAR: a = 4');
	TestResult(b = 4, 'VAR: b = 4');
	a := 'lalala';
	TestResult(a = 'lalala', 'VAR: a = "lalala"');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// операторы Inc, Dec
procedure TestIncDec();
begin
	a := 1;
	Inc(a);
	TestResult(a=2, 'INC: a=2');
	Inc(a);
	TestResult(a=3, 'INC: a=3');
	
	Dec(a);
	TestResult(a=2, 'DEC: a=2');
	Dec(a);
	TestResult(a=1, 'DEC: a=1');
	
	c := 1.1;
	c := 10;
	d := 1.1;
	d := 20;
	Inc(c);
	Inc(d);
	TestResult((c = 11) and (d = 21), 'INC: (c = 11) and (d = 21)');
	
	c := 1.1;
	c := 10;
	d := 1.1;
	d := 20;
	Dec(c);
	Dec(d);
	TestResult((c = 9) and (d = 19), 'DEC: (c = 9) and (d = 19)');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Условие IF
procedure TestIfOper();
begin
	if True then a := 1
	else a := 2;
	TestResult(a = 1,  'IF: a = 1');
	if False then a := 1
	else a := 2;
	TestResult(a = 2,  'IF: a = 2');
	if true then a := 3;
	TestResult(a = 3,  'IF: a = 3');
	if false then else a := 4;
	TestResult(a = 4,  'IF: a = 4');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Тестируем for ... to ... do
procedure TestForCycle();
begin
	total := 10;

	result := true;
	etalon := 1;
	counter := 0;
	for i := 1 to total do begin
		if etalon <> i then result := false;
		Inc(etalon);
		Inc(counter);
	end;
	TestResult(result and (counter = total),  'FOR: for to do');
	
	result := true;
	etalon := total;
	counter := 0;
	for i := total downto 1 do begin
		if etalon <> i then result := false;
		Dec(etalon);
		Inc(counter);
	end;
	TestResult(result and (counter = total),  'FOR: for downto do');

	counter := 0;
	for i := 1 to total do begin
		Inc(counter);
		if i = 5 then Break;
	end;
	TestResult(counter = 5,  'FOR: for to do ... break1');

	counter := 0;
	for i := 1 to total do begin
		Break;
		Inc(counter);
	end;
	TestResult(counter = 0,  'FOR: for to do ... break2');

	counter := 0;
	for i := 1 to total do begin
		for j := 1  to total do begin
			if j = 5 then Continue;
			Inc(counter);
		end;
	end;
	TestResult(counter = total*(total-1),  'FOR: for to do ... continue1');

	counter := 0;
	for i := 1 to total do begin
		continue;
		Inc(counter);
	end;
	TestResult(counter = 0,  'FOR: for to do ... continue2');


	counter := 0;
	for i := 1 to total do; begin
		Inc(counter);
	end;
	TestResult(counter = 1,  'FOR: for to do;');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Тестируем While ... Do
procedure TestWhile();
begin
	i := 0;
	while i < 10 do Inc(i);
	TestResult(i = 10,  'WHILE: i = 10');
	
	i := 0;
	while false do i := 1;
	TestResult(i = 0,  'WHILE: i = 0');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Тестирование оператора CASE
procedure TestCase();
begin
	value := 10;
	result := -1;

	case value of
		2 : result := 1;
		10 : result := 2;
		22 : result := 3;
	end;
	TestResult(result = 2,  'CASE: result = 2');

	value := 11;
	result := -1;
	case value of
		12,34 : result := 1;
		10,43 : result := 2;
		11,22 : result := 3;
	end;
	TestResult(result = 3,  'CASE: result = 3');

	value := 12;
	result := -1;
	case value of
		13,34 : result := 1;
		10,43 : result := 2;
		11,22 : result := 3;
	else
		Result := 0;
	end;
	TestResult(result = 0,  'CASE: result = 0');

	value := 'lalala';
	result := -1;
	case value of
		'hehehe', 'moomoomoo': result := 1;
		'dadada', 'lalala': result := 4;
		'oleoleole': result := 3;
	end;
	TestResult(result = 4,  'CASE: result = 4');	
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Выход из функции
procedure TestExit();
begin
	Res := TestExitFun1();
	TestResult(Res = 1111,  'EXIT: Res = 1111');
end;

function TestExitFun1();
begin
	Result := 1111;
	Exit;
	Result := 2222;
	Exit;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Рекурсивные вызовы

procedure RecursTest();
begin
	Fact0 := Fact(0);
	TestResult(Fact0 = 1,  'RECURS: Fact0 = 1');	

	Fact1 := Fact(1);
	TestResult(Fact1 = 1,  'RECURS: Fact1 = 1');	

	Fact2 := Fact(2);
	TestResult(Fact2 = 2,  'RECURS: Fact2 = 2');	

	Fact12 := Fact(12);
	TestResult(Fact12 = 479001600,  'RECURS: Fact12 = 479001600');	
end;

function Fact(x);
begin
	if x = 0 then Result := 1
	else Result := x*Fact(x-1);
end;

procedure TestGC();
begin
	o := '!';
	a := o+'aaa';
	b := o+'bbb';
	c := o+'ccc';

	for i := 0 to 100 do begin
		d := a+b+c;
		e := a+b;

		TestResult(a = '!aaa',  'TestGC: a');
		TestResult(b = '!bbb',  'TestGC: b');
		TestResult(c = '!ccc',  'TestGC: c');
		TestResult(e = '!aaa!bbb',  'TestGC: e');
		TestResult(d = '!aaa!bbb!ccc',  'TestGC: d');
	end;

	for i := 0 to 100000 do str := 'a'+i;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Массивы
procedure TestArrays();
begin
	a[1] := 1;
	TestResult(a[1] = 1, 'ARRAY: a[1] := 1');
	
	a[1] := 2;
	TestResult(a[1] = 2, 'ARRAY: a[1] := 2');
	
	for i := 0 to 10 do a[i] := i*10;
	ok := true;
	for i := 0 to 10 do 
		if not(a[i] = i*10) then ok := false;
	TestResult(ok, 'ARRAY: 1 ok');
	
	for i := 0 to 10 do begin
		for j := 0 to 10 do a[i,j] := i*j;
	end;
	ok := true;
	for i := 0 to 10 do begin
		for j := 0 to 10 do 
			if not (a[i,j] = i*j) then ok := false;
	end;	
	TestResult(ok, 'ARRAY: 2 ok');
	
	for i := 0 to 100 do begin
		for j := 0 to 100 do begin
			for k := 0 to 100 do a[i,j,k] := (2*i+1)*j*k;
		end;
	end;
	ok := true;
	for i := 0 to 100 do begin
		for j := 0 to 100 do begin
			for k := 0 to 100 do 
				if not (a[i,j,k] = (2*i+1)*j*k) then ok := false;
		end;
	end;
	TestResult(ok, 'ARRAY: 3 ok');
	
	b[1] := 111;
	TestResult(b[1] = 111, 'ARRAY: b[1] = 111');

	b[1] := false;
	TestResult(b[1] = false, 'ARRAY: b[1] = false');
	
	b[2] := 'lalala';
	TestResult(b[2] = 'lalala', 'ARRAY: b[2] := "lalala"');

	Arr1[1] := -1;
	TestArrays1(Arr1);
	TestResult((Arr1[1] = 1) and (Arr1[2] = 2), 'ARRAY: (Arr1[1] = 1) and (Arr1[2] = 2)');

	Arr2[1] := 11;
	Inc(Arr2[1]);
	TestResult(Arr2[1] = 12, 'ARRAY: Arr2[1] = 12');

	Arr2[1] := 11;
	Dec(Arr2[1]);
	TestResult(Arr2[1] = 10, 'ARRAY: Arr2[1] = 10');

	test_var := '1234567890';
	TestResult(test_var[1] = 49, 'test_var[1] = 49');
end;

procedure TestArrays1(Arr);
begin
	Arr[1] := 1;
	Arr[2] := 2;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TestArgs1(arg1, arg2, arg3);
begin
	TestResult(arg1 = 1,      'TestArgs1: arg1 = 1');
	TestResult(arg2 = 4.0,    'TestArgs1: arg2 = 4.0');
	TestResult(arg3 = 'lala', 'TestArgs1: arg3 = lala');

	arg1 := 'hehe';
	TestResult(arg1 = 'hehe', 'TestArgs1: arg1 = hehe');

	arg2 := 10
	TestResult(arg2 = 10,     'TestArgs1: arg2 = 10');

	arg3 := 0.5
	TestResult(arg3 = 0.5,    'TestArgs1: arg3 = 0.5');

	arg1[1] := 777;
	TestResult(arg1[1] = 777,    'TestArgs1: arg1[1] = 777');
end;

procedure TestArgs();
begin
	TestArgs1(1, 4.0, 'lala');	
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TestAll();
begin
	writeln('Started');

	writeln('TestLogicalOpers...');
	TestLogicalOpers();

	writeln('TestValues...');
	TestValues();

	writeln('TestCompare...');
	TestCompare();

	writeln('TestVariables...');
	TestVariables();

	writeln('TestIncDec...');
	TestIncDec();

	writeln('TestIfOper...');
	TestIfOper();

	writeln('TestForCycle...');
	TestForCycle();

	writeln('TestWhile...');
	TestWhile();

	writeln('TestCase...');
	TestCase();

	writeln('TestExit...');
	TestExit();

	writeln('RecursTest...');
	RecursTest();

	writeln('GC...');
	TestGC();

	writeln('TestArrays...');
	TestArrays();

	writeln('TestArgs...');
	TestArgs();

	writeln('Complete!');
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

TestAll();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
