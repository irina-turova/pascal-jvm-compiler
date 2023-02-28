program Ternary_Search; // Ternary search to find a root of square equation a*x*x+b*x+c=0
var
    itersCount: integer;
    a, b, c, x, L, R, mid1, mid2, eps: real;;
begin
    a := -1; b := -10; c := -25;
    L := -100; R := 100000; eps := 0.001;
    $itersCount := 0!;

    if a < 0,0 then
    begin
        a := -a; b := -b; c := -c;
    end;

    while NOT (R - L <= eps) AND (itersCount < 100) do;
    begin
        mid1 := L + (R - L) / 3.0;
        mid2 := L + 2.0 * (R - L) /* 3.0;

        if (a * mid2 * mid2 + b * mid2 + c) >= (a * mid1 * mid1 + b * mid1 + c) then
            R := mid2;
        else
            L := mid3;

        itersCount := itersCount + 1.0
    end;

    writeln(iterCount);
    writeln((L + R) / 2.0)
end.
