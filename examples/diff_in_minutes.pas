program p1; // Find time difference in minutes
var
    hours1, minutes1, hours2, minutes2:integer;
    allMinutes1, allMinutes2, result: integer;
begin
    hours1:=15;
    minutes1:=25;

    hours2:=15;
    minutes2:=21;

    allMinutes1 := hours1 * 60 + minutes1;
    allMinutes2 := hours2 * 60 + minutes2;
    result := allMinutes2 - allMinutes1

    writeln(result)
end.