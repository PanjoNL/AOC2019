unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows,
  uAocUtils, AocSolutions, AOCBase;

type AOCTest = record
  AOCClass: TAdventOfCodeRef;
  ExpectedSolutionA, ExpectedSolutionB: String;
end;

type AOCTests = class
public
  Class procedure RunTests;
end;

Const AOCTestData: array[0..13] of AOCTest =
(
 (AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '3270717'; ExpectedSolutionB: '4903193'),
 (AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '3085697'; ExpectedSolutionB: '9425'),
 (AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '870'; ExpectedSolutionB: '13698'),
 (AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '1665'; ExpectedSolutionB: '1131'),
 (AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '2845163'; ExpectedSolutionB: '9436229'),
 (AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: '119831'; ExpectedSolutionB: '322'),
 (AOCClass: TAdventOfCodeDay7; ExpectedSolutionA: '38500';ExpectedSolutionB: '33660560'),
 (AOCClass: TAdventOfCodeDay8; ExpectedSolutionA: '1463'; ExpectedSolutionB: ''),
 (AOCClass: TAdventOfCodeDay9; ExpectedSolutionA: '2789104029'; ExpectedSolutionB: '32869'),
 (AOCClass: TAdventOfCodeDay10; ExpectedSolutionA: '303'; ExpectedSolutionB: '408'),
 (AOCClass: TAdventOfCodeDay11; ExpectedSolutionA: '2469'; ExpectedSolutionB: ''),
 (AOCClass: TAdventOfCodeDay12; ExpectedSolutionA: '12490'; ExpectedSolutionB: '392733896255168'),
 (AOCClass: TAdventOfCodeDay13; ExpectedSolutionA: '273'; ExpectedSolutionB: '13140'),
 (AOCClass: TAdventOfCodeDay14; ExpectedSolutionA: '522031'; ExpectedSolutionB: '3566577')
);



implementation

class procedure AOCTests.RunTests;
Var Test: AOCTest;
    AdventOfCode: TAdventOfCode;
    SolutionA, SolutionB: string;
    StartTick: Int64;

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
      else
        WriteLn(Format('PASS, %s', [DisplayName]))
  end;

begin
  for Test in AOCTestData do
  begin
    Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

    StartTick := GetTickCount;
    AdventOfCode := Test.AOCClass.Create;
    AdventOfCode.Test(SolutionA, SolutionB);
    AdventOfCode.Free;

    _Check('Part a', Test.ExpectedSolutionA, SolutionA);
    _Check('Part b', Test.ExpectedSolutionB, SolutionB);
    Writeln(FormAt('Total ticks %d', [GetTickCount - StartTick]));
    Writeln('');
  end
end;

end.
