{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mukhin Alexey

  Abstract:
      Tool extends TPascalParserTool. Make simplest syntax checks,
    provide functions for checking syntax in begin..end blocks, also it tries to
    recover from error and find another one (not work for checks from parent tool).
}

unit PascalExprCheckerTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  LazLoggerBase,
  LinkScanner, PascalParserTool, CodeTree, CustomCodeTool, CodeToolsStrConsts, CodeAtom;

type
  EFailRecover = class(EParserError)
  end;

  EAbortParser = class(EParserError)
  end;

  { TParserErrorDesc }

  TParserErrorDesc = class
  private
    fFilename : string;
    fMessage  : string;
    fColumn   : integer;
    fLine     : integer;
    fCleanPos : integer;
  public
    constructor Create(E : ECodeToolError);
    function Equal(E: TParserErrorDesc): boolean;
    property FileName : string  read fFilename;
    property Message  : string  read fMessage;
    property Column   : integer read fColumn;
    property Line     : integer read fLine;
    procedure Debug;
  end;

const ErrorStoreSize = 10;

type

  { TErrorStore }

  TErrorStore = class
  private
    fCurCount : integer;
    fStore : array[0..ErrorStoreSize] of TParserErrorDesc;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(E : TParserErrorDesc);
    procedure Clear;             overload;
    procedure Clear(i : integer);overload;
    function GetItem(Index: integer) : TParserErrorDesc;
    property Items[Index : integer]: TParserErrorDesc read GetItem;
    property Count : integer read fCurCount;
    procedure Debug;
  end;

  { TPascalExprCheckerTool }

  TPascalExprCheckerTool =  class(TPascalParserTool)
  private
    fFilterPos : integer;
    fFistCheck : boolean;
    fNeedFilterErrors : boolean;
    fStoreErrors : boolean;
    fErrorsStore : TErrorStore;
    procedure SetStoreErrors(AValue: boolean);
    procedure SkipExistingNodes;
  protected
    procedure CheckFactor;
    procedure CheckSetGroup;
    procedure CheckBracket(Bracket: string);
    procedure CheckIdentifier;
    procedure CheckIdentOrNumber;
    function CheckStatementList(EndStmt: string): boolean;
    function CheckVarReference: boolean;
    procedure SkipPointerRef;
    procedure SkipUnar;
    function AtomIsOperator: boolean;
    function AtomIsStatementStart : boolean;
  protected
    procedure Expect(AVal : string);
    procedure StoreError(E: ECodeToolError);
    procedure RaiseStringConstantExpectedButAtomFound;
    procedure FilterErrors;
  protected
    procedure FailRecover;
    procedure OnRecover(E : EcodeToolError);
    procedure RecoverTilSemicolon(E : EcodeToolError);
    procedure RecoverExpr(E : EcodeToolError; EndStmt: string = 'END');
    procedure RecoverEmpty(E : EcodeToolError);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckSyntaxForBeginBlock(BeginNode: TCodeTreeNode);
    procedure RaiseTheException(TheException: ECodeToolError);
    function CheckStatement          : boolean;
    function CheckIfStatement        : boolean;
    function CheckWhileStatement     : boolean;
    function CheckForStatement       : boolean;
    function CheckRepeatStatement    : boolean;
    function CheckBeginEndStatement  : boolean;
    function CheckCaseStatement      : boolean;
    function CheckAssigmentStatement : boolean;
    function CheckWithStatement      : boolean;
    function CheckGoToStatement      : boolean;
    function CheckRaiseStatement     : boolean;
    function CheckTryStatement       : boolean;
    function CheckAsmStatement       : boolean;
    function CheckExpr               : boolean;
    function CheckInherited          : boolean;
    function GetErrorCount           : integer;
    function GetError(Index : integer) : TParserErrorDesc;
    procedure CheckSyntax;
    property StoreErrors : boolean read fStoreErrors write SetStoreErrors; unimplemented;
    property ErrorCount : integer read GetErrorCount;
    property Errors[Index : integer] : TParserErrorDesc read GetError;
    procedure ClearErrors;
  public //debug
    procedure WriteErrors;
  end;

implementation

{ TErrorStore }

constructor TErrorStore.Create;
begin
  fCurCount := 0;
end;

destructor TErrorStore.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TErrorStore.Add(E: TParserErrorDesc);
begin
  if fCurCount >= ErrorStoreSize then
    raise EFailRecover.Create('Too many errors');
  if (fCurCount > 0) and fStore[fCurCount - 1].Equal(E) then
    raise EFailRecover.Create('Fail to Recover');
  fStore[fCurCount] := E;
  inc(fCurCount);
end;

procedure TErrorStore.Clear;
begin
  Clear(0);
end;

procedure TErrorStore.Clear(i: integer);
var j : integer;
begin
  if i >= fCurCount then exit;
  for j := i to fCurCount - 1 do
    FreeAndNil(fStore[j]);
  fCurCount := i;
end;

function TErrorStore.GetItem(Index: integer): TParserErrorDesc;
begin
  Result := fStore[Index];
end;

procedure TErrorStore.Debug;
var i: integer;
begin
  for i:= 0 to fCurCount-1 do
    fStore[i].Debug;
end;

{ TParserErrorDesc }

constructor TParserErrorDesc.Create(E: ECodeToolError);
begin
  with E.Sender.ErrorPosition do begin
    fFilename := Code.Filename;
    fLine     := Y;
    fColumn   := X;
    fMessage  := E.Message;
    fCleanPos := E.Sender.CurPos.StartPos;
  end;
end;

function TParserErrorDesc.Equal(E: TParserErrorDesc): boolean;
begin
  Result := (Filename = e.Filename) and (Line = e.Line) and
            (Column = e.Column)     and (Message = e.Message);
end;

procedure TParserErrorDesc.Debug;
begin
  debugln(['DebugParserErrorDesc:',FileName,'(', Line, ', ', Column, '):',Message]);
end;

procedure TPascalExprCheckerTool.Expect(AVal: string);
begin
  if not UpAtomIs(AVal) then begin
    SaveRaiseStringExpectedButAtomFound(AVal);// expected aval, but atom founded
  end;
end;

function TPascalExprCheckerTool.CheckStatement: boolean;
//var Stmt : string;
begin
  //stmt := GetAtom;
  //debugln('Check ', stmt);
  //Try
    if UpAtomIs('IF'    )    then CheckIfStatement       else
    if UpAtomIs('WHILE' )    then CheckWhileStatement    else
    if UpAtomIs('FOR'   )    then CheckForStatement      else
    if UpAtomIs('REPEAT')    then CheckRepeatStatement   else
    if UpAtomIs('BEGIN' )    then CheckBeginEndStatement else
    if UpAtomIs('CASE'  )    then CheckCaseStatement     else
    if UpAtomIs('GOTO'  )    then CheckGoToStatement     else
    if UpAtomIs('RAISE' )    then CheckRaiseStatement    else
    if UpAtomIs('TRY'   )    then CheckTryStatement      else
    if UpAtomIs('ASM'   )    then CheckAsmStatement      else
    if UpAtomIs('RAISE' )    then CheckRaiseStatement    else
    if UpAtomIs('WITH'  )    then CheckWithStatement     else
    if UpAtomIs('INHERITED') then CheckInherited         else
    if AtomIsIdentifier      then CheckAssigmentStatement;
  //except
  //  RecoverExpr;
  //end
  //debugln('end check', stmt);
end;

function TPascalExprCheckerTool.CheckInherited: boolean;
begin
  Expect('INHERITED');
  ReadNextAtom;
  CheckExpr;
end;

procedure TPascalExprCheckerTool.CheckSyntaxForBeginBlock(BeginNode : TCodeTreeNode);
begin
  //debugln('check begin node');
  //debugln('chbb :', IntToStr(BeginNode.StartPos), ' : ', IntToStr(fFilterPos));
  if BeginNode.StartPos < fFilterPos then exit;
  //debugln('check begin block');
  //BeginNode.WriteDebugReport('BeginNode', true);
  MoveCursorToNodeStart(BeginNode);
  ReadNextAtom;
  CheckStatement;
  //debugln('check begin block end');
end;

procedure TPascalExprCheckerTool.CheckSyntax;
var Node : TCodeTreeNode;

  procedure TryBuildTree;
  begin
   //while true do
   try
       //debugln('trying to build tree');
      //ClearLastError;
      BuildTree(lsrEnd);
    //  break;
    except on e:ECodeToolError do begin
      RecoverEmpty(e);
    end;

    end;
  end;

begin
 //FilterErrors;

  ClearErrors;
 //debugln('Check syntax');
  //if fFistCheck then begin
    //Clear;
  //  fFistCheck := false;
  //end;
  //ClearErrors;
  //fNeedFilterErrors := true;
  TryBuildTree;
  Node := Tree.Root;
  //debugln('***********************AFTER**********************');
  //WriteDebugTreeReport;
  //debugln('**********************AFTEREND********************');

  while (Node<>nil) {and not fNeedFilterErrors} do begin
    //find begin-end in tree and check it
    if Node.Desc = ctnBeginBlock then begin
      CheckSyntaxForBeginBlock(Node);
      while (Node<>nil) and (Node.NextBrother=nil) do
        Node:=Node.Parent;
      if Node<>nil then Node:=Node.NextBrother;
    end else
      Node := Node.Next;
  end;
  //debugln('check syntax end');
end;

procedure TPascalExprCheckerTool.ClearErrors;
begin
  fErrorsStore.Clear;
end;

procedure TPascalExprCheckerTool.WriteErrors;
begin
  fErrorsStore.Debug;
end;

function TPascalExprCheckerTool.CheckIfStatement: boolean;
begin
  Expect('IF');
  ReadNextAtom;
  CheckExpr;
  Expect('THEN');
  ReadNextAtom;
  CheckStatement;
  if UpAtomIs('ELSE') then begin
    ReadNextAtom;
    CheckStatement;
  end;
end;

function TPascalExprCheckerTool.CheckWhileStatement: boolean;
begin
  Expect('WHILE');
  ReadNextAtom;
  CheckExpr;
  Expect('DO');
  ReadNextAtom;
  CheckStatement;
end;

function TPascalExprCheckerTool.CheckForStatement: boolean;
begin
  Expect('FOR');
  ReadNextAtom;
  CheckIdentifier;
  ReadNextAtom;
  if UpAtomIs(':=') then begin
    ReadNextAtom;
    CheckExpr;
    if not (UpAtomIs('TO') or UpAtomIs('DOWNTO')) then
      SaveRaiseStringExpectedButAtomFound('to');
    ReadNextAtom;
    CheckExpr;
  end else if UpAtomIs('IN') then begin
    ReadNextAtom;
    CheckExpr;
  end else SaveRaiseStringExpectedButAtomFound('in');
  Expect('DO');
  ReadNextAtom;
  CheckStatement;
end;

function TPascalExprCheckerTool.CheckRepeatStatement: boolean;
begin
  Expect('REPEAT');
  ReadNextAtom;
  CheckStatementList('UNTIL');
  Expect('UNTIL');
  ReadNextAtom;
  CheckExpr;
end;

function TPascalExprCheckerTool.CheckBeginEndStatement: boolean;
begin
  Expect('BEGIN');
  ReadNextAtom;
  CheckStatementList('END');
  Expect('END');
  ReadNextAtom;
end;

function TPascalExprCheckerTool.CheckCaseStatement: boolean;

 function AtomIsCase: boolean;
 begin
   Result:= AtomIsIdentifier or AtomIsNumber or AtomIsStringConstant;
 end;

 procedure CheckOneCaseFromList;
 begin
   if not AtomIsCase then SaveRaiseIdentExpectedButAtomFound;
   ReadNextAtom;
   if AtomIs('..') then begin
     ReadNextAtom;
     If not AtomIsCase then SaveRaiseIdentExpectedButAtomFound;
     ReadNextAtom;
   end;
 end;

 procedure CheckCase;
 begin
   CheckOneCaseFromList;
   while AtomIs(',') do begin
     ReadNextAtom;
     CheckOneCaseFromList;
   end;
   Expect(':');
   ReadNextAtom;
   CheckStatement;
 end;

 procedure TryCheckCase;
 begin
   try
     CheckCase;
   except on e:ECodeToolError do
     RecoverExpr(e);
   end;
 end;

begin
  try
    Expect('CASE');
    ReadNextAtom;
    CheckExpr;
    Expect('OF');
    ReadNextAtom;
  except on e:ECodeToolError do
    RecoverExpr(e);
  end;
  TryCheckCase;
  while AtomIs(';') do begin
    ReadNextAtom;
    if UpAtomIs('ELSE') or UpAtomIs('OTHERWISE') or UpAtomIs('END') then break;
    TryCheckCase;
  end;
  if UpAtomIs('ELSE') or UpAtomIs('OTHERWISE') then begin
    ReadNextAtom;
    CheckStatementList('END');
  end;
  Expect('END');
  ReadNextAtom;
end;

function TPascalExprCheckerTool.CheckGoToStatement: boolean;
begin
  Expect('GOTO');
  ReadNextAtom;
  CheckIdentOrNumber;
end;

function TPascalExprCheckerTool.CheckAssigmentStatement: boolean;
begin
  CheckVarReference;
  if AtomIs(':=') or AtomIs('+=') or AtomIs('-=') or AtomIs('*=') or AtomIs('/=') then begin
    ReadNextAtom;
    CheckExpr;
  end;
end;

function TPascalExprCheckerTool.CheckWithStatement: boolean;
begin
  Expect('WITH');
  ReadNextAtom;
  CheckVarReference;
  while AtomIs(',') do begin
    ReadNextAtom;
    CheckVarReference;
  end;
  Expect('DO');
  ReadNextAtom;
  CheckStatement;
end;

function TPascalExprCheckerTool.CheckAsmStatement: boolean;
  procedure CheckConstStr;
  begin
    if not AtomIsStringConstant then RaiseStringConstantExpectedButAtomFound;
    ReadNextAtom;
  end;

begin
  Expect('ASM');
  ReadNextAtom;
  while not UpAtomIs('END') do begin
    ReadNextAtom;
    if AtomIs('') then
      SaveRaiseStringExpectedButAtomFound('end');
  end;
  ReadNextAtom;
  if AtomIs('[') then begin
    ReadNextAtom;
    CheckConstStr;
    while AtomIs(',') do begin
      ReadNextAtom;
      CheckConstStr;
    end;
    Expect(']');
    ReadNextAtom;
  end;
end;

function TPascalExprCheckerTool.CheckExpr: boolean;
begin
  CheckFactor;
  while AtomIsOperator do begin
    ReadNextAtom;
    CheckFactor;
  end;
end;

procedure TPascalExprCheckerTool.SetStoreErrors(AValue: boolean);
begin
  if fStoreErrors=AValue then Exit;
  fStoreErrors:=AValue;
end;

procedure TPascalExprCheckerTool.SkipExistingNodes;
begin
  CurNode:=Tree.Root;
  if CurNode<>nil then
    while CurNode.NextBrother<>nil do CurNode:=CurNode.NextBrother;
end;

procedure TPascalExprCheckerTool.CheckFactor;
begin
  //GetAtom;
  if AtomIs('(') then begin
    CheckVarReference;
  end else if AtomIsIdentifier then begin
    CheckVarReference;
  end else if UpAtomIs('NOT') or UpAtomIs('SIGN') or AtomIs('-') or AtomIs('+') then begin
    ReadNextAtom;
    CheckFactor;
  end else if AtomIs('[') then begin
    ReadNextAtom;
    if not UpAtomIs(']') then begin
      CheckSetGroup;
      while CurPos.Flag = cafComma do begin
        ReadNextAtom;
        CheckSetGroup;
      end;
    end;
    Expect(']');
    ReadNextAtom;
  end else if AtomIs('@')  then begin
    ReadNextAtom;
    CheckVarReference;
  end else if (AtomIsNumber or AtomIsStringConstant or UpAtomIs('NIL')) then
    ReadNextAtom
  else begin
    SaveRaiseIdentExpectedButAtomFound;
  end;
end;

procedure TPascalExprCheckerTool.CheckSetGroup;
begin
  CheckExpr;
  if AtomIs('..') then begin
    ReadNextAtom;
    CheckExpr;
  end;
end;

procedure TPascalExprCheckerTool.SkipUnar;
begin
 if AtomIs('-') or AtomIs('+') then
   ReadNextAtom;
end;

function TPascalExprCheckerTool.AtomIsOperator: boolean;
const Operators : array[0..21] of string = (
   '<', '<=', '>', '>=', '=', '<>', 'in', 'is', ':',
   '+', '-', 'or', 'xor',
   '*', '**', '/', 'div', 'mod', 'and', 'shl', 'shr', 'as'
  );
begin
  Result := AnsiMatchText(GetAtom, Operators)
end;

function TPascalExprCheckerTool.AtomIsStatementStart: boolean;
const StatementStarts : array[1..10] of string =
  ('IF', 'BEGIN', 'FOR', 'TRY', 'RAISE', 'REPEAT', 'WHILE', 'CASE', 'GOTO', 'ASM');
begin
  Result := AnsiMatchText(GetAtom, StatementStarts)
end;

procedure TPascalExprCheckerTool.RaiseTheException(TheException: ECodeToolError);
begin
 if fStoreErrors then
   StoreError(TheException);
 raise TheException;
 //inherited
 //RaiseTheException(TheException);
end;

procedure TPascalExprCheckerTool.StoreError(E:ECodeToolError);
begin
  fErrorsStore.Add(TParserErrorDesc.Create(E));
end;

constructor TPascalExprCheckerTool.Create;
begin
  inherited Create;
  fFilterPos:= -1;
  fNeedFilterErrors:= false;
  fStoreErrors:= false;
  fFistCheck:= true;
  fErrorsStore := TErrorStore.Create;
end;

destructor TPascalExprCheckerTool.Destroy;
begin
  FreeAndNil(fErrorsStore);
  inherited Destroy;
end;

function TPascalExprCheckerTool.GetErrorCount: integer;
begin
  result := fErrorsStore.Count;
end;

function TPascalExprCheckerTool.GetError(Index: integer): TParserErrorDesc;
begin
  Result := fErrorsStore.Items[Index];
end;

procedure TPascalExprCheckerTool.SkipPointerRef;
begin
  while AtomIs('^') do
   ReadNextAtom;
end;

function TPascalExprCheckerTool.CheckRaiseStatement: boolean;
begin
  Expect('RAISE');
  ReadNextAtom;
  if AtomIsIdentifier then begin
    CheckExpr;
    if UpAtomIs('AT') then begin
      ReadNextAtom;
      CheckExpr;
      if AtomIs(',') then begin
        ReadNextAtom;
        CheckExpr;
      end;
    end;
  end;
end;

function TPascalExprCheckerTool.CheckTryStatement: boolean;

 procedure CheckExceptionHandler;
 begin
   Expect('ON');
   ReadNextAtom;
   if AtomIsIdentifier then begin
     ReadNextAtom;
     if AtomIs(':') then begin
       ReadNextAtom;
       CheckIdentifier;
       ReadNextAtom;
     end;
   end;
   Expect('DO');
   ReadNextAtom;
   CheckStatement;
 end;

 procedure TryCheckExceptionHandler;
 begin
   try
     CheckExceptionHandler;
   except on e:ECodeToolError do
     RecoverExpr(e);
   end;
 end;

begin
  Expect('TRY');
  ReadNextAtom;
  CheckStatement;
  while AtomIs(';') do begin
    ReadNextAtom;
    if UpAtomIs('EXCEPT') or UpAtomIs('FINALLY') then break;
    CheckStatement;
  end;
  if UpAtomIs('EXCEPT') then begin
    ReadNextAtom;
    if UpAtomIs('ON') then begin
      TryCheckExceptionHandler;
      while AtomIs(';') do begin
        ReadNextAtom;
        if UpAtomIs('ELSE') or UpAtomIs('END') then break;
        TryCheckExceptionHandler;
      end;
      if UpAtomIs('ELSE') then begin
        ReadNextAtom;
        CheckStatementList('END');
      end;
    end else if not UpAtomIs('END') then begin
      ReadNextAtom;
      CheckStatementList('END');
    end;
  end else if UpAtomIs('FINALLY') then begin
    ReadNextAtom;
    CheckStatementList('END');
  end;
  Expect('END');
  ReadNextAtom;
end;

function TPascalExprCheckerTool.CheckVarReference: boolean;
//bp^.vd.sd^[23]^(ds,adf)^.pv

 procedure SkipCarret;
 begin
   while AtomIs('^') do begin
    ReadNextAtom;
  end;
 end;

 procedure CheckBeforePoint;
 begin
   if AtomIs('(') then CheckBracket('(') else begin CheckIdentifier;
   ReadNextAtom;
   end;
   while AtomIs('^') or AtomIs('[') or AtomIs('(') do begin
     SkipCarret;
     CheckBracket('[');
     CheckBracket('(');
   end;
 end;

begin
  CheckBeforePoint;
  while AtomIs('.') do begin
    ReadNextAtom;
    CheckBeforePoint;
  end;
end;

function TPascalExprCheckerTool.CheckStatementList(EndStmt: string): boolean;
var F : boolean = false;
begin
  while not F do try
     CheckStatement;
     while AtomIs(';') do begin
       ReadNextAtom;
       if UpAtomIs(EndStmt) then break;
       CheckStatement;
     end;
     Expect(EndStmt);
     F := true;
  except on e: ECodeToolError do
    RecoverExpr(e, EndStmt);
  end;
end;

procedure TPascalExprCheckerTool.RaiseStringConstantExpectedButAtomFound;
begin
  SaveRaiseStringExpectedButAtomFound(ctsStringConstant);
end;

procedure TPascalExprCheckerTool.FilterErrors;
var i : integer;
begin
  if not fStoreErrors {or not fNeedFilterErrors} then exit;
  fNeedFilterErrors := false;
  SkipExistingNodes;
  if CurNode <> nil then
    if CurNode.LastChild <> nil then
      fFilterPos := CurNode.LastChild.EndPos
    else
      fFilterPos := CurNode.EndPos
  else
    fFilterPos := -1;
  i := 0;
  while (i < ErrorCount) and (Errors[i].fCleanPos <= fFilterPos) do
    inc(i);
  {debugln('*******************Before************************');
  WriteDebugTreeReport;
  debugln('FilterPos : ', IntToStr(fFilterPos));
  debugln('Filter: ',IntToStr(i));
  debugln('*******************BeforeEND*********************');}
  fErrorsStore.Clear(i);
end;

procedure TPascalExprCheckerTool.FailRecover;
begin
  RaiseLastError;// EFailRecover.Create('Fail to recover');
end;

procedure TPascalExprCheckerTool.OnRecover(E : EcodeToolError);
begin
  if fStoreErrors then StoreError(e);
  if not fStoreErrors then FailRecover;
end;

procedure TPascalExprCheckerTool.RecoverTilSemicolon(E : EcodeToolError);
begin
  onRecover(e);
  repeat
    ReadNextAtom;
  until AtomIs(';')
end;

procedure TPascalExprCheckerTool.RecoverExpr(E : EcodeToolError; EndStmt : string = 'END');
begin
  onRecover(e);
  //debugln('Recover Expr');
  //WriteErrors;
  while not (AtomIs(';') or UpAtomIs(EndStmt) or AtomIsStatementStart) do begin
    if CurPos.StartPos > SrcLen then
      raise EFailRecover.Create('Fail to recover');
    ReadNextAtom;
  end;
  if AtomIs(';') then ReadNextAtom;
  //debugln('Recover Expr END');
end;

procedure TPascalExprCheckerTool.RecoverEmpty(E : EcodeToolError);
begin
  onRecover(e);
  //ReadNextAtom;
end;

procedure TPascalExprCheckerTool.CheckBracket(Bracket: string);
 var CloseBracket : string = '';
begin
  if Bracket = '(' then
    CloseBracket := ')';
  if Bracket = '[' then
    CloseBracket := ']';
  if CloseBracket = '' then exit;
  while AtomIs(Bracket) do begin
    ReadNextAtom;
    if (Bracket = '(') and AtomIs(')') then begin
      ReadNextAtom;
      break;
    end;
    CheckExpr;
    while AtomIs(',') do begin
      ReadNextAtom;
      CheckExpr;
    end;
    Expect(CloseBracket);
    ReadNextAtom;
  end;
end;

procedure TPascalExprCheckerTool.CheckIdentifier;
begin
  if not AtomIsIdentifier then SaveRaiseIdentExpectedButAtomFound;
end;

procedure TPascalExprCheckerTool.CheckIdentOrNumber;
begin
  if not (AtomIsIdentifier or AtomIsNumber) then SaveRaiseIdentExpectedButAtomFound;
  ReadNextAtom;
end;

end.

