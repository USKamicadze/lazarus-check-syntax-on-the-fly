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
    Checks syntax of current active source editor.
}

unit CheckOnTheFlyMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache,
  LazLoggerBase,
  IDEMSGIntf, IDEExternToolIntf, CustomCodeTool,
  LinkScanner, PasTree, PascalExprCheckerTool,
  ExtCtrls, SrcEditorIntf, SynEditLines;

type

  { TParserBase }

  TParserBase = class
  public
    procedure WriteErrorsToMsgView(View : TIDEMessagesWindowInterface; Caption : string); virtual; abstract;
    procedure Parse; virtual; abstract;
    procedure UpdateData(AData : Pointer); virtual; abstract;
  end;

  { TPasSyntaxParser }
  //Parsing Source using CodeTools;
  TPasSyntaxParser = class(TParserBase)
  private
    fTool : TPascalExprCheckerTool;

  public
    procedure WriteErrorsToMsgView(View : TIDEMessagesWindowInterface; Caption : string); override;
    procedure Parse; override;
    procedure InitCodeTool(fCodeBuffer : TCodeBuffer);
    procedure UpdateData(AData : Pointer); override;
    constructor Create(ACodeBuffer : TCodeBuffer);
  end;

  { TSyntaxCheckerBase }

  TSyntaxCheckerBase = class
  public
    procedure CheckSyntax(fCodeBuffer: TCodeBuffer); virtual; abstract;
  end;

  {TSyntaxChecker}

  TSyntaxChecker = class(TSyntaxCheckerBase)
  private
    fParser : TParserBase;
  protected
    property Parser : TParserBase read fParser;
    procedure UpdateData(AData : Pointer);
  public
    procedure CheckSyntax(fCodeBuffer: TCodeBuffer); override;
    constructor Create(AParser : TParserBase);
    procedure WriteErrors(WMsg: TIDEMessagesWindowInterface; Caption: string);
  end;

  { TPasFlyCheck }

  TPasFlyCheck = class
  private
    fCodeStamp : Int64;
    Timer : TTimer;
    fCodeBuffer : TCodeBuffer;
    fSyntaxCheckTool : TSyntaxChecker;
    procedure SetCodeBuffer(AValue: TCodeBuffer);
    procedure InitSyntaxCheckTool;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckSyntax(Sender: TObject);
    property CodeBuffer : TCodeBuffer read FCodeBuffer write SetCodeBuffer;
  end;

implementation

var PasFlyCheck : TPasFlyCheck = nil;

procedure ClearCustomMessages(View: TIDEMessagesWindowInterface; Caption : string);
var V : TExtToolView;
begin
  V:=View.GetView(Caption,false);
  if (V=nil) or (V.Lines.Count=0) then exit;
  V.Lines.Clear;
  View.DeleteView(V);
  //debugln('Cleared');
end;

{ TPasSyntaxParser }

procedure TPasSyntaxParser.WriteErrorsToMsgView(View: TIDEMessagesWindowInterface;
  Caption: string);

    procedure WriteError(Error :TParserErrorDesc);
    begin
      //debugln('WriteError:: ', Error.Message);
      View.AddCustomMessage(mluFatal,
      Error.Message,
      Error.Filename,
      Error.Line,
      Error.Column,
      Caption
    );
    end;



var i:integer;
begin
  ClearCustomMessages(View, Caption);
  for i := 0 to fTool.ErrorCount - 1 do begin
    WriteError(fTool.Errors[i]);
  end;
end;

procedure TPasSyntaxParser.Parse;
begin
  fTool.StoreErrors:= true;
  try
    fTool.CheckSyntax;
  except
    on e:Exception do begin
        debugln('CheckOnTheFly :: ',e.Message);
    end;
  end;
  fTool.StoreErrors:= false;
  //debugln(fTool.Src)
  //fTool.WriteErrors;
end;

procedure TPasSyntaxParser.InitCodeTool(fCodeBuffer: TCodeBuffer);
begin
  if fTool = nil then
    fTool := TPascalExprCheckerTool.Create;
  CodeToolBoss.InitCurCodeTool(fCodeBuffer);
  fTool.Scanner := CodeToolBoss.CurCodeTool.Scanner;
  fTool.ClearErrors;

end;

procedure TPasSyntaxParser.UpdateData(AData: Pointer);
begin
  //debugln('update buffer');
  InitCodeTool(TCodeBuffer(AData));
end;

constructor TPasSyntaxParser.Create(ACodeBuffer: TCodeBuffer);
begin
  UpdateData(ACodeBuffer);
  inherited Create;
end;

procedure TSyntaxChecker.CheckSyntax(fCodeBuffer : TCodeBuffer);
begin
  Parser.UpdateData(fCodeBuffer);
  Parser.Parse;
end;

constructor TSyntaxChecker.Create(AParser: TParserBase);
begin
  fParser := AParser;
end;

procedure TSyntaxChecker.WriteErrors(WMsg: TIDEMessagesWindowInterface; Caption : string);
begin
  Parser.WriteErrorsToMsgView(WMsg, Caption);
end;

procedure TSyntaxChecker.UpdateData(AData: Pointer);
begin
  fParser.UpdateData(AData);
end;

{ TPasFlyCheck }

procedure TPasFlyCheck.SetCodeBuffer(AValue: TCodeBuffer);
begin
  if FCodeBuffer=AValue then Exit;
  FCodeBuffer:=AValue;
  InitSyntaxCheckTool;
  fSyntaxCheckTool.UpdateData(fCodeBuffer);
end;

procedure TPasFlyCheck.InitSyntaxCheckTool;
begin
  if fSyntaxCheckTool <> nil then exit;
  fSyntaxCheckTool := TSyntaxChecker.Create(TPasSyntaxParser.Create(fCodeBuffer));
end;

constructor TPasFlyCheck.Create;
begin
  Timer := TTimer.Create(nil);
  Timer.OnTimer := @CheckSyntax;
  //debugln('Fly Initialized');
end;

destructor TPasFlyCheck.Destroy;
begin
  inherited Destroy;
  Timer.Destroy;
end;

procedure TPasFlyCheck.CheckSyntax(Sender: TObject);
var    Code : TCodeBuffer;
    SrcEdit : TSourceEditorInterface;
       Stamp: Int64;
begin
  //debugln('timer');
  if SourceEditorManagerIntf = nil then exit;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit = nil then exit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if (Code <> FCodeBuffer) then begin
    SetCodeBuffer(Code);
    ClearCustomMessages(IDEMessagesWindow, 'CheckOnTheFly');
  end;
  Stamp := Code.CodeCache.ChangeStamp;
  //debugln('get stamp');
  if Stamp = fCodeStamp then exit;
  fCodeStamp := Stamp;
  //debugln('set stamp');
  //debugln('check syntax');
  fSyntaxCheckTool.CheckSyntax(fCodeBuffer);
  //debugln('writeerrors');
  fSyntaxCheckTool.WriteErrors(IDEMessagesWindow, 'CheckOnTheFly');
end;

initialization

  PasFlyCheck := TPasFlyCheck.Create;

finalization
  PasFlyCheck.Destroy;

end.



