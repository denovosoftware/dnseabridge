unit DNSEABridgeRScriptRunnerUnit;

{***************************************************************************************************
This De Novo Software External Application Bridge accepts data via TCP/IP and
converts it into a format required for the opaR library.

Copyright (C) 2017 De Novo Software

This program is free software: you can redistribute it and/or modify it under the terms of
the GNU General Public License as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.
If not, see <http://www.gnu.org/licenses/>.

***************************************************************************************************}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.UITypes,

  {opaR}
  opaR.Engine,
  opaR.Interfaces,

  {De Novo Software External Application Bridge}
  ExternalEvaluatorClassesUnit;

const
  CRLF = #13#10;

type
  TOnNeedsToAddErrorMessage = procedure(const aErrorMessage: string; aResult:
      TExternalEvaluatorResult; const aException: Exception = nil) of object;

  TDNSEABridgeRScriptRunner = class(TComponent)
  strict private
    FEngine: IREngine;
    FOnNeedsToAppendErrorMessage: TOnNeedsToAddErrorMessage;
    procedure DoAppendErrorMessage(const aErrorMessage: string; aResult:
        TExternalEvaluatorResult; const aException: Exception = nil);
    function GetS4Results(const aInput: TExternalEvaluatorInput; aResultData:
        TExternalEvaluatorResult): IS4Object;
    function ValidateS4Results(const aNumNewParams: integer; aNames:
        TArray<string>; const aNumElements: integer): boolean;
  public
    procedure SetupREngine;
    function CurrentVersionOfRdll: string;
    procedure PerformClustering(const aInput: TExternalEvaluatorInput; aResultData:
        TExternalEvaluatorResult);
    procedure PerformNewParam(const aInput: TExternalEvaluatorInput; aResultData:
        TExternalEvaluatorResult);
    procedure PerformAutoGating(const aInput: TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult);
    property OnNeedsToAppendErrorMessage: TOnNeedsToAddErrorMessage read
        FOnNeedsToAppendErrorMessage write FOnNeedsToAppendErrorMessage;
  end;

implementation

uses
  opaR.EngineExtension,
  opaR.NativeUtility;

function TDNSEABridgeRScriptRunner.CurrentVersionOfRdll: string;
var
  versionAsPAnsiChar: PAnsiChar;
begin
  versionAsPAnsiChar := FEngine.Rapi.DLLVersion;
  Result := string(versionAsPAnsiChar);
end;

procedure TDNSEABridgeRScriptRunner.DoAppendErrorMessage(const aErrorMessage:
    string; aResult: TExternalEvaluatorResult; const aException: Exception =
    nil);
begin
  if Assigned(OnNeedsToAppendErrorMessage) then
    OnNeedsToAppendErrorMessage(aErrorMessage, aResult, aException);
end;

function TDNSEABridgeRScriptRunner.GetS4Results(const aInput:
    TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult): IS4Object;
var
  fileName : String;
  fn1: IRFunction;
  expr: ISymbolicExpression;
  newMat: INumericMatrix;
  paramCntr: integer;
  setParamNamesDef: TStringBuilder;
  ParamNameOrientation: String;
  matSymbolName: String;
  savedExceptionMask: TArithmeticExceptionMask;
begin
  if not Assigned(FEngine) then
  begin
    DoAppendErrorMessage ('R is not Installed.', aResultData);
    result := nil;
    exit;
  end;

  // catch any exceptions coming from R
  try
    newMat := FEngine.CreateNumericMatrix(aInput.InputMatrix);

    // Choose to set the col/row names based on the matrix orientation.
    if (aInput.EventsAsRows) then
      ParamNameOrientation := 'colnames'
    else
      ParamNameOrientation := 'rownames';

    // Set the variable inside R so we can run a script on it.
    matSymbolName := 'mat';
    FEngine.SetSymbol(matSymbolName, newMat as ISymbolicExpression);

    // Make R script to add row or column names;
    setParamNamesDef := TStringBuilder.Create;
    setParamNamesDef.Append(ParamNameOrientation);
    setParamNamesDef.Append('(');
    setParamNamesDef.Append(matSymbolname);
    setParamNamesDef.Append(') <- c(');

    // Add in names with quoted text.
    for paramCntr := 0 to aInput.ParameterNames.Count - 1 do
    begin
      setParamNamesDef.Append(QuotedStr(aInput.ParameterNames[paramCntr]));
      setParamNamesDef.Append(', ')
    end;

    // Remove the trailing comma.
    setParamNamesDef.Length := setParamNamesDef.Length - 2;

    // Finish up the text
    setParamNamesDef.Append(')');

    // Set the row/column names
    FEngine.Evaluate(setParamNamesDef.ToString);

    setParamNamesDef.Free;

    // Get the matrix from R since we are going to pass it into our execute function.
    newMat := FEngine.GetSymbol(matSymbolName).AsNumericMatrix;

    // R expects forward slashes not back slashes.
    fileName := StringReplace(aInput.ScriptFileName, '\', '/', [rfReplaceAll]);
    FEngine.Evaluate('source(' + QuotedStr(fileName) + ')');
    fn1 := FEngine.Evaluate('Execute').AsFunction;

    try
      // Save the current exception mask
      // Add exceptions to the mask for floating point operations. The R.dll does
      // not handle these exceptions gracefully, and we want the behavior in the
      // script to replicate the behavior seen when running R from the R GUI
      savedExceptionMask := System.Math.GetExceptionMask;
      System.Math.SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                                      exOverflow, exUnderflow, exPrecision]);

      // Execute the R script file with the data supplied from the
      // TExternalEvaluatorInput
      expr := fn1.Invoke(newMat as ISymbolicExpression);
      result := expr.AsS4;
    finally
      // We now want to reset the exception mask to the state it was before we
      // ran the R script. Also, clear and raise any exceptions that may have
      // been thrown during the execution of the R script.
      System.Math.SetExceptionMask(savedExceptionMask);
      ClearExceptions(True, [exInvalidOp, exDenormalized, exZeroDivide,
                                      exOverflow, exUnderflow, exPrecision]);
    end;

  except
    on e: Exception do
    begin
      DoAppendErrorMessage('Error running R Script:' + CRLF + e.Message, aResultData, e);
      result := nil;
    end;
  end;
end;

procedure TDNSEABridgeRScriptRunner.PerformClustering(const aInput:
    TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult);
var
 s4Results: IS4Object;
 clusterNames: Tarray<string>;
 numberOfClusters: integer;
 clusterAssignments: Tarray<integer>;
 newClusterResultData: TClusterAssignmentResultData;
begin
  s4Results := GetS4Results(aInput, aResultData);

  if (s4Results <> nil) then
  begin
    clusterAssignments := s4Results['clusterAssignments'].AsInteger.ToArray;
    numberofClusters := s4Results['numberOfClusters'].AsInteger.First;
    clusterNames := s4Results['clusterNames'].AsCharacter.ToArray;

    if ValidateS4Results(numberOfClusters, clusterNames, Length(clusterAssignments)) then
    begin
      newClusterResultData:= TClusterAssignmentResultData.Create(nil);
      newClusterResultData.ParamName:= IntToStr(numberOfClusters) + ' ClusterAssignments';
      newClusterResultData.Data:= clusterAssignments;
      newClusterResultData.ClusterNames.AddStrings(clusterNames);
      newClusterResultData.NumOfClusters:=numberofClusters;
      aResultData.AddResultData(newClusterResultData);
      aResultData.Status := EV_STATUS_OK;
    end
    else
      DoAppendErrorMessage(Format('Number of clusters did not match results: '+
        CRLF + 'Number of Clusters: %d, Number of Names: %d', [numberOfClusters,
        length(clusterNames)]), aResultData);
  end
  else
    DoAppendErrorMessage('Cluster result data is not valid.', aResultData);
end;

procedure TDNSEABridgeRScriptRunner.PerformNewParam(const aInput:
    TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult);
var
  cntr: Integer;
  s4Results: IS4Object;
  numberOfNewParams: integer;
  newParamNames: Tarray<string>;
  newColumns: TDoubleMatrix;
  newParamResultData: TnewParameterResultData;
begin
  s4Results := GetS4Results(aInput, aResultData);

  if (s4Results <> nil) then
  begin
    newColumns:= s4Results['newParamData'].AsNumericMatrix.GetArrayFast;
    numberOfNewParams:= s4Results['numberOfNewParams'].AsInteger.First;
    newParamNames:= s4Results['newParamNames'].AsCharacter.ToArray;

    if ValidateS4Results(numberOfNewParams, newParamNames, length(newColumns)) then
    begin
      for cntr := 0 to numberOfNewParams -1 do
      begin
        newParamResultData:= TNewParameterResultData.Create(nil);
        newParamResultData.ParamName:= newParamNames[cntr];
        newParamResultData.Data:= newColumns[cntr];
        aResultData.AddResultData(newParamResultData);
        aResultData.Status := EV_STATUS_OK;
      end
    end
    else
      DoAppendErrorMessage(Format('Number of new parameters did not match results: '+
        CRLF + 'Number of New Parameters: %d, Number of Names: %d', [numberOfNewParams,
        length(newParamNames)]), aResultData);
  end
  else
    DoAppendErrorMessage('Parameter result data invalid.', aResultData);

end;

procedure TDNSEABridgeRScriptRunner.PerformAutoGating(const aInput: TExternalEvaluatorInput; aResultData: TExternalEvaluatorResult);
var
  s4Results: IS4Object;
  gatingMl: string;
  resData: TAutoGatingExternalEvaluatorResultData;
begin
  s4Results := GetS4Results(aInput, aResultData);

  if (s4Results <> nil) then
  begin
    gatingMl := s4Results['gatingML'].AsCharacter.First;

    if gatingMl <> '' then
    begin
      resData := TAutoGatingExternalEvaluatorResultData.create(nil);
      resData.gatingMl := gatingMl;
      aResultData.AddResultData(resData);
      aResultData.Status := EV_STATUS_OK;
    end
    else
      DoAppendErrorMessage('Gating ML was not returned', aResultData);
  end
  else
    DoAppendErrorMessage('Result data invalid.', aResultData);

end;

procedure TDNSEABridgeRScriptRunner.SetupREngine;
begin
  TREngine.SetEnvironmentVariables;
  FEngine := TREngine.GetInstance;
end;

function TDNSEABridgeRScriptRunner.ValidateS4Results(const aNumNewParams:
    integer; aNames: TArray<string>; const aNumElements: integer): boolean;
begin
  result := (aNumNewParams = length(aNames));
end;

end.
