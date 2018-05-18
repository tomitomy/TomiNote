unit uResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var

  AppFullName              : string;
  AppName                  : string;
  AppDir                   : string;

const

  AppTitle                 = 'TomiNote';
  AppVersion               = '1.1';

  DBFileExt                = '.tdb';

resourcestring

  Res_DBVersionError       = 'Database version error!';

  Res_CreateDBFail         = 'Failed to create the database!';
  Res_OpenDBFail           = 'Failed to open the database!';
  Res_SaveDBFail           = 'Failed to save the database!';
  Res_CloseDBFail          = 'Failed to close the database!';

  Res_OverwriteFileTip     = 'The file already exists. Do you want to overwrite it?';
  Res_OverwriteFileFail    = 'Failed to overwrite the file!';

  Res_SaveDataTip          = 'The data has changed. Do you want to save it?';
  Res_DeleteNodeTip        = 'The node can''t be recovered after delete. Do you want to continue?';
  Res_EmptyRecyclerTip     = 'The nodes can''t be recovered after empty. Do you want to continue?';

  Res_DialogFilterTDB      = 'TomiNote File|*.tdb|All File|*.*';
  Res_UnnamedNode          = 'Unnamed Node';
  Res_ChildNodesInfo       = 'Child Nodes: %d';

implementation

initialization

  AppFullName              := ParamStr(0);
  AppName                  := ExtractFileName(AppFullName);
  AppDir                   := ExtractFilePath(AppFullName);

end.

