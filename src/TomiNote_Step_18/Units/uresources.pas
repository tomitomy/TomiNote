unit uResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var

  AppFullName              : string;
  AppName                  : string;
  AppDir                   : string;
  ScriptFile               : string;

const

  AppTitle                 = 'TomiNote';
  AppVersion               = '1.1';

  BrightThemeID            = 1;
  DarkThemeID              = 2;

  DefFontSize              = 12;
  DefExpandSignSize        = 9;

  DefTreeBarPercent        = 25;
  DefRecyBarPercent        = 30;
  DefInfoBarPercent        = 30;

  DefBrightForeColor       = $212121;
  DefBrightBackColor       = $F7F7F7;
  DefDarkForeColor         = $D3D7CF;
  DefDarkBackColor         = $2E3436;

  DBFileExt                = '.tdb';

  EmptyRecyIconID          = 40;
  FullRecyIconID           = 41;

  LockFileExt              = '.tominote_lock';

  DefSearchCountLimit      = 50000;
  DefRecentCountLimit      = 10;

resourcestring

  Res_CaptionOK            = 'OK(&O)';
  Res_CaptionCancel        = 'Cancel(&C)';

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

  Res_CaptionSearch        = 'Search(&S)';
  Res_CaptionReplace       = 'Replace(&R)';

  Res_MultiReplaceTip      = 'Multiple nodes will be changed. Do you want to continue?';

  Res_SearchResultIndex    = 'Search Result: %d / %d';
  Res_ScarchResultPart     = 'Part %d [ %s | %ds ]';
  Res_ScarchResultTotal    = 'Total %d [ %s | %ds ]';

  Res_CaptionImport        = 'Import(&I)';
  Res_CaptionExport        = 'Export(&E)';

  Res_DialogFilterTXT      = 'Text File|*.txt|All File|*.*';
  Res_AutoSaveInfo         = '[ Save: %dm | Backup: %dm ]';

  Res_RecentFileNotExists  = 'The file doesn''t exists. Do you want to remove the menu item?';
  Res_DBIsLocked           = 'Database is locked. Do you want to force open it?';
  Res_BackupDBFail         = 'Failed to backup the database!';

  Res_CaptionExecute       = 'Execute(&E)';

  Res_RenameWarning        = 'Multiple nodes will be renamed and can''t be undone. Do you want to continue?';

  Res_CaptionEditScript    = 'Edit Script(&S)';
  Res_CaptionEndEdit       = 'End Edit(&S)';

  Res_OptionsTabsItems     = 'General'#10'Layout'#10'Theme'#10'Other';

  Res_HideBarsItems        = 'MenuBar'#10'ToolBar'#10'StatBar'#10'TreeBar'#10'RecyBar'#10'InfoBar';
  Res_KeepWindowSizeItems  = 'Main'#10'Search'#10'Import'#10'Export'#10'Options'#10'Utils';

  Res_TextSearch           = 'Search';

  Res_SearchFromItems      = 'Selected node'#10'Selected branch'#10'All nodes';
  Res_SearchInItems        = 'Node Name'#10'Node Note';

  Res_ImportFromItems      = 'From file     '#10'From directory'#10'From database ';
  Res_ImportToItems        = 'To prev node'#10'To first child'#10'To next node'#10'To last child';

  Res_ExportFromItems      = 'Selected Node  '#10'Selected Branch'#10'All Nodes      ';
  Res_ExportToItems        = 'To file     '#10'To directory'#10'To database ';

  Res_IgnoreCase           = 'Ignore Case';
  Res_MultiLine            = 'Multi-Line';
  Res_NonGreedy            = 'Non-Greedy';

  Res_SortOfItems          = 'Sibling'#10'Children';
  Res_SortDirectionItems   = 'Ascending'#10'Descending';

  Res_TitleOpenDlg         = 'Open File';
  Res_TitleSaveDlg         = 'Save File';
  Res_TitleSelDirDlg       = 'Select Directory';

  Res_LangList             = 'af=Afrikaans|am=Amharic|ar=Arabic|ar_ae=Arabic(United Arab Emirates)|ar_bh=Arabic(Bahrain)|ar_dz=Arabic(Algeria)|ar_eg=Arabic(Egypt)|ar_iq=Arabic(Iraq)|ar_jo=Arabic(Jordan)|ar_kw=Arabic(Kuwait)|ar_lb=Arabic(Lebanon)|ar_ly=Arabic(Libya)|ar_ma=Arabic(Morocco)|ar_om=Arabic(Oman)|ar_qa=Arabic(Qatar)|ar_sa=Arabic(Saudi Arabia)|ar_sy=Arabic(Syria)|ar_tn=Arabic(Tunisia)|ar_ye=Arabic(Yemen)|as=Assamese|az=Azeri|az_az=Azeri(Cyrillic)|be=Belarusian|bg=Bulgarian|bn=Bengali|bo=Tibetan|bs=Bosnian|ca=Catalan|cs=Czech|cy=Welsh|da=Danish|de=German|de_at=German(Austria)|de_ch=German(Switzerland)|de_de=German(Germany)|de_li=German(Liechtenstein)|de_lu=German(Luxembourg)|dv=Maldivian|el=Greek|en=English|en_au=English(Australia)|en_bz=English(Belize)|en_ca=English(Canada)|en_cb=English(Caribbean)|en_gb=English(Great Britain)|en_ie=English(Ireland)|en_in=English(India)|en_jm=English(Jamaica)|en_nz=English(New Zealand)|en_ph=English(Philippines)|en_tt=English(Trinidad)|en_us=English(United States)|en_za=English(Southern Africa)|es=Spanish|es_ar=Spanish(Argentina)|es_bo=Spanish(Bolivia)|es_cl=Spanish(Chile)|es_co=Spanish(Colombia)|es_cr=Spanish(Costa Rica)|es_do=Spanish(Dominican Republic)|es_ec=Spanish(Ecuador)|es_es=Spanish(Traditional)|es_gt=Spanish(Guatemala)|es_hn=Spanish(Honduras)|es_mx=Spanish(Mexico)|es_ni=Spanish(Nicaragua)|es_pa=Spanish(Panama)|es_pe=Spanish(Peru)|es_pr=Spanish(Puerto Rico)|es_py=Spanish(Paraguay)|es_sv=Spanish(ElSalvador)|es_uy=Spanish(Uruguay)|es_ve=Spanish(Venezuela)|et=Estonian|eu=Basque|fa=Farsi|fi=Finnish|fo=Faroese|fr=French|fr_be=French(Belgium)|fr_ca=French(Canada)|fr_ch=French(Switzerland)|fr_fr=French(France)|fr_lu=French(Luxembourg)|ga=Irish|gd=Gaelic(Scotland)|gd_ie=Gaelic(Ireland)|gl=Galician|gn=Guarani(Paraguay)|gu=Gujarati|he=Hebrew|hi=Hindi|hr=Croatian|hu=Hungarian|hy=Armenian|id=Indonesian|is=Icelandic|it=Italian|it_ch=Italian(Switzerland)|it_it=Italian(Italy)|ja=Japanese|ka=Georgian|kk=Kazakh|km=Khmer|kn=Kannada|ko=Korean|ks=Kashmiri|la=Latin|lo=Lao|lt=Lithuanian|lv=Latvian|mi=Maori|mk=FYRO Macedonia|ml=Malayalam|mn=Mongolian|mr=Marathi|ms=Malay|ms_bn=Malay(Brunei)|ms_my=Malay(Malaysia)|mt=Maltese|my=Burmese|nb=Norwegian(Bokml)|ne=Nepali|nl=Dutch|nl_be=Dutch(Belgium)|nl_nl=Dutch(Netherlands)|no=Norwegian|or=Oriya|pa=Punjabi|pl=Polish|pt=Portuguese|pt_br=Portuguese(Brazil)|pt_pt=Portuguese(Portugal)|rm=Raeto(Romance)|ro=Romanian|ro_mo=Romanian(Moldova)|ru=Russian|ru_mo=Russian(Moldova)|sa=Sanskrit|sb=Sorbian|sd=Sindhi|si=Sinhalese|sk=Slovak|sl=Slovenian|so=Somali|sq=Albanian|sr=Serbian(Latin)|sr_sp=Serbian(Cyrillic)|sv=Swedish|sv_fi=Swedish(Finland)|sv_se=Swedish(Sweden)|sw=Swahili|sz=Sami(lappish)|ta=Tamil|te=Telugu|tg=Tajik|th=Thai|tk=Turkmen|tn=Setswana|tr=Turkish|ts=Tsonga|tt=Tatar|uk=Ukrainian|ur=Urdu|uz=Uzbek(Latin)|uz_uz=Uzbek(Cyrillic)|ve=Venda|vi=Vietnamese|xh=Xhosa|yi=Yiddish|zh=Chinese|zh_cn=Chinese(China)|zh_hk=Chinese(Hong Kong SAR)|zh_mo=Chinese(Macau SAR)|zh_sg=Chinese(Singapore)|zh_tw=Chinese(Taiwan)|zu=Zulu';

implementation

initialization

  AppFullName              := ParamStr(0);
  AppName                  := ExtractFileName(AppFullName);
  AppDir                   := ExtractFilePath(AppFullName);

end.

