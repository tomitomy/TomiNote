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

  Res_AboutTitle           = 'About';
  Res_HelpTitle            = 'Help';

  Res_AboutContent         = #10 +
    'TomiNote is a simple note collection tool (This program only supports characters in the range of "Unicode UCS-2").'#10 +
    #10 +
    'License: This program has no license (This means it belongs to the public domain), it is free for everyone. If you use the source code of this program, you only need to pay attention to the license of Lazarus and the license of the icon files.'#10 +
    #10 +
    'The preparation of this program is purely personal hobby, this program does not provide any guarantee, I am not responsible for the loss caused by the use of the program.'#10 +
    #10 +
    'You can get the source code of this program here:'#10 +
    'https://github.com/tomitomy/TomiNote'#10 +
    #10 +
    '------------------------------'#10 +
    #10 +
    'This program was written using Lazarus:'#10 +
    'http://www.lazarus-ide.org'#10 +
    #10 +
    'The License of Lazarus is GPL/LGPL. See Lazarus and Free Pascal sources for license details.'#10 +
    #10 +
    'Thanks to Lazarus Team for designing such good programming software and sharing it to everyone! Thanks to the members of Lazarus Forum for their help!'#10 +
    #10 +
    '------------------------------'#10 +
    #10 +
    'The icons used in this program are download from FatCow:'#10 +
    'http://www.fatcow.com/free-icons'#10 +
    #10 +
    'The License of the Icons is "CCBY 3.0":'#10 +
    'https://creativecommons.org/licenses/by/3.0/'#10 +
    #10 +
    'I modified some of the icons to suit the needs of my program.'#10 +
    #10 +
    'Thanks to FatCow for designing so many good icons and sharing them to everyone!'#10 +
    #10 +
    #10 +
    #10;

  Res_HelpContent          = #10 +
    '[ --- Note --- ]'#10 +
    #10 +
    'This program only supports characters in the range of "Unicode UCS-2" (0000 ~ FFFF). If there are characters beyond this range in your database, the function of "History" and "Search or Replace" will be abnormal.'#10 +
    #10 +
    'The database engine used by this program is SQLite3, you need to install SQLite3 in order to use this program normally, or you can put the SQLite3 dynamic link library file (sqlite3.dll in Windows or libsqlite3.so in Linux) into the "lib" directory of the program''s directory.'#10 +
    #10 +
    'All configuration information for this program is saved in the ".ini" file with the same name as the program, you can delete the configuration file to make the program restore the default settings, configuration file is in the ''config'' directory of the program''s directory.'#10 +
    #10 +
    'This program uses dynamic load node, when you open a database, it will not load all the nodes, only the first depth nodes will be loaded, when you expand a node, its child nodes will be loaded, so you don''t have to worry about loading too much data too slow, but the count of nodes should not exceed 2147483640, which is determined by the type of integer used in the program, you can modify the source code to make it support more the number of nodes, but also consume more database space.'#10 +
    #10 +
    'You can use the command line parameter --c or --config to specify the directory of the configuration file, such as: TomiNote --config=~/.config/TomiNote'#10 +
    #10 +
    'You can use the command line parameter --l or --lang to specify the directory of the language files, such as: TomiNote --lang=/usr/share/TomiNote/languages'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Move And Copy Node --- ]'#10 +
    #10 +
    'You can use Move Up or Move Down functions to move the selected node, if you move up the first node, the node will be moved to the end, if you move down the last node, the node will be moved to the head.'#10 +
    #10 +
    'You can use Move Left or Move Right to move the selected node, then the node looks like it is moved left or right. Note that if the node is in the Expanded state, then after the Move Right, all its child nodes will be moved to behind itself. If the node is in the Collapsed state, its child nodes will remain (this is more in line with what you would like to see).'#10 +
    #10 +
    'You can also drag and drop the node as follows:'#10 +
    #10 +
    #10 +
    #10 +
    'Move To Target Front:'#10 +
    'Clrl + Shift + Drag -> Release Mouse Button -> Release Ctrl and Shift Key'#10 +
    #10 +
    'Move To Target Behind:'#10 +
    'Ctrl + Drag -> Release Mouse Button -> Release Ctrl Key'#10 +
    #10 +
    'Move To Target First Child:'#10 +
    'Clrl + Shift + Drag -> Release Ctrl and Shift Key -> Release Mouse Button'#10 +
    #10 +
    'Move To Target Last Child:'#10 +
    'Clrl + Drag -> Release Ctrl Key -> Release Mouse Button'#10 +
    #10 +
    #10 +
    #10 +
    'Copy To Target Front:'#10 +
    'Alt + Shift + Drag -> Release Mouse Button -> Release Alt and Shift Key'#10 +
    #10 +
    'Copy To Target Behind:'#10 +
    'Alt + Drag -> Release Mouse Button -> Release Alt Key'#10 +
    #10 +
    'Copy To Target First Child:'#10 +
    'Alt + Shift + Drag -> Release Alt and Shift Key -> Release Mouse Button'#10 +
    #10 +
    'Copy To Target Last Child:'#10 +
    'Alt + Drag -> Release Alt Key -> Release Mouse Button'#10 +
    #10 +
    #10 +
    #10 +
    'Drag and Drop has a feature: if you only use the Ctrl or Alt keys to drag and drop (without the Shift key), then if you are dragging forward, you can drag the node to the front of the target, if you are dragging backward, you can drag the node to the behind of the target. E.g:'#10 +
    #10 +
    #10 +
    #10 +
    'Move To Target Front:'#10 +
    'Ctrl + Drag(Move Forward) -> Release Mouse Button -> Release Ctrl Key'#10 +
    #10 +
    'Move To Target Behind:'#10 +
    'Ctrl + Drag(Move Backward) -> Release Mouse Button -> Release Ctrl Key'#10 +
    #10 +
    #10 +
    #10 +
    'Copy To Target Front:'#10 +
    'Alt + Drag(Move Forward) -> Release Mouse Button -> Release Alt Key'#10 +
    #10 +
    'Copy To Target Behind:'#10 +
    'Alt + Drag(Move Backward) -> Release Mouse Button -> Release Alt Key'#10 +
    #10 +
    #10 +
    #10 +
    'You can drag node from the Directory Tree and drop into the Recycler, or drag from the Recycler and drop into the Directory Tree.'#10 +
    #10 +
    'Note: If your system does not support "Alt + drag" operation, you can use "Ctrl + Alt + drag" to copy the node.'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Recent Files --- ]'#10 +
    #10 +
    'The recent files will listed in the File menu. The menu order in this list will not change, to facilitate you to quickly find the last opened file. If you open more than 10 files, the oldest opened file item will be removed from the list. If you want to remove a recent file item, you can hold down the Shift key and then click the recent file item.'#10 +
    #10 +
    'All Drop-Down Lists in this program are supports press the Shift key to delete item also. such as "recent search string list" or "recent replace string list".'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Save And Backup --- ]'#10 +
    #10 +
    'The Auto Save Timer will start after opening the file, and will reset after saving the file.'#10 +
    #10 +
    'You can change the Auto Save Timer interval in the Options dialog box. If set to 0, Auto Save is disabled.'#10 +
    #10 +
    'The Auto Save function checks the remaining time every minute, and if the remaining time is 0, Auto Save is performed and the remaining time is reset.'#10 +
    #10 +
    #10 +
    #10 +
    'The Auto Backup Timer will start after the first time save the file. If you never saved the file after last backup, the Auto Backup Timer will not start.'#10 +
    #10 +
    'You can change the Auto Backup Timer interval in the Options dialog box. If set to 0, Auto Backup is disabled.'#10 +
    #10 +
    'You can change the maximum count of Auto Backup files in the Options dialog box. If set to 0, Auto Backup is disabled.'#10 +
    #10 +
    'The Auto Backup function checks the remaining time every minute, and if the remaining time is 0, Auto Backup is performed and the remaining time is reset.'#10 +
    #10 +
    'When you close the database, the remaining time of Auto Backup will store to the database.'#10 +
    #10 +
    'You can see the remaining time for Auto Save and Auto Backup in the Status Bar.'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- History Record --- ]'#10 +
    #10 +
    'This program will keep the History of all nodes, including the history of all Replace and Script operations, you can Undo or Redo any node at any time.'#10 +
    #10 +
    'When you Undo or Redo through the Toolbar button, the changes in the note will be selected so that you can know what is changed. If you Undo or Redo via a shortcut, nothing will be selected.'#10 +
    #10 +
    'You can limit the maximum capacity and minimum count of single node''s history in the Options dialog box. If you set it to 0, there is no limit. the history will give priority to the Minimum Count limit, and secondly will consider the Maximum Capacity limit.'#10 +
    #10 +
    'You can discard the history to free up memory. You can do this in the Options dialog box.'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Search And Replace --- ]'#10 +
    #10 +
    'You can search for text in Node Name and Node Content. You can search for text in Selected Node or Selected Branch or Whole Tree.'#10 +
    #10 +
    'In the Search dialog box, after entering Search String or Replace String, you can also perform Search or Replace operation with "Ctrl + Enter".'#10 +
    #10 +
    'The Search Results or Replace Results are listed in the Information Bar. You can double-click the entry in the Information Bar to jump to the corresponding location of the search results, or browse the search results in turn by using the F3 or F4 key.'#10 +
    #10 +
    'The maximum number of search results is limited to 50000 because too many search results will cause the Information Bar load slowly. You can modify this value in the Options dialog box. If it is set to 0, there is no limit.'#10 +
    #10 +
    'Search and Replace are performed in a separate thread, and you can abort the Search or Replace process at any time by clicking the Stop button in the Toolbar.'#10 +
    #10 +
    #10 +
    #10 +
    'The Carriage Return Character and the NewLine Character in Search Box and Replace Box will be converted to \r \n, the Tab Character will be converted to \t, the Backslash Character will not be processed, only "the separate backslash in front of special character" will be converted to \\. For example:'#10 +
    #10 +
    '''\a\\''#10 will be converted to ''\a\\\n'' (the double backslash in front of special character #10 will be ignored),'#10 +
    #10 +
    '''\a\''#10 will be converted to''\a\\\n'' (the separate backslash in front of special character #10 will be converted).'#10 +
    #10 +
    'If you perform a Search or Replace operation, the "\r \n \t \\ \xFF" in the Search String or Replace String will be converted to "Carriage Return, NewLine, Tab, Backslash, and Ascii Character Char($FF)". \x will not be converted if the two characters after \x are not hexadecimal characters.'#10 +
    #10 +
    #10 +
    #10 +
    'The Name Filter option in the Search dialog box is used to specify the name of the node to be filtered. The filter condition uses regular expression syntax. Only nodes whose name matches the filter condition perform Search or Replace operations.'#10 +
    #10 +
    #10 +
    #10 +
    'Note: The Search and Replace operations are taken the content from the database to process, and the Line-Ending used in the database is \n, so you can never find the \r character.'#10 +
    #10 +
    'Note: If the pasted content contains Line-Ending character that do not match the current system, you can convert the Line-Ending to the current system''s Line-Ending by "switching to other node and then switching back". The wrong Line-Ending will cause the search result to display incorrectly.'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Import And Export --- ]'#10 +
    #10 +
    'You can import data from the Text File or Directory or Database File to the current database, you can also export the Node or the Branch or the Whole Tree in the current database to Text File or Directory or Database File.'#10 +
    #10 +
    'The "Export To Database" function will compress the fragmented space in the database.'#10 +
    #10 +
    'If you export the data to an existing database file, the original data in the target file will not be overwritten, and the new data will be append to the end of the target file.'#10 +
    #10 +
    'Note: The files to be imported only supports the text file of the UTF8 format.'#10 +
    #10 +
    'Note: The Save As operation is implemented using the "Export To Database" function. The Save As operation will delete the target file first, without retaining the data in the target file.'#10 +
    #10 +
    'Note: The Backup function is implemented using the "Export To Database" function, the Backup funciton will backup the nodes in the Directory Tree and Recycler.'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Utils --- ]'#10 +
    #10 +
    'The Sort tool can sort sibling nodes or subnodes.'#10 +
    #10 +
    #10 +
    #10 +
    'The Split tool can split one note into multiple subnodes.'#10 +
    #10 +
    'In the Split tool, the Separator String is the symbol that used to split the note, its using the regular expression syntax, the program will search for the Sparator String in the note, then split the text before and after the Separator String into two parts, then save them to the different child nodes. the Title String is also use the regular expression syntax, the program will search the Title String in the splitted text, then the Title String will be used as the name of the child node. If the Include Separator option is checked, the Separator String will be include in the splitted text, otherwise it will not be include. If the "Add Prefix Number" option is checked, a serial number will be added to the beginning of the subnode name, the length of the number is specified in the edit box which follow the option.'#10 +
    #10 +
    #10 +
    #10 +
    'The Rename tool can search for a string in the node name or the note, then replace it with the specified string, and then rename the node using the replaced result.'#10 +
    #10 +
    'In the Rename tool, the Search String and Replace String must use regular expression. The Rename operation will be applied to the current node and its descendant nodes, if a node''s search result is empty, it will be ignored, it will not be renamed. If you checked the "Only affect at specified depth" option, only nodes with a specified depth will be renamed, and nodes not belonging to this depth will be ignored.'#10 +
    #10 +
    #10 +
    #10 +
    'The Script tool can perform Multi-Replace on multiple nodes. You can edit the script in the Script tool, or you can use the Main Program to open the script database directly for editing. The script database is placed in the "config" directory and the file name is "script.tdb".'#10 +
    #10 +
    'The Name Filter option in the Script tool is used to specify the node name to be filtered. The filter condition uses regular expression syntax. Only nodes whose names match the filter condition execute the Script.'#10 +
    #10 +
    'You can see the Script menu in the popup menu of the Note Editor, which lists all the script names. You can use it to perform script operations on the Current Note or on the Selected Text.'#10 +
    #10 +
    #10 +
    #10 +
    'The content of the script is very simple, it is the duplication of two lines: one line is the search for content, one line is the replace content, the search for content must begin with "srch=", and the replace content must begin with "repl=", such two lines can be repeated multiple times, for example:'#10 +
    #10 +
    '----------'#10 +
    'srch=Dad'#10 +
    'repl=Son'#10 +
    #10 +
    'srch=Mom'#10 +
    'repl=Daughter'#10 +
    '----------'#10 +
    #10 +
    'This script will replace all "Dad" with "Son", and replace all "Mom" with "Daughter".'#10 +
    #10 +
    #10 +
    #10 +
    'You can use "case=True" to specify CaseSensitive or "case=False" to specify CaseInSensitive, for example:'#10 +
    #10 +
    '----------'#10 +
    'case=True'#10 +
    #10 +
    'srch=Dad'#10 +
    'repl=Son'#10 +
    #10 +
    'case=False'#10 +
    #10 +
    'srch=Mom'#10 +
    'repl=Daughter'#10 +
    '----------'#10 +
    #10 +
    'case=True is the default setting.'#10 +
    #10 +
    #10 +
    #10 +
    'In addition, you can use "mode=" to specify the search rule, there are 3 rules to chiose:'#10 +
    #10 +
    'mode=General'#10 +
    'mode=RegExpr'#10 +
    'mode=OneToOne'#10 +
    #10 +
    'the General mode is the default mode, which is an ordinary search and replace function.'#10 +
    #10 +
    'The RegExpr mode is used like this:'#10 +
    #10 +
    'The RegExpr mode is to use Regular Expression to search and replace, usage is as follows:'#10 +
    #10 +
    '----------'#10 +
    'mode=RegExpr'#10 +
    #10 +
    'srch=([a-zA-Z])([0-9])'#10 +
    'repl=$1 $2'#10 +
    #10 +
    'srch=([0-9])([a-zA-Z])'#10 +
    'repl=$1 $2'#10 +
    '----------'#10 +
    #10 +
    'This script will add a Space Character between all letters and numbers.'#10 +
    #10 +
    'The OneToOne mode is a one-to-one general search and replace, usage is as follows:'#10 +
    #10 +
    '----------'#10 +
    'mode=OneToOne'#10 +
    'case=False'#10 +
    #10 +
    'srch=Dad|Mom|Cat|Chicken'#10 +
    'repl=Son|Daughter|Dog|Duck'#10 +
    '----------'#10 +
    #10 +
    'This script will replace "Dad" with "Son", replace "Mom" with "Daughter", replace "Cat" with "Dog", replace "Chicken" with "Duck". This mode can''t use regular expression. If you want to search for "|", you can use \x7C to search it.'#10 +
    #10 +
    #10 +
    #10 +
    'In addition, you can use "loop=" to specify the number of loop for some replace operation, for example:'#10 +
    #10 +
    '----------'#10 +
    'mode=RegExpr'#10 +
    #10 +
    'loop=9999'#10 +
    'srch=“([^“”]*)“'#10 +
    'repl=“$1”'#10 +
    #10 +
    'srch=”([^“”]*)”'#10 +
    'repl=”$1“'#10 +
    'loop=End'#10 +
    '----------'#10 +
    #10 +
    'This script will execute replace operation between "loop=9999" and "loop=End" many times, if the search from content is:'#10 +
    #10 +
    '“AAA”,“BBB“,”CCC“,”DDD“'#10 +
    #10 +
    'Then the script will replace it to:'#10 +
    #10 +
    '“AAA”,“BBB”,“CCC”,“DDD”'#10 +
    #10 +
    'This script will not loop 9999 times, if no matches are found in a loop, the loop will be aborted. If there is no scripts after "loop=End", the "loop=End" can be omitted. you can also use "loop=" like this (there are 2 loops, one is from the first "loop=9999" to the second "loop=9999", another is from the second "loop=9999" to the end):'#10 +
    #10 +
    '----------'#10 +
    'mode=RegExpr'#10 +
    #10 +
    'loop=9999'#10 +
    'srch=“([^“”]*)“'#10 +
    'repl=“$1”'#10 +
    #10 +
    'srch=”([^“”]*)”'#10 +
    'repl=”$1“'#10 +
    #10 +
    'loop=9999'#10 +
    'srch=‘([^‘’]*)‘'#10 +
    'repl=‘$1’'#10 +
    #10 +
    'srch=’([^‘’]*)’'#10 +
    'repl=’$1‘'#10 +
    '----------'#10 +
    #10 +
    'Every time a "loop=n" (n>0) is encountered, the previous loop will start executing.'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Others --- ]'#10 +
    #10 +
    'You can set the font name and font size in the Options dialog box, if the font name set is empty, the system default font name is used, if the font size is set to 0, the system default font size is used.'#10 +
    #10 +
    'You can also use "Ctrl + MouseWheel" or "Ctrl + Shift + MouseWheel" to adjust the font size.'#10 +
    #10 +
    'You can switch between bright theme and dark theme, and you can also customize the foreground and background colors for each theme.'#10 +
    #10 +
    'When you perform a Collapse operation, if the current node has no children, its parent node is collapsed. If you hold down the Shift key, you can collapse to the Root Node.'#10 +
    #10 +
    'In the case of the TreeBar is closed, you can use "Ctrl + PageUp" and "Ctrl + PageDown" to switch nodes for browsing.'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Partial Shortcuts --- ]'#10 +
    #10 +
    'Search                        : Ctrl + F'#10 +
    'Prev Search Result            : F3'#10 +
    'Next Search Result            : F4'#10 +
    'Utils                         : Ctrl + G'#10 +
    #10 +
    'Recycle node                  : Ctrl + Shift + Delete'#10 +
    'Delete node                   : Alt + Shift + Delete'#10 +
    #10 +
    'Select the previous node      : Ctrl + PageUp'#10 +
    'Select the next node          : Ctrl + PageDown'#10 +
    #10 +
    'Undo                          : Ctrl + Z'#10 +
    'Redo                          : Ctrl + Shift + Z / Ctrl + Y'#10 +
    #10 +
    'Toggle MenuBar                : F5'#10 +
    'Toggle ToolBar                : F6'#10 +
    'Toggle StatBar                : F7'#10 +
    'Toggle TreeBar                : F9  /  Alt + 1'#10 +
    'Toggle InfoBar                : F10 /  Alt + 2'#10 +
    'Toggle RecyBar                : F8  /  Alt + 3'#10 +
    #10 +
    'Toggle FullScreen             : F11'#10 +
    'Toggle FullWindow             : F12'#10 +
    #10 +
    'Toggle Theme                  : Ctrl + Q / Alt + 4'#10 +
    #10 +
    'Help                          : F1 / Shift + F1'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Language Files --- ]'#10 +
    #10 +
    'You can put your own language file in the "languages" directory to localize the program. The language corresponding to each language file is as follows:'#10 +
    #10 +
    'TomiNote.af.po    = Afrikaans'#10 +
    'TomiNote.am.po    = Amharic'#10 +
    'TomiNote.ar.po    = Arabic'#10 +
    'TomiNote.ar_ae.po = Arabic(United Arab Emirates)'#10 +
    'TomiNote.ar_bh.po = Arabic(Bahrain)'#10 +
    'TomiNote.ar_dz.po = Arabic(Algeria)'#10 +
    'TomiNote.ar_eg.po = Arabic(Egypt)'#10 +
    'TomiNote.ar_iq.po = Arabic(Iraq)'#10 +
    'TomiNote.ar_jo.po = Arabic(Jordan)'#10 +
    'TomiNote.ar_kw.po = Arabic(Kuwait)'#10 +
    'TomiNote.ar_lb.po = Arabic(Lebanon)'#10 +
    'TomiNote.ar_ly.po = Arabic(Libya)'#10 +
    'TomiNote.ar_ma.po = Arabic(Morocco)'#10 +
    'TomiNote.ar_om.po = Arabic(Oman)'#10 +
    'TomiNote.ar_qa.po = Arabic(Qatar)'#10 +
    'TomiNote.ar_sa.po = Arabic(Saudi Arabia)'#10 +
    'TomiNote.ar_sy.po = Arabic(Syria)'#10 +
    'TomiNote.ar_tn.po = Arabic(Tunisia)'#10 +
    'TomiNote.ar_ye.po = Arabic(Yemen)'#10 +
    'TomiNote.as.po    = Assamese'#10 +
    'TomiNote.az.po    = Azeri'#10 +
    'TomiNote.az_az.po = Azeri(Cyrillic)'#10 +
    'TomiNote.be.po    = Belarusian'#10 +
    'TomiNote.bg.po    = Bulgarian'#10 +
    'TomiNote.bn.po    = Bengali'#10 +
    'TomiNote.bo.po    = Tibetan'#10 +
    'TomiNote.bs.po    = Bosnian'#10 +
    'TomiNote.ca.po    = Catalan'#10 +
    'TomiNote.cs.po    = Czech'#10 +
    'TomiNote.cy.po    = Welsh'#10 +
    'TomiNote.da.po    = Danish'#10 +
    'TomiNote.de.po    = German'#10 +
    'TomiNote.de_at.po = German(Austria)'#10 +
    'TomiNote.de_ch.po = German(Switzerland)'#10 +
    'TomiNote.de_de.po = German(Germany)'#10 +
    'TomiNote.de_li.po = German(Liechtenstein)'#10 +
    'TomiNote.de_lu.po = German(Luxembourg)'#10 +
    'TomiNote.dv.po    = Maldivian'#10 +
    'TomiNote.el.po    = Greek'#10 +
    'TomiNote.en.po    = English'#10 +
    'TomiNote.en_au.po = English(Australia)'#10 +
    'TomiNote.en_bz.po = English(Belize)'#10 +
    'TomiNote.en_ca.po = English(Canada)'#10 +
    'TomiNote.en_cb.po = English(Caribbean)'#10 +
    'TomiNote.en_gb.po = English(Great Britain)'#10 +
    'TomiNote.en_ie.po = English(Ireland)'#10 +
    'TomiNote.en_in.po = English(India)'#10 +
    'TomiNote.en_jm.po = English(Jamaica)'#10 +
    'TomiNote.en_nz.po = English(New Zealand)'#10 +
    'TomiNote.en_ph.po = English(Philippines)'#10 +
    'TomiNote.en_tt.po = English(Trinidad)'#10 +
    'TomiNote.en_us.po = English(United States)'#10 +
    'TomiNote.en_za.po = English(Southern Africa)'#10 +
    'TomiNote.es.po    = Spanish'#10 +
    'TomiNote.es_ar.po = Spanish(Argentina)'#10 +
    'TomiNote.es_bo.po = Spanish(Bolivia)'#10 +
    'TomiNote.es_cl.po = Spanish(Chile)'#10 +
    'TomiNote.es_co.po = Spanish(Colombia)'#10 +
    'TomiNote.es_cr.po = Spanish(Costa Rica)'#10 +
    'TomiNote.es_do.po = Spanish(Dominican Republic)'#10 +
    'TomiNote.es_ec.po = Spanish(Ecuador)'#10 +
    'TomiNote.es_es.po = Spanish(Traditional)'#10 +
    'TomiNote.es_gt.po = Spanish(Guatemala)'#10 +
    'TomiNote.es_hn.po = Spanish(Honduras)'#10 +
    'TomiNote.es_mx.po = Spanish(Mexico)'#10 +
    'TomiNote.es_ni.po = Spanish(Nicaragua)'#10 +
    'TomiNote.es_pa.po = Spanish(Panama)'#10 +
    'TomiNote.es_pe.po = Spanish(Peru)'#10 +
    'TomiNote.es_pr.po = Spanish(Puerto Rico)'#10 +
    'TomiNote.es_py.po = Spanish(Paraguay)'#10 +
    'TomiNote.es_sv.po = Spanish(ElSalvador)'#10 +
    'TomiNote.es_uy.po = Spanish(Uruguay)'#10 +
    'TomiNote.es_ve.po = Spanish(Venezuela)'#10 +
    'TomiNote.et.po    = Estonian'#10 +
    'TomiNote.eu.po    = Basque'#10 +
    'TomiNote.fa.po    = Farsi'#10 +
    'TomiNote.fi.po    = Finnish'#10 +
    'TomiNote.fo.po    = Faroese'#10 +
    'TomiNote.fr.po    = French'#10 +
    'TomiNote.fr_be.po = French(Belgium)'#10 +
    'TomiNote.fr_ca.po = French(Canada)'#10 +
    'TomiNote.fr_ch.po = French(Switzerland)'#10 +
    'TomiNote.fr_fr.po = French(France)'#10 +
    'TomiNote.fr_lu.po = French(Luxembourg)'#10 +
    'TomiNote.ga.po    = Irish'#10 +
    'TomiNote.gd.po    = Gaelic(Scotland)'#10 +
    'TomiNote.gd_ie.po = Gaelic(Ireland)'#10 +
    'TomiNote.gl.po    = Galician'#10 +
    'TomiNote.gn.po    = Guarani(Paraguay)'#10 +
    'TomiNote.gu.po    = Gujarati'#10 +
    'TomiNote.he.po    = Hebrew'#10 +
    'TomiNote.hi.po    = Hindi'#10 +
    'TomiNote.hr.po    = Croatian'#10 +
    'TomiNote.hu.po    = Hungarian'#10 +
    'TomiNote.hy.po    = Armenian'#10 +
    'TomiNote.id.po    = Indonesian'#10 +
    'TomiNote.is.po    = Icelandic'#10 +
    'TomiNote.it.po    = Italian'#10 +
    'TomiNote.it_ch.po = Italian(Switzerland)'#10 +
    'TomiNote.it_it.po = Italian(Italy)'#10 +
    'TomiNote.ja.po    = Japanese'#10 +
    'TomiNote.ka.po    = Georgian'#10 +
    'TomiNote.kk.po    = Kazakh'#10 +
    'TomiNote.km.po    = Khmer'#10 +
    'TomiNote.kn.po    = Kannada'#10 +
    'TomiNote.ko.po    = Korean'#10 +
    'TomiNote.ks.po    = Kashmiri'#10 +
    'TomiNote.la.po    = Latin'#10 +
    'TomiNote.lo.po    = Lao'#10 +
    'TomiNote.lt.po    = Lithuanian'#10 +
    'TomiNote.lv.po    = Latvian'#10 +
    'TomiNote.mi.po    = Maori'#10 +
    'TomiNote.mk.po    = FYRO Macedonia'#10 +
    'TomiNote.ml.po    = Malayalam'#10 +
    'TomiNote.mn.po    = Mongolian'#10 +
    'TomiNote.mr.po    = Marathi'#10 +
    'TomiNote.ms.po    = Malay'#10 +
    'TomiNote.ms_bn.po = Malay(Brunei)'#10 +
    'TomiNote.ms_my.po = Malay(Malaysia)'#10 +
    'TomiNote.mt.po    = Maltese'#10 +
    'TomiNote.my.po    = Burmese'#10 +
    'TomiNote.nb.po    = Norwegian(Bokml)'#10 +
    'TomiNote.ne.po    = Nepali'#10 +
    'TomiNote.nl.po    = Dutch'#10 +
    'TomiNote.nl_be.po = Dutch(Belgium)'#10 +
    'TomiNote.nl_nl.po = Dutch(Netherlands)'#10 +
    'TomiNote.no.po    = Norwegian'#10 +
    'TomiNote.or.po    = Oriya'#10 +
    'TomiNote.pa.po    = Punjabi'#10 +
    'TomiNote.pl.po    = Polish'#10 +
    'TomiNote.pt.po    = Portuguese'#10 +
    'TomiNote.pt_br.po = Portuguese(Brazil)'#10 +
    'TomiNote.pt_pt.po = Portuguese(Portugal)'#10 +
    'TomiNote.rm.po    = Raeto(Romance)'#10 +
    'TomiNote.ro.po    = Romanian'#10 +
    'TomiNote.ro_mo.po = Romanian(Moldova)'#10 +
    'TomiNote.ru.po    = Russian'#10 +
    'TomiNote.ru_mo.po = Russian(Moldova)'#10 +
    'TomiNote.sa.po    = Sanskrit'#10 +
    'TomiNote.sb.po    = Sorbian'#10 +
    'TomiNote.sd.po    = Sindhi'#10 +
    'TomiNote.si.po    = Sinhalese'#10 +
    'TomiNote.sk.po    = Slovak'#10 +
    'TomiNote.sl.po    = Slovenian'#10 +
    'TomiNote.so.po    = Somali'#10 +
    'TomiNote.sq.po    = Albanian'#10 +
    'TomiNote.sr.po    = Serbian(Latin)'#10 +
    'TomiNote.sr_sp.po = Serbian(Cyrillic)'#10 +
    'TomiNote.sv.po    = Swedish'#10 +
    'TomiNote.sv_fi.po = Swedish(Finland)'#10 +
    'TomiNote.sv_se.po = Swedish(Sweden)'#10 +
    'TomiNote.sw.po    = Swahili'#10 +
    'TomiNote.sz.po    = Sami(lappish)'#10 +
    'TomiNote.ta.po    = Tamil'#10 +
    'TomiNote.te.po    = Telugu'#10 +
    'TomiNote.tg.po    = Tajik'#10 +
    'TomiNote.th.po    = Thai'#10 +
    'TomiNote.tk.po    = Turkmen'#10 +
    'TomiNote.tn.po    = Setswana'#10 +
    'TomiNote.tr.po    = Turkish'#10 +
    'TomiNote.ts.po    = Tsonga'#10 +
    'TomiNote.tt.po    = Tatar'#10 +
    'TomiNote.uk.po    = Ukrainian'#10 +
    'TomiNote.ur.po    = Urdu'#10 +
    'TomiNote.uz.po    = Uzbek(Latin)'#10 +
    'TomiNote.uz_uz.po = Uzbek(Cyrillic)'#10 +
    'TomiNote.ve.po    = Venda'#10 +
    'TomiNote.vi.po    = Vietnamese'#10 +
    'TomiNote.xh.po    = Xhosa'#10 +
    'TomiNote.yi.po    = Yiddish'#10 +
    'TomiNote.zh.po    = Chinese'#10 +
    'TomiNote.zh_cn.po = Chinese(China)'#10 +
    'TomiNote.zh_hk.po = Chinese(Hong Kong SAR)'#10 +
    'TomiNote.zh_mo.po = Chinese(Macau SAR)'#10 +
    'TomiNote.zh_sg.po = Chinese(Singapore)'#10 +
    'TomiNote.zh_tw.po = Chinese(Taiwan)'#10 +
    'TomiNote.zu.po    = Zulu'#10 +
    #10 +
    #10 +
    #10 +
    '[ --- Regular Expression Syntax --- ]'#10 +
    #10 +
    '(Need the translator to organize their own content)'#10 +
    #10 +
    #10 +
    #10;

implementation

initialization

  AppFullName              := ParamStr(0);
  AppName                  := ExtractFileName(AppFullName);
  AppDir                   := ExtractFilePath(AppFullName);

end.

