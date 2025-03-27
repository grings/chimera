object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JSON Viewer'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 622
    object btnOpen: TButton
      Left = 0
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object chkSorted: TCheckBox
      Left = 81
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Sorted'
      TabOrder = 1
      OnClick = chkSortedClick
    end
  end
  object tsFiles: TTabSet
    Left = 0
    Top = 420
    Width = 624
    Height = 21
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    ShrinkToFit = True
    SelectedColor = 12189136
    Style = tsIDETabs
    OnChange = tsFilesChange
    ExplicitTop = 412
    ExplicitWidth = 622
  end
  object Viewer: TJSONViewer
    Left = 0
    Top = 41
    Width = 624
    Height = 379
    FontName = 'Consolas'
    FontSize = 10
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Consolas'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    ExplicitLeft = 136
    ExplicitTop = 112
    ExplicitWidth = 320
    ExplicitHeight = 240
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '.json'
    Filter = 'JSON Files (*.json)|*.json|All Files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a JSON File'
    Left = 304
    Top = 224
  end
end
